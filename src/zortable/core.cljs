(ns zortable.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [clojure.set :as set]
            [cljs.core.async :as async :refer [>! <! chan]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.style :as style]
            [goog.dom.classes :as classes]
            [goog.events :as events]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as zimpl]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [zortable.util :as u]))

;; ====================================================================== 
;; Custom Mouse Target Stream

;; TODO: PR to Zelkova

(defn- listen [el type & xforms]
  (let [out (apply async/chan 1 xforms)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

(defn- mouse-target-ch [graph opts]
  (listen js/document "mousemove" (map (fn [e] (.. e -target)))))

(defonce mouse-target 
  (z/input #js {} ::mouse-target mouse-target-ch))

;; ======================================================================  
;; Box

(defn add-node [box]
  {:pre [(some? (:eid box))]}
  (assoc box :node (.getElementById js/document (:eid box))))

(defn add-size [box]
  (let [n (aget (.-childNodes (:node box)) 0) 
        size (style/getSize n)]
    (assoc box :width (.-width size) :height (.-height size))))

(defn add-pos [box]
  (let [final-pos (style/getPosition (:node box))
        left (.-x final-pos)
        top (.-y final-pos)]
    (assoc box :left left :top top)))

(defn box-center [box]
  (letfn [(add [kb ks]
            (+ (kb box) (/ (ks box) 2)))]
    [(add :left :width) (add :top :height)]))

(defn topleft-pos [{:keys [left top]}]
  [left top])

(defn box-offset [pos box]
  (->> box topleft-pos (mapv - pos)))

(defn find-drag-id [drag-class node]
  (some-> (if (classes/has node drag-class)
            node 
            (.closest node (str "." drag-class)))
    (.closest ".sortable-container")
    .-id))

(defn eid->box [id]
  (add-size (add-pos (add-node {:eid id}))))

(defn eid->id [id->eid ele-id]
  (some (fn [[id eid]] (if (= ele-id eid) id)) id->eid))

(defn sort-by-pos [id->eid ids]
  (vec (sort-by (comp second box-center eid->box id->eid) ids)))

;; ====================================================================== 
;; Drag Events

(defn dragging? [state]
  (not (empty? (:box state))))

(defn start-drag
  "Identifies the boxes to be dragged (or build) and returns the updated state"
  [drag-class pos et state]
  (let [eid (find-drag-id drag-class et)]
    (if (or (nil? eid) (not (contains? (set (vals (:id->eid state))) eid)))
      state
      (let [box (eid->box eid)]
        (assoc state
          :start-pos pos
          :box (assoc box
                 :eid eid
                 :id (eid->id (:id->eid state) eid) 
                 :offset (box-offset pos box)))))))

(defn drag
  "Updates the state by interpreting what the new position means for each box."
  [pos state]
  (letfn [(drag-to-pos [box]
            (let [[left top] (map - pos (:offset box))]
              (assoc box :left left :top top)))]
    (-> state
      (update :box drag-to-pos)
      (update :ids (partial sort-by-pos (:id->eid state))))))

(defn stop-drag [state]
  (-> state
    (update :ids (partial sort-by-pos (:id->eid state)))
    (assoc :box {})
    (assoc :start-pos [])))

;; ====================================================================== 
;; Unique dragger

(defonce current-dragger (atom nil))

;; ====================================================================== 
;; Side Effects from Event Handlers

(defrecord NoOp [] IFn (-invoke [_ state] state))

(defrecord StartDrag [zid drag-class pos et]
  IFn (-invoke [_ state]
        (let [state' (start-drag drag-class pos et state)]
          (if (and (dragging? state') (nil? @current-dragger))
            (do (reset! current-dragger zid)
                state')
            state'))))

(defrecord Drag [zid pos]
  IFn (-invoke [_ state]
        (if (= zid @current-dragger) 
          (drag pos state)
          state)))

(defrecord StopDrag [zid ch]
  IFn (-invoke [_ state]
        (if (= zid @current-dragger) 
          (let [state' (stop-drag state)]
            (reset! current-dragger nil)
            (go (>! ch [::stop state']))
            state')
          state)))

(defrecord ResetState [new-state]
  IFn (-invoke [_ state]
        (merge state new-state)))

;; ====================================================================== 
;; Zelkova Piping

(defn reset-signal [init-state ch] 
  (z/input init-state ::reset-signal ch))

(defn state-signal [stop-ch reset-ch zid drag-class init-state]
  (let [dragging-positions (z/keep-when mouse/down? [0 0] mouse/position)
        dragging? (->> (z/constant true)
                    (z/sample-on dragging-positions)
                    (z/merge (z/keep-if not false mouse/down?))
                    (z/drop-repeats))
        dragstarts (z/keep-if identity true dragging?)
        dragstops (z/keep-if not false dragging?)
        reset-states (->> (reset-signal init-state reset-ch)
                       (z/keep-if (comp (partial = ::reset) first))
                       (z/map second))
        actions (z/merge (z/constant (->NoOp))
                  (z/map (partial ->StartDrag zid drag-class)
                    (z/sample-on dragstarts mouse/position)
                    (z/sample-on dragstarts mouse-target))
                  (z/map (constantly (->StopDrag zid stop-ch)) dragstops)
                  (z/map (partial ->Drag zid) dragging-positions)
                  (z/map ->ResetState reset-states))]
    (z/foldp (fn [action state] (action state))
      init-state
      actions)))

(defn pipe-to-atom
  "Adds live-graph to the return value of the original z/pipe-to-atom"
  [x]
  (let [live-graph (z/spawn x)]
    [(z/pipe-to-atom live-graph
       (atom (zimpl/init live-graph)
         :meta {::source live-graph}))
     live-graph]))

;; ====================================================================== 
;; Wrapper Components

(def valid-opts #{:box-view :box-filler})

(defn clean-opts [opts]
  (apply dissoc opts valid-opts))

(defn sort-draggable [{:keys [box item]} owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortDraggable")
    om/IRender
    (render [_]
      (dom/div #js {:id (:eid box)
                    :className "sortable-draggable"
                    :style #js {:position "absolute"
                                :zIndex 1
                                :top (:top box)
                                :left (:left box)}}
        (om/build (:box-view opts) item
          {:react-key (:id box)
           :opts (:opts opts)})))))

(defn sort-filler [{:keys [item box]} owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortFiller")
    om/IRender
    (render [_]
      (dom/div #js {:className "sortable-filler"}
        (om/build (:box-filler opts) item
          {:init-state (select-keys box [:width :height])
           :opts (:opts opts)})))))

(defn sort-wrapper [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortWrapper")
    om/IRenderState
    (render-state [_ {:keys [eid id]}]
      (dom/div #js {:id eid 
                    :className "sortable-container"}
        (om/build (:box-view opts) item
          {:opts (:opts opts) :react-key id})))))

(defn new-ids [sort]
  (zipmap sort (mapv (fn [_] (u/guid)) sort)))

(defn zortable [{:keys [sort items]} owner opts]
  (letfn [(get-local [kw]
            (om/get-state owner kw))
          (set-local! [kw v]
            (om/set-state! owner kw v))]
    (reify
      om/IDisplayName (display-name [_] "Zortable")
      om/IInitState
      (init-state [_]
        ;; State present during the whole lifecycle
        {:zid (u/guid) ;; Unique to each loaded zortable
         :reset-ch (chan)
         :stop-ch (chan)
         :ids (om/value sort)
         :id->eid (new-ids @sort) 
         ;; State present during drag
         :start-pos []
         :box {}})
      om/IWillMount
      (will-mount [_]
        (letfn [(reset-drag-state! []
                  (set-local! :box {})
                  (set-local! :start-pos []))]
          (go-loop []
            (let [[tag state] (<! (get-local :stop-ch))]
              (when (= tag ::stop)
                (om/update! sort (:ids state))
                (reset-drag-state!)
                (recur))))))
      om/IDidMount
      (did-mount [_]
        (let [signal (state-signal
                       (get-local :stop-ch)
                       (get-local :reset-ch)
                       (get-local :zid)
                       (:drag-class opts)
                       (dissoc (om/get-state owner) :stop-ch :reset-ch :zid))
              [state-ref live-graph] (pipe-to-atom signal)]
          (add-watch state-ref ::sortable
            (fn [_ _ _ nv]
              (om/update-state! owner #(merge % nv))))
          (set-local! :live-graph live-graph)
          (set-local! :state-ref state-ref)))
      om/IWillUnmount
      (will-unmount [_]
        (async/close! (get-local :live-graph))
        (async/close! (get-local :stop-ch))
        (remove-watch (get-local :state-ref) ::sortable))
      om/IWillReceiveProps
      (will-receive-props [_ {:keys [items sort]}]
        (assert (= (count items) (count sort))
          "Length of sort and items don't match")
        (when-not (= (count sort) (count (om/get-state owner :ids)))
          (let [old-ids (set (om/get-state owner :ids))
                future-ids (set @sort)
                to-create-ids (set/difference future-ids old-ids)
                to-delete-ids (set/difference old-ids future-ids)
                eids (->> (apply dissoc (get-local :id->eid) to-delete-ids)
                       (merge (new-ids to-create-ids)))
                new-state {:ids @sort 
                           :id->eid eids}]
            (async/put! (get-local :reset-ch) [::reset new-state]))))
      om/IRenderState
      (render-state [_ {:keys [ids id->eid zid box]}]
        (let [moving-id (:id box)]
          (apply dom/div #js {:id zid :className "zort-list"
                              :style #js {:WebkitTouchCallout "none"
                                          :WebkitUserSelect "none"
                                          :KhtmlUserSelect "none"
                                          :MozUserSelect "none"
                                          :msUserSelect "none"
                                          :userSelect "none"}} 
            (when-not (empty? box)
              (om/build sort-draggable
                {:box box 
                 :item (items moving-id)}
                {:opts opts :react-key moving-id}))
            (map (fn [item-id]
                   (let [eid (id->eid item-id)
                         item (items item-id)]
                     (if (= item-id moving-id) 
                       (om/build sort-filler {:item item :box box} {:opts opts})
                       (om/build sort-wrapper item
                         {:opts opts :init-state {:eid eid :id item-id}
                          :react-key item-id}))))
              ids)))))))
