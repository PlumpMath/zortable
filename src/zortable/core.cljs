(ns zortable.core
  (:import [goog.ui IdGenerator])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as async :refer [>! <! chan]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.style :as style]
            [goog.dom.classes :as classes]
            [goog.events :as events]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as zimpl]
            [jamesmacaulay.zelkova.mouse :as mouse]))
;; ======================================================================
;; Util

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

;; ====================================================================== 
;; Custom Stream

(defn- listen [el type & xforms]
  (let [out (apply async/chan 1 xforms)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

(defn- mouse-target-ch [graph opts]
  (listen js/document "mousemove" (map (fn [e] (.. e -target)))))

(def mouse-target 
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
  (let [final-pos (style/getPageOffset (:node box))
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
        (println "offset"  (box-offset pos box))
        (println "startbox" box)
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
    (println (drag-to-pos (:box state)))
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

;; ====================================================================== 
;; Zelkova Piping

(defn state-signal [stop-ch zid drag-class init-state]
  (let [dragging-positions (z/keep-when mouse/down? [0 0] mouse/position)
        dragging? (->> (z/constant true)
                    (z/sample-on dragging-positions)
                    (z/merge (z/keep-if not false mouse/down?))
                    (z/drop-repeats))
        dragstarts (z/keep-if identity true dragging?)
        dragstops (z/keep-if not false dragging?)
        actions (z/merge (z/constant (->NoOp))
                  (z/map (partial ->StartDrag zid drag-class)
                    (z/sample-on dragstarts mouse/position)
                    (z/sample-on dragstarts mouse-target))
                  (z/map (constantly (->StopDrag zid stop-ch)) dragstops)
                  (z/map (partial ->Drag zid) dragging-positions))]
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
        (om/build (:box-view opts) item {:react-key (:id box)})))))

(defn sort-filler [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortFiller")
    om/IRender
    (render [_]
      (dom/div #js {:className "sortable-filler"}
        (om/build (:box-filler opts) item)))))

(defn sort-wrapper [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortWrapper")
    om/IRenderState
    (render-state [_ {:keys [eid id]}]
      (dom/div #js {:id eid 
                    :className "sortable-container"}
        (om/build (:box-view opts) item {:react-key id})))))

(defn zortable [{:keys [sort items]} owner opts]
  (reify
    om/IDisplayName (display-name [_] "Zortable")
    om/IInitState
    (init-state [_]
      ;; State present during the whole lifecycle
      {:zid (guid) ;; Unique to each loaded zortable
       :stop-ch (chan)
       :ids @sort
       :id->eid (zipmap @sort (mapv (fn [_] (guid)) @sort))
       ;; State present during drag
       :start-pos []
       :box {}})
    om/IWillMount
    (will-mount [_]
      (letfn [(reset-drag-state! []
                (om/set-state! owner :box {})
                (om/set-state! owner :start-pos []))]
        (go-loop []
          (let [[tag state] (<! (om/get-state owner :stop-ch))]
            (when (= tag ::stop)
              (om/update! sort (:ids state))
              (reset-drag-state!)
              (recur))))))
    om/IDidMount
    (did-mount [_]
      (let [signal (state-signal (om/get-state owner :stop-ch)
                     (om/get-state owner :zid)
                     (:drag-class opts)
                     (dissoc (om/get-state owner) :stop-ch :zid))
            [state-ref live-graph] (pipe-to-atom signal)]
        (add-watch state-ref ::sortable
          (fn [_ _ _ nv]
            (om/update-state! owner #(merge % nv))))
        (om/set-state! owner :live-graph live-graph)
        (om/set-state! owner :state-ref state-ref)))
    om/IWillUnmount
    (will-unmount [_]
      (async/close! (om/get-state owner :live-graph))
      (async/close! (om/get-state owner :stop-ch))
      (remove-watch (om/get-state owner :state-ref) ::sortable))
    om/IRenderState
    (render-state [_ {:keys [ids id->eid zid box]}]
      (let [moving-id (:id box)]
        (apply dom/div #js {:id zid :className "zort-list"} 
          (when-not (empty? box)
            (om/build sort-draggable
              {:box box 
               :item (items moving-id)}
              {:opts opts :react-key moving-id}))
          (map (fn [item-id]
                 (let [eid (id->eid item-id)
                       item (items item-id)]
                   (if (= item-id moving-id) 
                     (om/build sort-filler item {:opts opts})
                     (om/build sort-wrapper item
                       {:opts opts :init-state {:eid eid :id item-id}
                        :react-key item-id}))))
            ids))))))
