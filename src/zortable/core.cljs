(ns zortable.core
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

(defn- listen [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

(defn- target-channel [graph opts]
  (listen js/document "mousemove"
    (map (fn [e] (.. e -target)))))

(def mouse-target 
  (z/input #js {} ::target target-channel))

(defn add-node [box]
  {:pre [(some? (::id box))]}
  (assoc box :node (.getElementById js/document (::id box))))

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
  (->> box topleft-pos (map - pos)))

(defn find-drag-id [drag-class node]
  (some-> (if (classes/has node drag-class)
            node 
            (.closest node (str "." drag-class)))
    (.closest ".sortable-container")
    .-id))

(defn id->box [id]
  (add-size (add-pos (add-node {::id id}))))

(defn find-id [ids ele-id]
  (some (fn [[id eid]] (if (= ele-id eid) id)) ids))

(defn start-drag
  "Identifies the boxes to be dragged (or build) and returns the updated state"
  [drag-class pos et state]
  (if-let [moving-id (find-drag-id drag-class et)] 
    (let [box (id->box moving-id)]
      (assoc state
        :moving-id (find-id (:ids state) moving-id)
        :start-pos pos
        :box (assoc box :offset (box-offset pos box))))
    state))

(defn sort-by-pos [ids]
  (vec (sort-by (comp second box-center id->box) ids)))

(defn drag
  "Updates the state by interpreting what the new position means for each box."
  [pos state]
  (letfn [(drag-to-pos [box]
            (let [[left top] (map - pos (:offset box))]
              (assoc box :left left :top top)))]
    (println (drag-to-pos (:box state)))
    (-> state
      (update :box drag-to-pos)
      #_(update :ids sort-by-pos))))

(defn stop-drag [state]
  (-> state
    (dissoc :box :moving-id :start-pos)
    #_(update :ids sort-by-pos)))

;; We check dragging? before updating the state in case the signal
;; came from some other sortable component.

(defonce dragging-component (atom nil))

(defrecord NoOp [] IFn (-invoke [_ state] state))

(defrecord StartDrag [zid drag-class pos et]
  IFn (-invoke [_ state]
        (let [state' (start-drag drag-class pos et state)]
          (if (and (some? (:moving-id state)) (nil? @dragging-component))
            ;; Should check if there is one before
            (do (reset! dragging-component zid)
                state')
            state'))))

(defrecord Drag [zid pos]
  IFn (-invoke [_ state]
        (if (= zid @dragging-component) 
          (drag pos state)
          state)))

(defrecord StopDrag [zid ch]
  IFn (-invoke [_ state]
        (if (= zid @dragging-component) 
          (let [state' (stop-drag state)]
            (reset! dragging-component nil)
            (go (>! ch [::stop state']))
            state')
          state)))

(defn state-signal [stop-ch zid drag-class init-state]
  (let [dragging-positions (z/keep-when mouse/down?
                             [0 0]
                             mouse/position)
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

(defn sort-draggable [{:keys [box item]} owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortDraggable")
    om/IRender
    (render [_]
      (dom/div #js {:id (:ele-id box) 
                    :className "sortable-draggable"
                    :style #js {:position "absolute"
                                :zIndex 1
                                :top (:top box)
                                :left (:left box)}}
        (om/build (:box-view opts) item {:react-key (::id box)})))))

(defn sort-filler [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortFiller")
    om/IRender
    (render [_]
      (dom/div #js {:className "sortable-filler"
                    :style #js {:position "relative"}}
        (om/build (:box-filler opts) item)))))

(defn sort-wrapper [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortWrapper")
    om/IRenderState
    (render-state [_ {:keys [ele-id]}]
      (dom/div #js {:id ele-id 
                    :className "sortable-container"
                    :style #js {:position "relative"}}
        (om/build (:box-view opts) item)))))

(defn find-by-key [k v coll]
  (first (filter #(= v (get % k)) coll)))

(defn pipe-to-atom
  "Adds live-graph to the return value of the original z/pipe-to-atom"
  [x]
  (let [live-graph (z/spawn x)]
    [(z/pipe-to-atom live-graph
       (atom (zimpl/init live-graph)
         :meta {::source live-graph}))
     live-graph]))

(defn zortable [{:keys [sort items]} owner opts]
  (println sort)
  (println items)
  (reify
    om/IDisplayName (display-name [_] "Zortable")
    om/IInitState
    (init-state [_]
      {:zid (str (gensym)) ;; Unique to each loaded zortable
       :stop-ch (chan)
       :ids (zipmap @sort (mapv (fn [_] (str (gensym))) @sort))
       :moving-id nil
       :start-pos []
       :box {}})
    om/IWillMount
    (will-mount [_]
      (letfn [(reset-state! []
                (om/set-state! owner :moving-id nil)
                (om/set-state! owner :box {})
                (om/set-state! owner :start-pos []))]
        (go-loop []
          (let [[tag state] (<! (om/get-state owner :stop-ch))]
            (when (= tag ::stop)
              (reset-state!)
              (println "sTop")
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
      ;; Just in case.
      (remove-watch (om/get-state owner :state-ref) ::sortable))
    om/IRenderState
    (render-state [_ {:keys [ids zid moving-id box]}]
      (apply dom/div {:id zid :className "zort-list"} 
        (when (some? moving-id)
          (println moving-id)
          (println (find-by-key (:id-key opts) moving-id items))
          (om/build sort-draggable
            {:box (assoc box :ele-id (get ids moving-id)) 
             :item (find-by-key (:id-key opts) moving-id items)}
            {:opts opts :react-key moving-id}))
        (map (fn [[item-id ele-id]]
               (let [item (find-by-key (:id-key opts) item-id items)]
                 (if (= item-id moving-id) 
                   (om/build sort-filler item {:opts opts})
                   (om/build sort-wrapper item
                     {:opts opts :init-state {:ele-id ele-id}
                      :react-key item-id}))))
          ids)))))
