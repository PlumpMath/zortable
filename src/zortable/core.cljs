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

(defn ensure-attrs [k f]
  (fn [box]
    (if (nil? (k box))
      (f box)
      box)))

(defn add-node [box]
  {:pre [(some? (::id box))]}
  (assoc box :node (.getElementById js/document (::id box))))

(def ensure-node (ensure-attrs :node add-node))

(defn add-size [box]
  (let [box' (ensure-node box)
        n (aget (.-childNodes (:node box')) 0) 
        size (style/getSize n)]
    (assoc box' :width (.-width size) :height (.-height size))))

(def ensure-size (ensure-attrs :width add-size))

(defn add-pos [box]
  (let [box' (ensure-node box)
        final-pos (style/getPageOffset (:node box'))
        left (.-x final-pos)
        top (.-y final-pos)]
    (assoc box' :left left :top top)))

(def ensure-pos (ensure-attrs :left add-pos))

(defn box-center [box]
  (let [box' (ensure-pos (ensure-size box))]
    (letfn [(add [kb ks]
              (+ (kb box') (/ (ks box') 2)))]
      [(add :left :width) (add :top :height)])))

(defn filler-box? [box]
  (= "filler-box" (::id box)))

(defn filler-box []
  {::id "filler-box"})

(defn in-box? [[x y] box]
  (let [{:keys [top left width height]} (ensure-pos (ensure-size box))]
    (and (< left x (+ left width))
      (< top y (+ top height)))))

(defn moving? [box]
  (contains? box :drag-offset))

(defn topleft-pos [{:keys [left top]}]
  [left top])

(defn start-dragging-box-from-pos [pos box]
  (let [offset (->> box topleft-pos (map - pos))]
    (assoc box :drag-offset offset)))

(defn find-drag-id [drag-class node]
  (some-> (if (classes/has node drag-class)
            node 
            (.closest node (str "." drag-class)))
    (.closest ".sortable-container")
    .-id))

(defn start-drag
  "Identifies the boxes to be dragged (or build) and returns the updated state"
  [drag-class pos et state]
  (let [drag-target?
        (if (nil? drag-class)
          (partial in-box? pos)
          (if-let [id (find-drag-id drag-class et)]
            #(= (::id %) id)
            (fn [_] false)))]
    (-> state
      (update-in [:boxes]
        (fn [boxes]
          (->> (map (comp add-size add-pos add-node) boxes)
            (mapv #(if (drag-target? %)
                     (start-dragging-box-from-pos pos %)
                     %)))))
      (assoc :drag {:start-pos pos}))))

(defn sort-by-pos [boxes]
  (vec (sort-by (comp second box-center add-size add-pos add-node) boxes)))

(defn drag
  "Updates the state by interpreting what the new position means for each box."
  [pos state]
  (letfn [(drag-to-pos [box]
            (let [[left top] (map - pos (:drag-offset box))]
              (assoc box :left left :top top)))]
    (update-in state [:boxes]
      (fn [boxes]
        (->> boxes 
          (map (comp add-pos add-node))
          (map #(if (moving? %) (drag-to-pos %) %))
          sort-by-pos)))))

(defn drop-boxes [state]
  (letfn [(drop-box [box]
            (dissoc box :drag-offset))]
    (update-in state [:boxes] (partial mapv drop-box))))

(defn stop-drag [state]
  (-> state
    drop-boxes
    (assoc :drag nil)))

(defrecord NoOp [] IFn (-invoke [_ state] state))
(defrecord StartDrag [drag-class pos et]
  IFn (-invoke [_ state]
        (start-drag drag-class pos et state)))
(defrecord Drag [pos] IFn (-invoke [_ state] (drag pos state)))
(defrecord StopDrag [ch]
  IFn (-invoke [_ state]
        (let [state' (stop-drag state)]
          (go (>! ch [::stop state']))
          state')))

(defn state-signal [stop-ch drag-class init-state]
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
                  (z/map (partial ->StartDrag drag-class)
                    (z/sample-on dragstarts mouse/position)
                    (z/sample-on dragstarts mouse-target))
                  (z/map (constantly (->StopDrag stop-ch)) dragstops)
                  (z/map ->Drag dragging-positions))]
    (z/foldp (fn [action state] (action state))
      init-state
      actions)))

(defn sort-draggable [[box item] owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortDraggable")
    om/IRender
    (render [_]
      (dom/div #js {:id (::id box)
                    :className "sortable-draggable"
                    :style #js {:position "absolute"
                                :zIndex 1
                                :top (:top box)
                                :left (:left box)}}
        (om/build (:box-view opts) item)))))

(defn sort-filler [[box item] owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortFiller")
    om/IRender
    (render [_]
      (dom/div #js {:id "filler-box"
                    :className "sortable-filler"
                    :style #js {:position "relative"}}
        (om/build (:box-filler opts) item)))))

(defn sort-wrapper [[box item] owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortWrapper")
    om/IRender
    (render [_]
      (dom/div #js {:id (::id box)
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

(defn zortable [items owner opts]
  (reify
    om/IDisplayName (display-name [_] "Zortable")
    om/IInitState
    (init-state [_]
      (let [boxes (mapv #(hash-map ::id (str (gensym))
                           ::key (get % (:id-key opts)))
                    items)]
        {:boxes boxes 
         :stop-ch (chan)
         :drag nil}))
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (when (om/mounted? owner)
          (let [[tag state] (<! (om/get-state owner :stop-ch))]
            (case tag
              ::stop (om/transact! items
                       #(mapv (fn [box]
                                (find-by-key (:id-key opts) (::key box) %))
                          (:boxes state))))))
        (recur)))
    om/IDidMount
    (did-mount [_]
      (let [boxes (mapv (comp add-pos add-node) (om/get-state owner :boxes))
            signal (state-signal (om/get-state owner :stop-ch)
                     (:drag-class opts) {:boxes boxes})
            [state-ref live-graph] (pipe-to-atom signal)]
        (add-watch state-ref ::sortable
          (fn [_ _ _ nv]
            (om/update-state! owner #(merge % nv))))
        (om/set-state! owner :live-graph live-graph)
        (om/set-state! owner :state-ref state-ref)
        (om/set-state! owner :boxes boxes)))
    om/IWillUnmount
    (will-unmount [_]
      (async/close! (om/get-state owner :live-graph))
      ;; Just in case.
      (remove-watch (om/get-state owner :state-ref) ::sortable))
    om/IRenderState
    (render-state [_ {:keys [boxes]}]
      (apply dom/div {:className "zort-list"} 
        (if-let [draggable (first (filter moving? boxes))]
          (om/build sort-draggable
            [draggable (find-by-key (:id-key opts) (::key draggable) items)]
            {:opts opts}))
        (->> (map ensure-node boxes)
          (map-indexed vector)
          (sort-by (fn [[index box]]
                     (if (nil? (:node box))
                       index
                       (second (box-center box)))))
          (map (fn [[_ box]]
                 (let [item (find-by-key (:id-key opts) (::key box) items)]
                   (if (moving? box)
                     (om/build sort-filler [box item] {:opts opts})
                     (om/build sort-wrapper [box item] {:opts opts}))))))))))
