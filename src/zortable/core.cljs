(ns zortable.core
  (:import [goog.events EventType])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async.impl.protocols :as async-impl]
            [clojure.set :as set]
            [cljs.core.async :as async :refer [>! <! chan put!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.style :as style]
            [goog.dom.classes :as classes]
            [goog.events :as events]
            [zortable.util :as u]))

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

(defn element-inside? [class element]
  (some? (.closest element (str "." class))))

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
  "Finds the box to be dragged and returns the updated state"
  [eid pos state]
  (let [box (eid->box eid)]
    (assoc state
      :start-pos pos
      :box (assoc box
             :eid eid
             :id (eid->id (:id->eid state) eid) 
             :offset (box-offset pos box)))))

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

(defn event->pos [e]
  [(.-clientX e) (.-clientY e)])

(defn sort-wrapper [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortWrapper")
    om/IRenderState
    (render-state [_ {:keys [eid id]}]
      (dom/div #js {:id eid 
                    :onMouseDown
                    (fn [e]
                      (when (element-inside? (:drag-class opts) (.-target e))
                        (put! (:ch opts) [:start-drag {:eid eid
                                                       :pos (event->pos e)}])
                        nil))
                    :className "sortable-container"}
        (om/build (:box-view opts) item
          {:opts (:opts opts) :react-key id})))))

(defn new-ids [sort]
  (zipmap sort (mapv (fn [_] (u/guid)) sort)))

(def zortable-styles #js {:WebkitTouchCallout "none"
                          :WebkitUserSelect "none"
                          :KhtmlUserSelect "none"
                          :MozUserSelect "none"
                          :msUserSelect "none"
                          :userSelect "none"})

(defn disabled-zortable [{:keys [sort items]} owner opts]
  (reify
    om/IDisplayName (display-name [_] "Zortable")
    om/IRender
    (render [_]
      (apply dom/div #js {:className "zort-list"
                          :style zortable-styles} 
        (map (fn [item-id]
               (let [item (items item-id)]
                 (om/build sort-wrapper item
                   {:opts opts :init-state {:id item-id}
                    :react-key item-id})))
          sort)))))

;; Listen to Events


(defn zortable [{:keys [sort items] :as data} owner opts]
  (letfn [(init-state []
            {:start-pos []
             :box {}})
          (next-state! [state]
            (om/update-state! owner #(merge % state)))
          (get-local [kw]
            (om/get-state owner kw))
          (set-local! [kw v]
            (om/set-state! owner kw v))]
    (if (:disabled? opts)
      (disabled-zortable data owner opts)
      (reify
        om/IDisplayName (display-name [_] "Zortable")
        om/IInitState
        (init-state [_]
          (let [ch (chan)]
            (letfn [(mouse-listen [tag e]
                      (put! ch [tag {:pos (event->pos e)}]))]
              (merge
                ;; State present during the whole lifecycle
                {:zid (u/guid) ;; Unique to each loaded zortable
                 :ids (om/value sort)
                 :id->eid (new-ids @sort) 
                 :ch ch
                 :listeners [(partial mouse-listen :drag)
                             (partial mouse-listen :stop-drag)]}
                ;; State present during drag
                (init-state)))))
        om/IWillMount
        (will-mount [_]
          (go-loop []
            (let [state (om/get-state owner) 
                  [tag {:keys [eid pos]}] (<! (get-local :ch))]
              (when (some? tag)
                (case tag
                  :start-drag
                  (do (doto js/window
                        (events/listen EventType.MOUSEMOVE
                          (get-local [:listeners 0]))
                        (events/listen EventType.MOUSEUP
                          (get-local [:listeners 1])))
                      (next-state! (start-drag eid pos (om/get-state owner))))
                  :drag (next-state! (drag pos state))
                  :stop-drag (do (doto js/window
                                   (events/unlisten EventType.MOUSEMOVE
                                     (get-local [:listeners 0]))
                                   (events/unlisten EventType.MOUSEUP
                                     (get-local [:listeners 1]))) 
                                 (next-state! (stop-drag state))))
                (recur)))))
        om/IWillUnmount
        (will-unmount [_]
          (async/close! (om/get-state owner :ch)))
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
                         (merge (new-ids to-create-ids)))]
              (next-state! {:ids @sort 
                            :id->eid eids}))))
        om/IRenderState
        (render-state [_ {:keys [ch ids id->eid zid box] :as state}]
          (let [moving-id (:id box)]
            (apply dom/div #js {:id zid :className "zort-list"
                                :style zortable-styles} 
              (when (dragging? state)
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
                           {:opts (assoc opts :ch ch)
                            :init-state {:eid eid :id item-id}
                            :react-key item-id}))))
                ids))))))))
