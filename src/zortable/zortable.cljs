(ns zortable.zortable
  (:require [clojure.set :as set]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [zortable.core :as z]
            [zortable.draggable :as zd]
            [zortable.util :as u]))

;; ====================================================================== 
;; Indexes

(defn eid->box
  "Finds the internal eid from the user given id"
  [id]
  (zd/add-size (zd/add-pos (zd/add-node {:eid id}))))

(defn eid->id
  "Returns the user given id from the internal eid"
  [id->eid ele-id]
  (some (fn [[id eid]] (if (= ele-id eid) id)) id->eid))

(defn sort-by-pos
  "Sorts the user-given ids by their vertical positions 
   in the DOM into a vector"
  [id->eid ids]
  (vec (sort-by (comp second zd/box-center eid->box id->eid) ids)))

(defn new-ids [sort]
  (zipmap sort (mapv (fn [_] (u/guid)) sort)))

;; ====================================================================== 
;; Wrapper Components

(defn sort-filler [{:keys [item box]} owner opts]
  (reify
    om/IDisplayName (display-name [_] "SortFiller")
    om/IRender
    (render [_]
      (dom/div #js {:className "sortable-filler"}
        (om/build (:box-filler opts) item
          {:init-state (select-keys box [:width :height])
           :opts (:opts opts)})))))

(def zortable-styles #js {:WebkitTouchCallout "none"
                          :WebkitUserSelect "none"
                          :KhtmlUserSelect "none"
                          :MozUserSelect "none"
                          :msUserSelect "none"
                          :userSelect "none"
                          :position "relative"})

(defn disabled-zortable [{:keys [sort items]} owner opts]
  (reify
    om/IDisplayName
    (display-name [_] "Zortable")
    om/IRender
    (render [_]
      (apply dom/div #js {:className "zort-list"
                          :style zortable-styles} 
        (map (fn [item-id]
               (om/build (:box-view opts) (items item-id)
                 {:opts opts :init-state {:id item-id}}))
          sort)))))

;; with-filler needs to be a separate component:
;; https://facebook.github.io/react/docs/create-fragment.html
;; When reordering a 'set' of children, you either wrap them or
;; provide a key-fragment

(defn with-filler
  "Wrapper over draggable to render a filler while it's dragging"
  [{:keys [eid id item-id item]} owner opts]
  (om/component
    (dom/div nil
      (om/build zd/draggable (assoc item :drag/id eid) {:opts opts})
      (when (= item-id id)
        (om/build sort-filler {:item item} {:opts opts})))))

(defn zortable
  "Given a set of items and their sort order, it allows the user
   to be sort them via dragging.

   (om/build zortable data opts)

   data -
    :items {item-id item-data}
    :sort [item-ids]
   opts -
    :drag-class - className to click to start the dragging,
                  should be rendered by box-view
    :box-view - component that renders item-data
    :box-filler - component that fills the void left by a dragging object"
  [{:keys [sort items] :as data} owner opts]
  (if (:disabled? opts)
    (disabled-zortable data owner opts)
    (reify
      om/IDisplayName
      (display-name [_] "Zortable")
      om/IInitState
      (init-state [this]
        {:ids (om/value sort)
         :drag/id nil
         :id->eid (new-ids @sort)})
      z/IWire
      (get-owner [_] owner)
      (get-signal [_] nil)
      z/IStep
      (step [_ state [[_ id] e]]
        (let [state' (-> state
                       (update :ids (partial sort-by-pos (:id->eid state)))
                       (assoc :drag/id (if (zd/dragging? (:box e)) id)))]
          ;; FIX: sideffects shouldn't be here
          (when (and (not (zd/dragging? (:box e)))
                     (not= @sort (:ids state')))
            (om/update! sort (:ids state')))
          state'))
      om/IWillReceiveProps
      (will-receive-props [_ {:keys [items sort]}]
        (assert (= (count items) (count sort))
          "Length of sort and items don't match")
        (when-not (= (count sort) (count (om/get-state owner :ids)))
          (let [old-ids (set (om/get-state owner :ids))
                future-ids (set @sort)
                to-create-ids (set/difference future-ids old-ids)
                to-delete-ids (set/difference old-ids future-ids)
                eids (->> to-delete-ids
                       (apply dissoc (om/get-state owner :id->eid))
                       (merge (new-ids to-create-ids)))]
            (om/update-state! owner #(merge % {:ids @sort :id->eid eids})))))
      om/IRenderState
      (render-state [this {:keys [ids id->eid :drag/id] :as state}]
        (let [box (->> ids
                    (map #(:box @(z/signal this [:draggable %])))
                    (filter zd/dragging?)
                    first)]
          (apply dom/div #js {:className "zort-list"
                              :style zortable-styles} 
            (map (fn [item-id]
                   (let [eid (id->eid item-id)
                         item (items item-id)]
                     (om/build with-filler {:item item :eid eid
                                            :id id :item-id item-id} 
                       {:opts {:z (z/signal this [:draggable item-id])
                               :dragger (zd/snapped-drag)
                               :drag-class (:drag-class opts) 
                               :view (:box-view opts)
                               :box-filler (:box-filler opts)}
                        ;; Necessary for sorting
                        :react-key eid})))
              ids)))))))

