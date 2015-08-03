(ns zortable.zortable
  (:import [goog.events EventType])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [clojure.set :as set]
            [cljs.core.async :as async :refer [>! <! chan put!]]
            [goog.dom :as gdom]
            [goog.style :as style]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [zortable.core :as z]
            [zortable.draggable :as zd]
            [zortable.util :as u]))

;; ====================================================================== 
;; General Protocols

(defprotocol IHandle
  (handle [_ e]))

;; ======================================================================  
;; Box

(defn add-node
  "Adds the DOM node to the box as :node"
  [box]
  {:pre [(some? (:eid box))]}
  (assoc box :node (.getElementById js/document (:eid box))))

(defn add-size
  "Adds the box size as :width and :height from the DOM node"
  [box]
  (let [n (aget (.-childNodes (:node box)) 0) 
        size (style/getSize n)]
    (assoc box :width (.-width size) :height (.-height size))))

(defn add-pos
  "Adds the box position as :left and :top from the DOM node"
  [box]
  (let [final-pos (style/getPosition (:node box))
        left (.-x final-pos)
        top (.-y final-pos)]
    (assoc box :left left :top top)))

(defn box-center
  "Calculates the box-center position"
  [box]
  (letfn [(add [kb ks]
            (+ (kb box) (/ (ks box) 2)))]
    [(add :left :width) (add :top :height)]))

(defn topleft-pos [{:keys [left top]}]
  [left top])

(defn box-offset [pos box]
  (->> box topleft-pos (mapv - pos)))

(defn element-inside?
  "Finds if an element is inside another element with the specified class"
  [class element]
  (some? (gdom/getAncestorByClass element class)))

(defn eid->box
  "Finds the internal eid from the user given id"
  [id]
  (add-size (add-pos (add-node {:eid id}))))

(defn eid->id
  "Returns the user given id from the internal eid"
  [id->eid ele-id]
  (some (fn [[id eid]] (if (= ele-id eid) id)) id->eid))

(defn sort-by-pos
  "Sorts the user-given ids by their vertical positions in the DOM into a vector"
  [id->eid ids]
  (vec (sort-by (comp second box-center eid->box id->eid) ids)))

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
  {:pre [(fn? (:handler opts))]}
  (reify
    om/IDisplayName
    (display-name [_] "SortWrapper")
    om/IRenderState
    (render-state [_ {:keys [eid id]}]
      (dom/div #js {:className "sortable-container"}
        (om/build (:box-view opts) item
          {:opts (:opts opts) :react-key id})))))

(defn new-ids [sort]
  (zipmap sort (mapv (fn [_] (u/guid)) sort)))

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

;; Needs to be a separate component:
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
        (-> state
          (update :ids (partial sort-by-pos (:id->eid state)))
          (assoc :drag/id (if (zd/dragging? (:box e)) id))))
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

