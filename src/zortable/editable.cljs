(ns zortable.editable
  (:require-macros [cljs.core.async.macros :as async :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! chan put!)]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [zortable.util :as u]
            [zortable.zortable :as z :refer [zortable]]))

;; ====================================================================== 
;; Components

(defn render-filler [box owner]
  (reify
    om/IDisplayName (display-name [_] "Filler")
    om/IRender
    (render [_]
      (dom/div nil
        (dom/br nil)
        (dom/br nil)))))

(def item-drag-class "item-drag")

(defn drag-icon [drag-class]
  (dom/span #js {:className drag-class} "\u22EE"))

;; FIX: editable markup is hardcoded and is a problem for styling and
;; reusability.
;; TODO: transform into a kioo component that takes markup as an argument.

(defn editable
  "Given an item with a :val-key pointing to a string, it displays it
   as normal text and allows editing when clicking on it. It exits edit
   mode onBlur or by pressing Enter (which adds a new empty item right after)."
  [item owner {:keys [edit-ch val-key id-key]}]
  (reify
    om/IDisplayName
    (display-name [_] "Editable")
    om/IRender
    (render [_]
      (letfn [(raise! [tag data]
                (go (>! edit-ch [tag data])))]
        (let [id (get item id-key)]
          (dom/div #js {:className "editable"} 
            (dom/i #js {:className (str item-drag-class " content icon")})
            (dom/i #js {:className "close icon"
                        :onClick (fn [_] (raise! :delete id))})
            (dom/input
              #js {:placeholder "New item" 
                   :type "text"
                   :ref "input"
                   :autoFocus (:focus? item)
                   :value (get item val-key)
                   :onFocus (fn [_] (raise! :focus id))
                   :onChange #(om/update! item val-key (.. % -target -value))
                   :onKeyDown #(when (= (.-key %) "Enter")
                                 (raise! :enter id))
                   :onBlur (fn [_] (raise! :blur id))})))))
    om/IDidMount
    (did-mount [_]
      (when (:focus? item)
        (.focus (om/get-node owner "input"))))))

(defn list-maker
  "Allows editing and sorting to a list of items:
  - items is a map with: {id {:id-key id :val-key \"String Value\"
  - sort is a vector [id]
  - add-node is a component for adding more list items"
  [{:keys [sort items] :as data} owner {:keys [id-key val-key add-node] :as opts}]
  (reify
    om/IDisplayName
    (display-name [_] "ListMaker")
    om/IInitState
    (init-state [_]
      {:edit-ch (chan)
       :z-ch (chan)
       :d-id nil ;; dragging-id
       :focus-id (if-not (empty? sort) (last sort))})
    om/IWillMount 
    (will-mount [_]
      (letfn [(focus-on [id]
                (om/set-state! owner :focus-id id))
              (delete-item [id]
                (when-not (= 1 (count @sort))
                  (om/transact! items #(dissoc % id))
                  (om/transact! sort (comp vec (partial remove #(= id %))))))
              (add-item [idx]
                (let [id (u/guid)]
                  (focus-on id)
                  (om/transact! items #(assoc % id {id-key id val-key ""}))
                  (om/transact! sort (partial u/insert-at idx id))))]
        (go-loop []
          (let [[tag id] (<! (om/get-state owner :edit-ch))]
            (when (some? tag)
              (case tag
                :enter (add-item (inc (u/find-index id @sort)))
                :focus (focus-on id) 
                :blur  (when (and (empty? (get-in @items [id val-key]))
                               (nil? (om/get-state owner :d-id)))
                         (delete-item id)) 
                :delete (delete-item id))
              (recur))))
        (go-loop []
          (let [[tag {:keys [id]}] (<! (om/get-state owner :z-ch))]
            (when (some? tag)
              (case tag
                :start-drag (om/set-state! owner :d-id id)
                :stop-drag
                (do (doseq [rid (->> @items
                                  (filter (comp empty? #(get % val-key) second))
                                  (map first)
                                  (remove #(= % (om/get-state owner :d-id))))]
                      (delete-item rid))
                    (om/set-state! owner :d-id nil))
                nil)
              (recur))))))
    om/IWillUnmount
    (will-unmount [_]
      (async/close! (om/get-state owner :edit-ch))
      (async/close! (om/get-state owner :z-ch)))
    om/IRenderState
    (render-state [_ {:keys [z-ch edit-ch focus-id]}]
      (dom/div nil
        (apply dom/div #js {:className "list-maker" :ref "ele-list"}
          (let [items' (->> items
                         (map (fn [[k v]]
                                [k (assoc v :focus? (= focus-id k))]))
                         (into {}))]
            (if-not (:disabled? opts)
              [(om/build zortable {:sort sort :items items'} 
                 {:opts {:box-view editable 
                         :id-key :item-id
                         :drag-class item-drag-class 
                         :box-filler render-filler
                         :pub-ch z-ch
                         :opts (assoc opts :edit-ch edit-ch)}})]
              (map #(om/build editable (get items' %)
                      {:opts (assoc opts :edit-ch edit-ch)
                       :key id-key})
                sort))))
        (if (some? add-node)
          (om/build add-node (last sort) {:opts {:edit-ch edit-ch}}))))))
