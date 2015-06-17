(ns zortable.editable
  (:require-macros [cljs.core.async.macros :as async :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! chan put!)]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [zortable.util :as u]
            [zortable.core :as z :refer [zortable]]))

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
                   :autoFocus (:focus? item)
                   :value (get item val-key)
                   :onFocus (fn [_] (raise! :focus id))
                   :onChange #(om/update! item val-key (.. % -target -value))
                   :onKeyDown #(when (= (.-key %) "Enter")
                                 (raise! :enter id))
                   :onBlur (fn [_] (raise! :blur id))})))))))

(defn list-maker
  "Allows editing and sorting to a list of items:
  - items is a map with: {id {:id-key id :val-key \"String Value\"
  - sort is a vector [id]" 
  [{:keys [sort items] :as data} owner {:keys [id-key val-key] :as opts}]
  (reify
    om/IDisplayName
    (display-name [_] "ListMaker")
    om/IInitState
    (init-state [_]
      {:edit-ch (chan)
       :focus-id (if-not (empty? sort) (last sort))})
    om/IWillMount 
    (will-mount [_]
      (letfn [(focus-on [id]
                (om/set-state! owner :focus-id id))
              (delete-item [id]
                (om/transact! items #(dissoc % id))
                (om/transact! sort (comp vec (partial remove #(= id %)))))
              (add-item [idx]
                (let [id (u/guid)]
                  (om/transact! items #(assoc % id {id-key id val-key ""}))
                  (om/transact! sort (partial u/insert-at idx id))
                  id))]
        (go-loop []
          (let [[tag id] (<! (om/get-state owner :edit-ch))]
            (when (some? tag)
              (case tag
                :enter (focus-on (add-item (inc (u/find-index id @sort))))
                :focus (focus-on id) 
                :blur (when (empty? (get-in @items [id val-key]))
                        (delete-item id)) 
                :delete (delete-item id))
              (recur))))))
    om/IWillUnmount
    (will-unmount [_]
      (async/close! (om/get-state owner :edit-ch)))
    om/IRenderState
    (render-state [_ {:keys [edit-ch focus-id]}]
      (dom/div #js {:className "list-maker"}
        (dom/div #js {:className "list-maker" :ref "ele-list"}
          (om/build zortable
            {:sort sort
             :items (into {} (map (fn [[k v]]
                                    [k (assoc v :focus? (= focus-id k))])
                               items))} 
            {:opts {:box-view editable 
                    :id-key :item-id
                    :drag-class item-drag-class 
                    :box-filler render-filler
                    :opts (assoc opts :edit-ch edit-ch)}}))))))
