(ns ^:figwheel-always editable.core
    (:require-macros [cljs.core.async.macros :as async :refer (go go-loop)])
    (:require [cljs.core.async :as async :refer (<! >! chan put!)]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.core :as z :refer [zortable]]))

(enable-console-print!)

(defn- add-to-list
  "Should check if there is already a last one"
  [xs kork]
  {:pre [(om/cursor? xs)]}
  (when-not (empty? (last @xs))
    (let [empty-val {kork ""}]
      (om/transact! xs #(if (empty? %) [empty-val] (conj % empty-val))))))

(defn find-index [v coll]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll)) 
            (= v (last coll)))
      i)))

(defn insert-at [n v xs]
  (if (< n (count xs))
    (into (conj (subvec xs 0 n) v) (subvec xs n))
    (conj xs v)))

(defn drop-nth
  "Drops specified index from vector"
  [n xs]
  (vec (concat
         (subvec xs 0 n)
         (subvec xs (inc n)))))

(defn build-box [id]
  {:item-id id
   :value (str id)})

(def n-strings 5)

(def all-boxes (mapv build-box (range n-strings)))

(def cards
  (zipmap (map :item-id all-boxes) all-boxes))

(defonce app-state
  (atom {:items cards
         :sort (vec (keys cards))}))

(defn render-filler [box owner]
  (reify
    om/IDisplayName (display-name [_] "Filler")
    om/IRender
    (render [_]
      (dom/div nil
        (dom/br nil)
        (dom/br nil)))))

(def item-drag-class "drag-item")
(def card-drag-class "drag-class")

(defn drag-icon [drag-class]
  (dom/span #js {:className drag-class} "\u22EE"))

(defn editable
  "Given a cursor+korks into a text field, it displays it as normal
   text and allows editing when clicking on it. It exits edit mode by
   clicking somewhere else (onBlur) or pressing Enter. It is deleted
   when it stores nil in the cursor. Used in record builder editor "
  [item owner {:keys [edit-ch val-key id-key]}]
  (reify
    om/IDisplayName (display-name [_] "Editable")
    om/IRender
    (render [_]
      (letfn [(raise! [tag data]
                (go (>! edit-ch [tag data])))]
        (let [id (get item id-key)]
          (dom/li nil
            (drag-icon item-drag-class)
            (dom/input
              #js {:placeholder "New item" 
                   :type "text"
                   :autoFocus (:focus? item)
                   :value (get item val-key)
                   :onFocus (fn [_] (raise! :focus id))
                   :onChange #(om/update! item val-key (.. % -target -value))
                   :onKeyDown #(when (= (.-key %) "Enter")
                                 (raise! :enter id))
                   :onBlur (fn [_] (raise! :blur id))})
            (dom/button #js {:className "close icon"
                             :onClick (fn [_] (raise! :delete id))}
              "X")))))))

(defn list-maker
  "Adds elements to a list in cursor korks"
  [{:keys [sort items] :as data} owner {:keys [id-key val-key] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      {:edit-ch (chan)
       :focus-id (if-not (empty? sort) (last sort))})
    om/IWillMount 
    (will-mount [_]
      (letfn [(focus-on [id]
                (om/set-state! owner :focus-id id))
              (delete-item [id]
                (om/transact! data
                  (fn [d]
                    (-> d 
                      (update :items #(dissoc % id))
                      (update :sort (comp vec (partial remove #(= id %))))))))
              (add-item [idx]
                (let [id (z/guid)]
                  (om/transact! data
                    (fn [d]
                      (-> d 
                        (update :items #(assoc % id {id-key id val-key ""}))
                        (update :sort (partial insert-at idx id)))))
                  id))]
        (go-loop []
          (let [[tag id] (<! (om/get-state owner :edit-ch))]
            (when-not (nil? tag)
              (case tag
                :enter (focus-on (add-item (inc (find-index id @sort))))
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
        (let [itemcount (count items)
              itemcount (if (zero? itemcount) 1 itemcount)]
          (dom/ul #js {:className "element-list" :ref "ele-list"}
            (om/build zortable
              {:sort sort
               :items (into {} (map (fn [[k v]]
                                      [k (assoc v :focus? (= focus-id k))])
                                 items))} 
              {:opts {:box-view editable 
                      :id-key :item-id
                      :drag-class item-drag-class 
                      :box-filler render-filler
                      :opts (assoc opts :edit-ch edit-ch)}})))))))

(defn render-state [state owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil 
        (dom/h1 nil "Sortable")
        (pr-str (:sort state))
        (apply dom/ul nil
          (om/build-all
            #(om/component (dom/li nil (get-in state [:items % :value])))
            (:sort state)))
        (om/build list-maker state
          {:opts {:id-key :item-id :val-key :value}})))))

(om/root render-state app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
) 

