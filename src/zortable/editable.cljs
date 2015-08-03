(ns zortable.editable
  (:require-macros [cljs.core.async.macros :as async :refer (go go-loop)])
  (:require [cljs.core.async :as async :refer (<! >! chan put!)]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [zortable.core :as z]
            [zortable.util :as u]
            [zortable.zortable :as zz :refer [zortable]]))

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
  [item owner {:keys [z val-key id-key]}]
  (reify
    om/IDisplayName
    (display-name [_] "Editable")
    z/IWire
    (get-owner [_] owner)
    (get-signal [_] z)
    om/IRender
    (render [this]
      (let [id (get item id-key)]
        (dom/div #js {:className "editable"} 
          (dom/i #js {:className (str item-drag-class " content icon")})
          (dom/i #js {:className "close icon"
                      :onClick (fn [_] (z/raise! this [:delete id]))})
          (dom/input
            #js {:placeholder "New item" 
                 :type "text"
                 :ref "input"
                 :autoFocus (:focus? item)
                 :value (get item val-key)
                 :onFocus (fn [_] (z/raise! this [:focus id]))
                 :onChange #(om/update! item val-key (.. % -target -value))
                 :onKeyDown #(when (= (.-key %) "Enter")
                               (z/raise! this [:enter id]))
                 :onBlur (fn [_] (z/raise! this [:blur id]))}))))
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
      {:focus-id (if-not (empty? sort) (last sort))})
    z/IWire
    (get-owner [_] owner)
    (get-signal [_] nil)
    z/IStep
    (step [this state [route [tag event]]]
      (letfn [(delete-item [id]
                (when-not (= 1 (count @sort))
                  (om/transact! items #(dissoc % id))
                  (om/transact! sort (comp vec (partial remove #(= id %))))))]
        (case route 
          :editable
          (let [id event]
            (case tag
              :enter (let [idx (inc (u/find-index id @sort))
                           id (u/guid)]
                       (om/transact! items #(assoc % id {id-key id val-key ""}))
                       (om/transact! sort (partial u/insert-at idx id))
                       (assoc state :focus-id id))
              :focus (assoc state :focus-id id) 
              :blur  (when (and (empty? (get-in @items [id val-key]))
                                (nil? (:drag/id @(z/signal this :zortable))))
                       (doseq [rid (->> @items
                                     (filter (comp empty? #(get % val-key) second))
                                     (map first))]
                         (delete-item rid))
                       state) 
              :delete (do (delete-item id)
                          state)))
          state)))
    om/IRenderState
    (render-state [this {:keys [focus-id]}]
      (dom/div nil
        (apply dom/div #js {:className "list-maker" :ref "ele-list"}
          (let [items' (->> items
                         (map (fn [[k v]]
                                [k (assoc v :focus? (= focus-id k))]))
                         (into {}))]
            (if-not (:disabled? opts)
              [(om/build zz/zortable {:sort sort :items items'} 
                 {:opts {:z (z/signal this :zortable)
                         :box-view editable 
                         :id-key :item-id
                         :drag-class item-drag-class 
                         :box-filler render-filler
                         :opts {:z (z/signal this :editable)
                                :val-key val-key
                                :id-key id-key}}})]
              (map #(om/build editable (get items' %)
                      {:opts {:z (z/signal this :editable)}
                       :key id-key})
                sort))))
        (if (some? add-node)
          (om/build add-node (last sort)
            {:opts {:z (z/signal this :editable)}}))))))
