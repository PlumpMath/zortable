(ns ^:figwheel-always editable.core
    (:require-macros [cljs.core.async.macros :as async :refer (go go-loop)])
    (:require [cljs.core.async :as async :refer (<! >! chan put!)]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.editable :refer [list-maker]]))

(enable-console-print!)

(defn build-box [id]
  {:item-id (+ 100 id)
   :value (str id)})

(def n-strings 5)

(def all-boxes (mapv build-box (range n-strings)))

(def cards
  (zipmap (map :item-id all-boxes) all-boxes))

(defonce app-state
  (atom {:items cards
         :sort (vec (keys cards))}))

(defn add-node
  "Add button for adding a new list element"
  [last-sort owner {:keys [edit-ch]}]
  (om/component
    (dom/div (clj->js {:style {:cursor "pointer"
                               :font-size "2em"}
                       :onClick #(put! edit-ch [:enter last-sort])}) "+")))

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
                  {:opts {:add-node add-node
                          :id-key :item-id
                          :val-key :value}})))))

(om/root render-state app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []) 
