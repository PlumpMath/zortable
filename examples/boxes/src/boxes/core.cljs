(ns ^:figwheel-always boxes.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.core :refer [zortable]]))

;; Example

(defn pos->hue
  [[x y]]
  (mod (+ (/ x 2) (/ y 2)) 360))

(def box-width 50)
(def box-height 20)

(defn build-box
  [id]
  {:item-id id 
   :width box-width
   :height (+ box-height (* 10 id)) 
   :hue (pos->hue [(rand-int 500) (rand-int 500)])})

(defonce app-state (atom {:boxes (mapv build-box (range 5))}))

(defn box-color
  [box]
  (let [opacity 1]
    (str "hsla(" (:hue box) ",50%,50%," opacity ")")))

(defn render-filler [box owner]
  (reify
    om/IDisplayName (display-name [_] "Filler")
    om/IRender
    (render [_]
      (dom/div nil 
        "Filler Box"))))

(def drag-class "drag-item")

(defn render-item [item owner]
  (reify
    om/IDisplayName (display-name [_] "Box")
    om/IRender
    (render [_]
      (when item 
        (dom/div #js {:style #js {:backgroundColor (box-color item)}}
          (dom/h3 #js {:class drag-class} "O")
          (dom/p nil (:item-id item)))))))

(defn render-state
  [state]
  (dom/div #js {:style #js {:-webkit-touch-callout "none"
                            :-webkit-user-select "none"
                            :-khtml-user-select "none"
                            :-moz-user-select "none"
                            :-ms-user-select "none"
                            :user-select "none"}}
    (dom/div #js {:style #js {:position "relative"}}
      (dom/h1 nil "Sortable")
      (dom/p nil (pr-str (map :item-id (:boxes state))))
      (om/build zortable (:boxes state)
        {:opts {:box-view render-item
                :id-key :item-id
                :drag-class drag-class 
                :box-filler render-filler}}))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
) 

