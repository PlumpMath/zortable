(ns ^:figwheel-always boxes.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.core :refer [zortable]]))

(enable-console-print!)

(defn pos->hue [[x y]]
  (mod (+ (/ x 2) (/ y 2)) 360))

(def box-width 50)
(def box-height 20)

(defn build-box [id]
  {:item-id id 
   :width box-width
   :height (+ box-height (* 10 id)) 
   :hue (pos->hue [(rand-int 500) (rand-int 500)])})

(def n-boxes 10)
(def n-cards 2)

(def all-boxes (mapv build-box (range n-boxes)))

(def cards
  (mapv (fn [bxs] {:card-id (str (gensym)) :items (vec bxs)}) 
        (partition (/ n-boxes n-cards) all-boxes)))

(defonce app-state
  (atom {:cards cards}))

(defn box-color [box]
  (let [opacity 1]
    (str "hsla(" (:hue box) ",50%,50%," opacity ")")))

(defn render-filler [box owner]
  (reify
    om/IDisplayName (display-name [_] "Filler")
    om/IRender
    (render [_]
      (dom/div nil
        (dom/br nil)
        (dom/br nil)))))

(def drag-class "drag-item")

(defn drag-icon []
  (dom/span #js {:className drag-class} "\u22EE"))

(defn render-item [item owner]
  (reify
    om/IDisplayName (display-name [_] "Box")
    om/IRender
    (render [_]
      (when item 
        (dom/div #js {:style #js {:backgroundColor (box-color item)
                                  :height (:height item)
                                  :width 100}}
          (drag-icon)
          (:item-id item))))))

(defn card [{:keys [items card-id]} owner]
  (reify
    om/IDisplayName (display-name [_] "Card")
    om/IRender
    (render [_]
      (dom/div #js {:id card-id :style #js {:position "relative"}}
        (drag-icon)
        (pr-str (map :item-id items))
        (om/build zortable items 
          {:opts {:box-view render-item
                  :id-key :item-id
                  :drag-class drag-class 
                  :box-filler render-filler}})))))

(defn render-state [state owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:style #js {:-webkit-touch-callout "none"
                                :-webkit-user-select "none"
                                :-khtml-user-select "none"
                                :-moz-user-select "none"
                                :-ms-user-select "none"
                                :user-select "none"}}
        (dom/div nil 
          (dom/h1 nil "Sortable")
          (om/build zortable (:cards state)
            {:opts {:box-view card
                    :id-key :card-id
                    :drag-class drag-class
                    :box-filler render-filler}}))))))

(om/root render-state app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
) 

