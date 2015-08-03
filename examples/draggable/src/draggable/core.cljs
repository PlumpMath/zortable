(ns ^:figwheel-always draggable.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.core :as z]
              [zortable.draggable :as zd]))

;; ====================================================================== 
;; Example

(enable-console-print!)

(defn pos->hue [[x y]]
  (mod (+ (/ x 2) (/ y 2)) 360))

(def box-side 50)

(defn build-box [id]
  {:item-id id 
   :width box-side 
   :height box-side 
   :hue (pos->hue [(rand-int 500) (rand-int 500)])})

(defonce app-state
  (atom {:items (mapv build-box (range 2))}))

(defn box-color [box]
  (let [opacity 1]
    (str "hsla(" (:hue box) ",50%,50%," opacity ")")))

(defn render-box [item _]
  (om/component
    (dom/div #js {:className "box"
                  :style #js {:backgroundColor (box-color item)
                              :height (:height item)
                              :position "relative"
                              :width (:width item)
                              :top (:top item)
                              :left (:left item)}})))
(defn mean [xs]
  (/ (reduce + xs) (count xs)))

(defn guidelines [items owner]
  (reify
    om/IInitState
    (init-state [_] {:a 1})
    z/IWire
    (get-owner [_] owner)
    (get-signal [_] nil)
    om/IRenderState
    (render-state [this state]
      (let [boxes (map #(:box @(z/signal this [:draggable (:item-id %)])) items)]
        (dom/div nil
          (om/build render-box {:height box-side 
                                :width 10
                                :top (mean (map :top boxes))
                                :hue 100
                                :left 0})
          (om/build render-box {:height 10
                                :width box-side 
                                :top 0
                                :hue 100
                                :left (mean (map :left boxes))})
          (apply dom/div nil
            (map (fn [ item]
                   (om/build zd/draggable item 
                     {:opts {:drag-class "box"
                             :react-key (:item-id item)
                             :z (z/signal this [:draggable (:item-id item)])
                             :view render-box}}))
              items)))))))

(defn main [data owner]
  (om/component
    (dom/div #js {:style #js {:userSelect "none"}} 
      (om/build guidelines (:items data)))))

;; TODO: use no-local

(om/root main app-state
  {:target (. js/document (getElementById "app"))
   ;; :descriptor (om/no-local-descriptor om/no-local-state-methods)
   })
