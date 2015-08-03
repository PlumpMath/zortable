(ns ^:figwheel-always click-out.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.util :as u]
              [zortable.click :as click]))
  
(enable-console-print!)

(defonce app-state
  (atom {:click-out nil
         :click-in nil
         :click-else nil}))

(defn box-component
  "Recognizes clicks inside or outside."
  [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "Box")
    om/IDidMount
    (did-mount [this]
      (om/set-state! owner :listen-in-key
        (click/install-in! ["other-box"]
          #(om/update! data :click-else (u/event->pos %))))
      (om/set-state! owner :listen-out-key
        (click/install-out! ["box"]
          #(om/update! data :click-out (u/event->pos %)))))
    om/IWillUnmount
    (will-unmount [this]
      (click/uninstall! (om/get-state owner :listen-in-key))
      (click/uninstall! (om/get-state owner :listen-out-key)))
    om/IRender
    (render [this]
      (dom/div #js {:className "box"
                    :onClick #(om/update! data :click-out (u/event->pos %))
                    :style #js {:backgroundColor "red" 
                                :height 100 
                                :width 100}}))))

(defn other-box [_ _]
  (om/component
    (dom/div #js {:className "other-box"
                  :onClick #(js/alert "Clicked!")
                  :style #js {:backgroundColor "blue" 
                              :height 100 
                              :width 100}})))

(defn render-click [click owner]
  (om/component
    (when (some? click)
      (dom/p nil (pr-str click)))))

(defn render-state [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil 
        (om/build box-component data)
        (dom/span nil
          "Last Click In: " (om/build render-click (:click-in data)))
        (dom/span nil 
          "Last Click Out: " (om/build render-click (:click-out data)))
        (dom/span nil
          "Last Click Blue: " (om/build render-click (:click-else data)))
        (om/build other-box {})))))

(om/root render-state app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []) 
