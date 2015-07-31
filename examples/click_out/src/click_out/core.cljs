(ns ^:figwheel-always click-out.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [zortable.core :as z]
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
    z/IHandle
    (handle [_ [tag e]]
      (om/update! data tag (z/event->pos e)))
    om/IDisplayName
    (display-name [_] "Box")
    om/IDidMount
    (did-mount [this]
      (om/set-state! owner :listen-in-key
        (click/install-in! ["other-box"] #(z/handle this [:click-else %])))
      (om/set-state! owner :listen-out-key
        (click/install-out! ["box"] #(z/handle this [:click-out %]))))
    om/IWillUnmount
    (will-unmount [this]
      (click/uninstall! (om/get-state owner :listen-in-key))
      (click/uninstall! (om/get-state owner :listen-out-key)))
    om/IRender
    (render [this]
      (dom/div #js {:className "box"
                    :onClick #(z/handle this [:click-in %])
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
