(ns ^:figwheel-always draggable.core
    (:import [goog.events EventType])
    (:require [goog.style :as style]
              [goog.dom :as gdom]
              [goog.events :as events]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(defprotocol IHandle
  (handle [_ e]))

(defprotocol IDrag
  (drag-start [this state event])
  (drag-move [this state event])
  (drag-stop [this state event]))

(defn dragging? [state]
  (not (empty? (:box state))))

(defn topleft-pos [{:keys [left top]}]
  [left top])

(defn box-offset [pos box]
  (->> box topleft-pos (mapv - pos)))

(defn event->pos [e]
  [(.-clientX e) (.-clientY e)])

(defn add-pos
  "Adds the box position as :left and :top from the DOM node"
  [box]
  (let [final-pos (style/getPosition (:node box))
        left (.-x final-pos)
        top (.-y final-pos)]
    (assoc box :left left :top top)))

(defn element-inside?
  "Finds if an element is inside another element with the specified class"
  [class element]
  (some? (gdom/getAncestorByClass element class)))

(defn free-drag []
  (reify
    IDrag
    (drag-start [_ _ {:keys [node pos]}]
      (let [box (add-pos {:node node})]
        (assoc box :offset (box-offset pos box))))
    (drag-move [_ box {:keys [pos]}]
      (let [[left top] (map - pos (:offset box))]
        (assoc box :left left :top top)))
    (drag-stop [_ _ _]
      (println "stop")
      {})))

;; ====================================================================== 
;; Example

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

(defonce app-state
  (atom {:item (build-box 1)}))

(defn box-color [box]
  (let [opacity 1]
    (str "hsla(" (:hue box) ",50%,50%," opacity ")")))

(defn install-move! [owner h]
  (om/set-state! owner :listener-move
    (events/listen js/window EventType.MOUSEMOVE h)))

(defn install-up! [owner h]
  (om/set-state! owner :listener-up
    (events/listen js/window EventType.MOUSEUP h)))

(defn uninstall! [owner]
  (doseq [k [:listener-move :listener-up]]
    (events/unlistenByKey (om/get-state owner k))))

(defn render-item [item owner opts]
  (reify
    om/IDisplayName (display-name [_] "Box")
    om/IInitState
    (init-state [_] {:dragger (free-drag)})
    IHandle
    (handle [this [tag e]]
      (let [d (om/get-state owner :dragger) 
            box (om/get-state owner :box)]
        (om/set-state! owner :box
          (case tag
            :start-drag
            (do (install-move! owner
                  #(handle this [:drag {:pos (event->pos %)}]))
                (install-up! owner
                  #(handle this [:stop-drag {:pos (event->pos %)}]))
                (drag-start d box e))
            :drag (drag-move d box e)
            :stop-drag (do (uninstall! owner)
                           (drag-stop d box e))))))
    om/IRenderState
    (render-state [this {:keys [box]}]
      (dom/div #js {:style #js {:position "absolute"
                                :zIndex 1
                                :top (:top box)
                                :left (:left box)
                                ;; =============
                                :backgroundColor (box-color item)
                                :height (:height item)
                                :width 100}
                    :className "box"
                    :onMouseDown
                    (fn [e]
                      (when (element-inside? (:drag-class opts) (.-target e))
                        (handle this [:start-drag {:node (om/get-node owner)
                                                   :pos (event->pos e)}])))}))))

(defn main [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil 
        (dom/h1 nil "draggable")
        (om/build render-item (:item data) {:opts {:drag-class "box"}})))))

(om/root main app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
) 

