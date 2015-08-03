(ns zortable.draggable
  (:import [goog.events EventType])
  (:require [goog.style :as style]
            [goog.dom :as gdom]
            [goog.events :as events]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [zortable.core :as z]
            [zortable.util :as u]))

;; ====================================================================== 
;; Draggable

(defprotocol IDrag
  (drag-start [this state event])
  (drag-move [this state event])
  (drag-stop [this state event]))

(defn dragging? [box]
  (:dragging? box))

(defn topleft-pos [{:keys [left top]}]
  [left top])

(defn box-offset [pos box]
  (->> box topleft-pos (mapv - pos)))

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
        (assoc box
          :offset (box-offset pos box)
          :position "absolute"
          :dragging? true)))
    (drag-move [_ box {:keys [pos]}]
      (let [[left top] (map - pos (:offset box))]
        (assoc box :left left :top top)))
    (drag-stop [_ box _]
      (assoc box :dragging? false))))

(defn snapped-drag []
  (let [d (free-drag)]
    (reify
      IDrag
      (drag-start [this b e] (drag-start d b e))
      (drag-move [this b e] (drag-move d b e))
      (drag-stop [this b e]
        (assoc (drag-stop d b e) :position "static")))))

(defonce installed? (atom {:listener-move false
                           :listener-up false}))

(defn install-move! [owner h]
  (when-not (:listener-move @installed?)
    (swap! installed? #(assoc % :listener-move true))
    (om/set-state! owner :listener-move
      (events/listen js/window EventType.MOUSEMOVE h))))

(defn install-up! [owner h]
  (when-not (:listener-up @installed?)
    (swap! installed? #(assoc % :listener-up true))
    (om/set-state! owner :listener-up
      (events/listen js/window EventType.MOUSEUP h))))

(defn uninstall! [owner]
  (doseq [k [:listener-move :listener-up]]
    (swap! installed? #(assoc % k false))
    (events/unlistenByKey (om/get-state owner k))))

(defn draggable [item owner {:keys [dragger drag-class view z]}]
  {:pre [(fn? view)]}
  (reify
    om/IDisplayName
    (display-name [_] "Draggable")
    om/IInitState
    (init-state [_]
      {:dragger (or dragger (free-drag))
       :box {:dragging? false
             :position "static"
             :top nil
             :left nil}})
    z/IWire
    (get-owner [_] owner)
    (get-signal [_] z)
    z/IStep
    (step [this state [tag e]]
      (let [{:keys [box dragger]} state]
        (assoc state :box
          (case tag
            :drag/start (drag-start dragger box e)
            :drag/move (drag-move dragger box e)
            :drag/stop (drag-stop dragger box e)))))
    om/IRenderState
    (render-state [this {:keys [box]}]
      (dom/div #js {:id (:drag/id item)
                    :style #js {:position (:position box) 
                                :userSelect "none"
                                :zIndex 1
                                :top (:top box)
                                :left (:left box)}
                    :onMouseDown
                    (fn [e]
                      (when (element-inside? drag-class (.-target e))
                        (install-move! owner
                          #(z/handle this [:drag/move {:pos (u/event->pos %)}]) )
                        (install-up! owner
                          #(do (uninstall! owner)
                               (z/handle this [:drag/stop {:pos (u/event->pos %)}])))
                        (z/handle this [:drag/start {:node (om/get-node owner)
                                                     :pos (u/event->pos e)}])))}
        (om/build view item)))))

