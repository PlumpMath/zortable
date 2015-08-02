(ns ^:figwheel-always draggable.core
    (:import [goog.events EventType])
    (:require [goog.style :as style]
              [goog.dom :as gdom]
              [goog.events :as events]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(defprotocol IStep
  (get-owner [this])
  (get-signal [this])
  (step [this state event]))

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
        (assoc box
          :offset (box-offset pos box)
          :dragging? true)))
    (drag-move [_ box {:keys [pos]}]
      (let [[left top] (map - pos (:offset box))]
        (assoc box :left left :top top)))
    (drag-stop [_ box _]
      (assoc box :dragging? false))))

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

;; Should consider the three elements.
(defn handle [this action]
  {:pre [(satisfies? IStep this)]}
  (let [owner (get-owner this)
        state (om/get-state owner)
        state' (step this state action)]
    (when (and (some? state') (not= state state'))
      (om/set-state! owner state')
      (when-let [signal (get-signal this)]
        (reset! signal state')))))

(defn signal-map [owner]
  (.-z$signals owner))

(defn add-signal! [owner k z]
  (set! (.-z$signals owner) (merge (signal-map owner) {k z})))

(defn signal [this k]
  (let [owner om/*parent*]
    (or (get (signal-map owner) k)
      (let [z (atom {})]
        (add-watch z k (fn [_ _ _ state']
                         (om/update-state! owner identity)
                         (when (satisfies? IStep this)
                           (handle this [k state']))))
        (add-signal! owner k z)
        z))))

(defn draggable [item owner {:keys [drag-class z view]}]
  (reify
    om/IDisplayName (display-name [_] "Box")
    om/IInitState
    (init-state [_]
      {:dragger (free-drag)
       :box {:dragging? false
             :top nil
             :left nil}})
    IStep
    (get-owner [this] owner)
    (get-signal [this] z)
    (step [this state [tag e]]
      (let [{:keys [box dragger]} state]
        (assoc state :box
          (case tag
            :drag/start (drag-start dragger box e)
            :drag/move (drag-move dragger box e)
            :drag/stop (drag-stop dragger box e)))))
    om/IRenderState
    (render-state [this {:keys [box]}]
      (dom/div #js {:style #js {:position "absolute"
                                :userSelect "none"
                                :zIndex 1
                                :top (:top box)
                                :left (:left box)}
                    :onMouseDown
                    (fn [e]
                      (when (element-inside? drag-class (.-target e))
                        (install-move! owner
                          #(handle this [:drag/move {:pos (event->pos %)}]) )
                        (install-up! owner
                          #(do (uninstall! owner)
                               (handle this [:drag/stop {:pos (event->pos %)}])))
                        (handle this [:drag/start {:node (om/get-node owner)
                                                   :pos (event->pos e)}])))}
        (om/build view item)))))

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

(defn wire! [component props address opts]
  (om/build component props opts))

(defn guidelines [items owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (let [boxes (map #(:box @(signal this [:draggable (:item-id %)])) items)]
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
                   (om/build draggable item 
                     {:opts {:drag-class "box"
                             :z (signal this [:draggable (:item-id item)]) 
                             :view render-box}}))
              items)))))))

(defn main [data owner]
  (om/component
    (dom/div #js {:style #js {:userSelect "none"}} 
      (om/build guidelines (:items data)))))

(om/root main app-state
  {:target (. js/document (getElementById "app"))})
