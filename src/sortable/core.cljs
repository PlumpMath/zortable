(ns ^:figwheel-always sortable.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [goog.style :as style]
              [jamesmacaulay.zelkova.signal :as z]
              [jamesmacaulay.zelkova.mouse :as mouse]))

(enable-console-print!)

(defn pos->hue
  [[x y]]
  (mod (+ (/ x 2) (/ y 2)) 360))

(def box-width 100)
(def box-height 100)

(defn box-center [box]
  (letfn [(add [kb ko]
            (+ (kb box) (/ (ko box) 2)))]
    [(add :left :width) (add :top :height)]))

(defn build-box
  [id top left]
  {:id id 
   :top top
   :left left
   :width box-width
   :height box-height 
   :hue (pos->hue [top left])})

(def init-state {:boxes (mapv #(build-box % (* % box-height) 0) (range 5))
                 :drag nil})

(defn in-box?
  [[x y]
   {:keys [top left width height]}]
  (and (< left x (+ left width))
       (< top y (+ top height))))

(defn moving?
  [box]
  (contains? box :drag-offset))

(defn click
  [pos state]
  (let [without-clicked (remove (partial in-box? pos))]
    (update-in state [:boxes] (partial into [] without-clicked))))

(defn topleft-pos
  [{:keys [left top]}]
  [left top])

(defn locate-box [box]
  (let [node (.getElementById js/document (:id box))
        final-pos (style/getPageOffset node)
        left (.-x final-pos)
        top (.-y final-pos)]
    (assoc box :left left :top top)))

(defn start-dragging-box-from-pos
  [pos box]
  (let [offset (->> box topleft-pos (map - pos))]
    (assoc box :drag-offset offset)))

(defn start-drag
  "Identifies the boxes to be dragged (or build) and returns the updated state"
  [pos state]
  (let [drag-target? (partial in-box? pos)]
    (-> state
      (update-in [:boxes]
        (fn [boxes]
          (->> (map locate-box boxes)
            (mapv #(cond->> %
                     (drag-target? %) (start-dragging-box-from-pos pos))))))
      (assoc :drag {:start-pos pos}))))

(defn drag
  "Updates the state by interpreting what the new position means for each box."
  [pos state]
  (letfn [(drag-to-pos [box]
            (let [[left top] (map - pos (:drag-offset box))]
              (assoc box :left left :top top)))]
    (update-in state [:boxes]
      (fn [boxes]
        (mapv #(if (moving? %) (drag-to-pos %) %) boxes)))))

(defn drop-boxes
  [state]
  (letfn [(drop-box [box]
            (dissoc box :drag-offset :top :left))]
    (update-in state [:boxes] (partial into [] (map drop-box)))))

(defn stop-drag
  [state]
  (-> state
    (update-in [:boxes] #(->> (map locate-box %)
                           (sort-by (comp second box-center))
                           vec ))
    drop-boxes
    (assoc :drag nil)))

(defrecord NoOp [] IFn (-invoke [_ state] state))
(defrecord Click [pos] IFn (-invoke [_ state] (click pos state)))
(defrecord StartDrag [pos] IFn (-invoke [_ state] (start-drag pos state)))
(defrecord Drag [pos] IFn (-invoke [_ state] (drag pos state)))
(defrecord StopDrag [] IFn (-invoke [_ state] (stop-drag state)))

(def state-signal
  (let [dragging-positions (z/keep-when mouse/down?
                                        [0 0]
                                        mouse/position)
        dragging? (->> (z/constant true)
                       (z/sample-on dragging-positions)
                       (z/merge (z/keep-if not false mouse/down?))
                       (z/drop-repeats))
        dragstarts (z/keep-if identity true dragging?)
        dragstops (z/keep-if not false dragging?)
        click-positions (->> mouse/position
                          (z/sample-on mouse/clicks)
                          (z/drop-when dragging? [0 0]))
        actions (z/merge (z/constant (->NoOp))
                         (z/map ->StartDrag (z/sample-on dragstarts mouse/position))
                         (z/map (constantly (->StopDrag)) dragstops)
                         (z/map ->Drag dragging-positions)
                         (z/map ->Click click-positions))]
    (z/foldp (fn [action state]
               (assoc (action state)
                 :last-action (pr-str action)))
             init-state
             actions)))

(defonce app-state (z/pipe-to-atom state-signal))

(defn box-color
  [box]
  (let [opacity (if (moving? box)  0.5 1)]
    (str "hsla(" (:hue box) ",50%,50%," opacity ")")))

(defn render-box
  [box]
  (when box
    (dom/div
      #js {:id (:id box)
           :class "sortable-container"
           :style (clj->js (cond-> {:backgroundColor (box-color box)
                                    :position "relative"
                                    :width (:width box)
                                    :height (:height box)}
                             (moving? box) (merge {:position "absolute"
                                                   :top (:top box)
                                                   :left (:left box)})))}
      (:id box))))

(defn render-state
  [state]
  (dom/div #js {:style #js {:-webkit-touch-callout "none"
                            :-webkit-user-select "none"
                            :-khtml-user-select "none"
                            :-moz-user-select "none"
                            :-ms-user-select "none"
                            :user-select "none"}}
    (apply dom/div nil
      (map render-box (:boxes state)))
    (dom/div #js {:style #js {:position "relative"}}
      (dom/h1 nil "Drag and drop")
      (dom/pre nil (.stringify js/JSON (clj->js state) nil 2)))))

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

