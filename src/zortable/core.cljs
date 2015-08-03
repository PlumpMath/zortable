(ns zortable.core
  (:require [goog.object :as gobj]
            [om.core :as om :include-macros true]))

;; ====================================================================== 
;; Wiring

(defprotocol IWire
  "Internal protocol used to pass signals under the covers"
  (get-owner [this])
  (get-signal [this]))

;; I choose how to raise! my state after each step 
(defprotocol IMontior
  (monitor [this state' event]))

;; Make sideffects?
(defprotocol IHandle
  (handle! [this state' event]))

(defprotocol IStep
  (step [this state event]))

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
  (gobj/get owner "z$signals"))

(defn add-signal! [owner k z]
  (gobj/set owner "z$signals" (merge (signal-map owner) {k z})))

(defn signal [this k]
  (let [owner (get-owner this)]
    (or (get (signal-map owner) k)
      (let [z (atom {})]
        (add-watch z k (fn [_ _ _ state']
                         (om/refresh! owner)
                         (when (satisfies? IStep this)
                           (handle this [k state']))))
        (add-signal! owner k z)
        z))))
