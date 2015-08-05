(ns zortable.core
  (:require [goog.object :as gobj]
            [om.core :as om :include-macros true]))

;; ====================================================================== 
;; Wiring

(defprotocol IWire
  "Internal protocol used to pass signals under the covers"
  (get-owner [this])
  (get-signal [this])
  (get-source [this]))

;; I choose how to raise! my state after each step 
(defprotocol IMontior
  (monitor [this state' event]))

;; Make sideffects?
(defprotocol IHandle
  (handle! [this state' event]))

(defprotocol IStep
  (step [this state event]))

(defrecord Signal [route state]
  IDeref
  (-deref [_] (second (-deref state)))
  IReset
  (-reset! [_ v] (reset! state v))
  IWatchable
  (-notify-watches [_ oldval newval]
    (-notify-watches state oldval newval))
  (-add-watch [_ k f]
    (-add-watch state k f))
  (-remove-watch [_ k]
    (-remove-watch state k)))

(defn handle [this action]
  {:pre [(satisfies? IStep this) (satisfies? IWire this)]}
  (when (nil? (get-source this))
    (let [owner (get-owner this)
          state (om/get-state owner)
          state' (step this state action)]
      (when (and (some? state') (not= state state'))
        (om/set-state! owner state')
        (when-let [signal (get-signal this)]
          ;; TODO: how do we compose events?
          ;; Maybe should formalize that action [tag data] and replace
          ;; :z/step with tag
          (reset! signal [:z/step state']))))))

(defn raise! [this action]
  {:pre [(satisfies? IWire this)]}
  (reset! (get-signal this) action))

(defn signal-map [owner]
  (gobj/get owner "z$signals"))

(defn add-signal! [owner k z]
  (gobj/set owner "z$signals" (merge (signal-map owner) {k z})))

(defn signal [this route]
  (let [owner (get-owner this)]
    (or (get (signal-map owner) route)
      (let [z (Signal. route (atom {}))]
        (add-watch z route (fn [_ _ _ state']
                             (om/refresh! owner)
                             (when (satisfies? IStep this)
                               (handle this [route state']))))
        (add-signal! owner route z)
        z))))
