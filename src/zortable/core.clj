(ns zortable.core)

;; TODO: check for signals in bindings

(defmacro lets [bindings expr]
  (assert (vector? bindings) "The bindings should be a vector")
  (assert (even? (count bindings)) "The bindings should be even")
  (let [syms (mapv first (partition 2 bindings))
        signals (mapv second (partition 2 bindings))
        indexed-signals (vec (map-indexed vector signals))]
    `(let [f# (fn ~syms
                ~expr)
           z# (atom {})]
       (doseq [[i# s#] ~indexed-signals]
         (add-watch s# (gensym)
           (fn [~'_ ~'_ ~'_ state#]
             (reset! z# [(first state#) (apply f# (assoc (mapv deref ~signals) i# (second state#)))]))))
       z#)))
