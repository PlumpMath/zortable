(ns zortable.util
  (:import [goog.ui IdGenerator]))

(defn find-index [v coll]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll)) 
            (= v (last coll)))
      i)))

(defn insert-at [n v xs]
  (if (< n (count xs))
    (into (conj (subvec xs 0 n) v) (subvec xs n))
    (conj xs v)))

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))
