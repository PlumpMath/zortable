(ns test.zortable.editable
  (:require [cljs.test :refer-macros [deftest testing is use-fixtures async]]
            [cljs.core.async :as async :refer (<! >! chan)]
            [cljs-react-test.simulate :as sim]
            [cljs-react-test.utils :as tu]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [zortable.editable :as e])
  (:require-macros [cljs.core.async.macros :as async :refer (go go-loop)]))

(enable-console-print!)

;; ====================================================================== 
;; Fixtures

(def ^:dynamic c nil)

;; Even if with-container will be used in every test ns, it seems like
;; a bad idea to have it reference always `c`. Maybe high order fn
;; that takes a symbol?
(defn with-container [test-fn]
  (binding [c (tu/new-container!)]
    (test-fn)
    (tu/unmount! c)))

(use-fixtures :each with-container)

;; ====================================================================== 
;; Tests

(deftest list-maker
  (let [list [{:db/id 1 :list/value "Red"}
              {:db/id 2 :list/value "Yellow"}]
        state (atom {:xs list 
                     :kork :list/value})
        rt (om/root e/list-maker state {:target c})
        input-vec (tu/find-by-tag rt "input")
        input-nodes (mapv om/get-node input-vec)]
    (testing "Initially displays"
      (testing "correct number of list elements"
        (is (= (count input-vec) (count list))))
      (testing "correct list element values"
        (is (= (map :list/value list) (map #(.-value %) input-nodes)))))
    (testing "State changes on input"
      (let [new-val "Blue"]
        (sim/change (first input-vec) {:target {:value new-val}})
        (is (= new-val (get-in @state [:xs 0 :list/value])))))
    (testing "On enter it adds a new empty editable"
      ;; Add a watch to state to input the last value only after
      ;; "Enter" had an effect. Currently the watch catches all
      ;; changes in :xs, but that might need to change if more tests
      ;; are added here.
      (let [last-node (last input-vec)
            last-val "Green"
            watch-ch (chan)]
        (add-watch state :xs #(go (>! watch-ch :update)))
        (sim/focus last-node nil)
        #_(sim/key-down last-node "Enter")
        #_(go (do (<! watch-ch)
                (sim/input (last input-nodes) last-val)
                (is (= (inc (count list)) (count (:xs @state))))
                (println "FIX: Not rerendering in list-maker test"
                         (count input-nodes))))))))
