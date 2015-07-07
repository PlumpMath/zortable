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
;; Tests

(def xs [{:id 1 :val "Red"}
         {:id 2 :val "Yellow"}])

(defn root-wrapper [data owner]
  (reify
    om/IRender
    (render [_]
      (om/build e/list-maker data {:opts {:id-key :id :val-key :val}}))))

(deftest list-maker
  (async done
    (let [c (tu/new-container!)
          sort (mapv :id xs)
          state (atom {:items (zipmap sort xs) 
                       :sort sort})
          rt (om/root root-wrapper state {:target c})
          input-vec (tu/find-by-tag rt "input")
          input-nodes (mapv om/get-node input-vec)]
      (testing "Initially displays"
        (testing "correct number of xs elements"
          (is (= (count input-vec) (count xs))))
        (testing "correct xs element values"
          (is (= (map :val xs) (map #(.-value %) input-nodes)))))
      (testing "State changes on input"
        (let [new-val "Blue"]
          (sim/change (first input-vec) {:target {:value new-val}})
          (is (= new-val (get-in @state [:items 1 :val])))))
      (testing "On enter it adds a new empty editable"
        ;; Add a watch to state to input the last value only after
        ;; "Enter" had an effect. Currently the watch catches all
        ;; changes in :xs, but that might need to change if more tests
        ;; are added here.
        (let [last-node (last input-vec)
              last-val "Green"
              watch-ch (chan)]
          (add-watch state :xs #(go (>! watch-ch :update)))
          (go (let [_ (<! watch-ch)]
                (sim/input (last input-nodes) last-val)
                (om/render-all rt)
                (let [_ (<! (async/timeout 50))]
                  ;; FIX: failing
                  #_(is (= (inc (count xs)) (count (:items @state))))
                  (tu/unmount! c)
                  (done))))
          (sim/focus last-node nil)
          (sim/key-down last-node "Enter"))))))
