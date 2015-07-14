(ns test.zortable.editable
  (:require [cljs.test :refer-macros [deftest testing is use-fixtures async]]
            [cljs.core.async :as async :refer (<! >! chan put!)]
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

(defn add-node
  "Add button for adding a new list element"
  [last-sort owner {:keys [edit-ch]}]
  (om/component
    (dom/div (clj->js {:className "addnode"
                       :style {:cursor "pointer"
                               :font-size "2em"}
                       :onClick #(put! edit-ch [:enter last-sort])}) "+")))

(defn root-wrapper [data owner]
  (reify
    om/IRender
    (render [_]
      (om/build e/list-maker data {:opts {:id-key :id
                                          :val-key :val
                                          :add-node add-node}}))))

(deftest list-maker
  (async done
    (let [c (tu/new-container!)
          sort (mapv :id xs)
          state (atom {:items (zipmap sort xs) 
                       :sort sort})
          rt (om/root root-wrapper state {:target c})
          input-vec (tu/find-by-tag rt "input")
          add-node (tu/find-one-by-class rt "addnode")
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
                (is (= (inc (count xs)) (count (:items @state))))
                (tu/unmount! c)
                (done)))
          (sim/focus last-node nil)
          (sim/key-down last-node {:key "Enter"})))
      #_(testing "On plus it adds a new empty editable"
        ;; Add a watch to state to input the last value only after
        ;; "Enter" had an effect. Currently the watch catches all
        ;; changes in :xs, but that might need to change if more tests
        ;; are added here.
        (let [watch-ch2 (chan)]
          (add-watch state :xs #(go (>! watch-ch2 :update)))
          (go (let [_ (<! watch-ch2)]
                (sim/input (last input-nodes) "hej")
                ;(println (dom/render-to-str add-node))
                #_(sim/click add-node nil)
                (om/render-all rt)
                ;(println #_(.stringify js/JSON) (.-innerHTML (om/get-node rt)))
                (println "### HEJ " (:items @state))
                (is (= (inc (count xs)) (count (:items @state))))
                (tu/unmount! c)
                (done)))
          (sim/click add-node nil))))))
