(defproject zortable "0.1.3-SNAPSHOT"
  :description "Reusable sortable om component"
  :url "https://github.com/bensu/zortable"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308" :scope "provided"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.9.0" :exclusions [cljsjs/react]]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-doo "0.1.4-SNAPSHOT"]
            [lein-figwheel "0.3.7"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :profiles {:dev {:dependencies [[cljs-react-test "0.1.3-SNAPSHOT"]
                                  [cljsjs/react-with-addons "0.13.3-0"]]}}

  :figwheel {:css-dirs ["resources/public/css"]}

  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/testable.js"
                :main "test.runner"
                :cache-analysis true
                :optimizations :whitespace}}
    {:id "boxes"
     :source-paths ["src" "examples/boxes"]
     :figwheel { :on-jsload "boxes.core/on-js-reload" }
     :compiler {:main boxes.core 
                :asset-path "js/compiled/boxes"
                :output-to "resources/public/js/compiled/main.js"
                :output-dir "resources/public/js/compiled/boxes"
                :source-map-timestamp true}}
    {:id "editable"
     :source-paths ["src" "examples/editable"]
     :figwheel { :on-jsload "editable.core/on-js-reload" }
     :compiler {:main editable.core 
                :asset-path "js/compiled/editable"
                :output-to "resources/public/js/compiled/main.js"
                :output-dir "resources/public/js/compiled/editable"
                :source-map-timestamp true}}
    {:id "click-out"
     :source-paths ["src" "examples/click_out"]
     :figwheel { :on-jsload "click-out.core/on-js-reload" }
     :compiler {:main click-out.core 
                :asset-path "js/compiled/click_out"
                :output-to "resources/public/js/compiled/main.js"
                :output-dir "resources/public/js/compiled/click_out"
                :source-map-timestamp true}}
    {:id "draggable"
     :source-paths ["src" "examples/draggable"]
     :figwheel { :on-jsload "draggable.core/on-js-reload" }
     :compiler {:main draggable.core 
                :asset-path "js/compiled/draggable"
                :output-to "resources/public/js/compiled/main.js"
                :output-dir "resources/public/js/compiled/draggable"
                :source-map-timestamp true}}]})
