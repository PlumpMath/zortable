(defproject zortable "0.1.2-SNAPSHOT"
  :description "Reusable sortable om component using Zelkova"
  :url "https://github.com/bensu/zortable"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308" :scope "provided"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.9.0" :exclusions [cljsjs/react]]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-doo "0.1.2-SNAPSHOT"]
            [lein-figwheel "0.3.2"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :profiles {:dev {:dependencies [[cljs-react-test "0.1.3-SNAPSHOT"]
                                  [cljsjs/react-with-addons "0.13.3-0"]]}}

  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/testable.js"
                :main test.runner 
                :cache-analysis true
                :optimizations :whitespace}}
    {:id "boxes"
     :source-paths ["src" "examples/boxes"]
     :figwheel { :on-jsload "boxes.core/on-js-reload" }
     :compiler
     {:main boxes.core 
      :asset-path "js/compiled/boxes"
      :output-to "resources/public/js/compiled/main.js"
      :output-dir "resources/public/js/compiled/boxes"
      :source-map-timestamp true}}
    {:id "editable"
     :source-paths ["src" "examples/editable"]
     :figwheel { :on-jsload "editable.core/on-js-reload" }
     :compiler
     {:main editable.core 
      :asset-path "js/compiled/editable"
      :output-to "resources/public/js/compiled/main.js"
      :output-dir "resources/public/js/compiled/editable"
      :source-map-timestamp true}}]}

  :figwheel {:css-dirs ["resources/public/css"]})
