(defproject zortable "0.1.0-SNAPSHOT"
  :description "Reusable sortable om component using Zelkova"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.8.8"]
                 [jamesmacaulay/zelkova "0.4.0"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.3"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "examples/boxes"]
              :figwheel { :on-jsload "boxes.core/on-js-reload" }
              :compiler {:main boxes.core 
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/sortable.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map-timestamp true }}]}

  :figwheel {:css-dirs ["resources/public/css"]})
