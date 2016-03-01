(set-env!
 :source-paths #{"src"}
 :resource-paths #{"html" "resources"}

 :dependencies '[;; Boot setup
                 [adzerk/boot-cljs "1.7.228-1"]
                 [adzerk/boot-reload "0.4.4"]
                 [pandeiro/boot-http "0.7.0"]
                 [adzerk/boot-cljs-repl "0.3.0"]
                 [org.martinklepsch/boot-garden "1.3.0-0"]
                 [danielsz/boot-autoprefixer "0.0.7"]

                 ;; bREPL dependencies
                 [com.cemerick/piggieback "0.2.1"]
                 [weasel "0.7.0"]
                 [org.clojure/tools.nrepl "0.2.12"]

                 ;; Dev helpers
                 [binaryage/devtools "0.5.2"]
                 [devcards "0.2.1-6"]
                 [devcards-om-next "0.1.1"]
                 ;; om-i ?

                 ;; App dependencies
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [org.clojure/core.async "0.2.374"]

                 [org.omcljs/om "1.0.0-alpha30"]
                 [sablono "0.6.0"]
                 [garden "1.3.2"]
                 [facjure/mesh "0.4.0"]

                 [datascript "0.15.0"]
                 [funcool/tubax "0.2.0"]
                 [com.rpl/specter "0.9.2"]])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[adzerk.boot-reload :refer [reload]]
         '[pandeiro.boot-http :refer [serve]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[org.martinklepsch.boot-garden :refer [garden]]
         '[danielsz.autoprefixer :refer [autoprefixer]])

(deftask dev []
  (comp
   (watch)
   (reload :on-jsload 'digglepedia.web.core/reload)
   (garden :pretty-print true
           :output-to "css/style.css"
           :styles-var 'digglepedia.web.styles/index)
   (autoprefixer :files ["style.css"])
   (cljs-repl)
   (cljs :source-map true
         :compiler-options {:devcards true
                            :parallel-build true}
         :optimizations :none)
   (target :dir #{"target"})
   (serve :dir "target" :port 4000)))

;; TODO: need to fix some issue with
;;       :advanced compilation and (xml->clj)
;; (deftask prod []
;;   (comp
;;    (watch)
;;    (garden :pretty-print false
;;            :output-to "css/style.css"
;;            :styles-var 'digglepedia.web.styles/index)
;;    (autoprefixer :files ["style.css"])
;;    (cljs :source-map true
;;          :compiler-options {:devcards true
;;                             :parallel-build true}
;;          :optimizations :advanced)
;;    (target :dir #{"target"})
;;    (serve :dir "target" :port 4000)))
