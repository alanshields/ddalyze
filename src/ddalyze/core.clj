(ns ddalyze.core
  "Namespace for DDalyze"
  (:use clojure.core
        ddalyze.binds
        ddalyze.map
        ddalyze.parse
        ddalyze.search
        ddalyze.util
        [clojure.contrib.combinatorics :only (cartesian-product)]
        [incanter.stats :only (sd)]))

(def *simple-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")
(def *simple-map* (map-from-map-file *simple-map-file*))

(defn analyze-map-file [file method]
  (let [current-map (map-from-map-file file)
        best-towers-map (method current-map)]
    (when *debug-output*
      (println)(println)
      (println "Result cost: " (shortest-map-path-cost best-towers-map))
      (println (show-map-compares (draw-shortest-path best-towers-map) best-towers-map)))))

(defn compare-map-analyses [file method1 method2]
  (let [current-map (map-from-map-file file)
        best-towers1 (method1 current-map)
        best-towers2 (method2 current-map)]
    (println (format "Analyses done. Method1: %.2f vs Method2: %.2f" (float (shortest-map-path-cost best-towers1)) (float (shortest-map-path-cost best-towers2))))
    (println (show-map-compares best-towers1 best-towers2))))

(defn find-best-genetic-search-params [file method]
  (let [current-map (map-from-map-file file)
        score-to-params
        (map (fn [[max-generations descendants-from-each number-of-fit-to-keep :as params]]
               (binding [*ddebug-output* false
                         *max-generations* max-generations
                         *descendants-from-each* descendants-from-each
                         *number-of-fit-to-keep* number-of-fit-to-keep]
                 (println "trying max" max-generations "desc" descendants-from-each "number-of-fit" number-of-fit-to-keep)
                 (cons
                  (shortest-map-path-cost (method current-map))
                  params)))
             (cartesian-product '(50)
                                '(5 30)
                                '(20 100)))
        best-scores (sort-by #(first %1) > score-to-params)]
    (println "best:" (first best-scores))
    (println "next best:" (second best-scores))
    (println "sd:" (sd (map first best-scores)))
    (println best-scores)))
  