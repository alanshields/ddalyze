(ns ddalyze.core
  "Namespace for DDalyze"
  (:use clojure.core
        ddalyze.binds
        ddalyze.map
        ddalyze.parse
        ddalyze.search
        ddalyze.util))

(def *simple-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")
(def *simple-map* (map-from-map-file *simple-map-file*))

(defn analyze-map-file [file]
  (let [current-map (map-from-map-file file)
        best-towers-map (best-towers-for-creeps current-map)]
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