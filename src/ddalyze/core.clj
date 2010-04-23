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

