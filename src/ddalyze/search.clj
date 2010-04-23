(ns ddalyze.search
  "Pathing and analysis functions"
  (:use clojure.core
        [clojure.contrib.combinatorics :only (cartesian-product)]
        ddalyze.binds
        ddalyze.map
        ddalyze.util
        ddalyze.parse))

(defn all-legal-ground-creep-moves [pos map]
  (let [offset-block (fn [r c]
                       (block-at-offset pos r c map))
        creep-or-nil (fn [block]
                       (if (and block (creepable? block))
                         block
                         nil))]
    (let [ur (offset-block 1 1)
          lr (offset-block -1 1)
          ll (offset-block -1 -1)
          ul (offset-block 1 -1)
          u  (offset-block 1 0)
          r  (offset-block 0 1)
          d  (offset-block -1 0)
          l  (offset-block 0 -1)
          u? (creep-or-nil u)
          r? (creep-or-nil r)
          d? (creep-or-nil d)
          l? (creep-or-nil l)]
      (filter creepable?
              (remove nil?
                      (list u
                            r
                            d
                            l
                            (if (and u? r?)
                              ur)
                            (if (and r? d?)
                              lr)
                            (if (and d? l?)
                              ll)
                            (if (and l? u?)
                              ul)))))))


(defn a* [from goal legal-moves-for-pos-fn mapdata]
  "f(x) = g(x) + h(x)
g is current path-cost
h is estimated future path-cost

Uses terminology and pseudocode from Wikipedia:
http://en.wikipedia.org/wiki/A*_search_algorithm"
  (let [number-of-rows (+ 1 (apply max (block-map :row mapdata)))
        number-of-columns (+ 1 (apply max (block-map :column mapdata)))
        vecsize (* number-of-columns number-of-rows)
        ; v: convert 1-based (r,c) to vector position
        v (fn [r c] (+ (* number-of-columns r) c))
        vp (fn [pos] (v (:row pos) (:column pos)))
        estimated-path-distance (fn [pos] (crow-distance pos goal))]
    (let [f-score (transient (vec (take vecsize (repeat nil))))
          g-score (transient (vec (take vecsize (repeat nil))))
          h-score (transient (vec (take vecsize (repeat nil))))
          came-from (transient (vec (take vecsize (repeat nil))))]
      (loop [add-queue '() ; queue of adjacent positions to x (implements the "for next adjacent")
             x nil
             closedset #{}
             openset (list from)
             came-from came-from
             g-score (assoc! g-score (vp from) 0)
             h-score (assoc! h-score (vp from) (estimated-path-distance from))
             f-score (assoc! f-score (vp from) (estimated-path-distance from))]
        (let [reconstruct-path (fn [pos]
                                 (loop [pos pos
                                        accum '()]
                                   (if-let [from (get came-from (vp pos))]
                                     (recur from (cons pos accum))
                                     (cons pos accum))))
              in-closedset? (fn [pos]
                              (contains? closedset (pos-id pos mapdata)))
              in-openset? (fn [pos]
                            (some #(pos= pos %1) openset))]
          (if (not (empty? add-queue))
            (let [y (first add-queue)]
              (if (in-closedset? y)
                (recur (rest add-queue) x closedset openset came-from g-score h-score f-score)
                (let [tentative-g-score (+ (get g-score (vp x))
                                           (crow-distance x y))]
                  (if (or (not (in-openset? y))
                          (< tentative-g-score (get g-score (vp y))))
                    (recur (rest add-queue) x
                           closedset
                           (if (in-openset? y)
                             openset
                             (cons y openset))
                           (assoc! came-from (vp y) x)
                           (assoc! g-score (vp y) tentative-g-score)
                           (assoc! h-score (vp y) (estimated-path-distance y))
                           (assoc! f-score (vp y) (+ tentative-g-score
                                                    (estimated-path-distance y))))
                    (recur (rest add-queue) x closedset openset came-from g-score h-score f-score)))))
            ; generate next add-queue
            (if (empty? openset)
              nil
              (let [openset (sort-by #(f-score (vp %1)) openset)
                    x (first openset)]
                (if (pos= x goal)
                  (reconstruct-path x)
                  (recur (legal-moves-for-pos-fn x) x
                         (conj closedset (pos-id x mapdata)) (rest openset)
                         came-from g-score h-score f-score))))))))))

(defn genetic-search [state moves-for-state-fn apply-move-fn legal-move? legal-state? cost-fn cmp]
  "Finds best moves to apply to achieve best cost via genetic search
moves-for-state-fn: (fn [state]) -> list of 
apply-move-fn: (fn [state move]) -> state
legal-move?: (fn [state move]) -> bool
legal-state?: (fn [state]) -> bool
cost-fn: (fn [state]) -> comparable via cmp
cmp: (fn [cost-a cost-b] -> boolean"
  (let [max-generations 100
        descendants-from-each 10
        number-of-fit-to-keep 20
        max-generations-without-max-fitness-change 5
        epsilon 0.01]
    (loop [cur-generation 1
           most-fit (list state)
           greatest-fitness (reduce max (map cost-fn most-fit))
           generations-with-this-fitness 1]
      (when *debug-output*
        (println "generation:" cur-generation "greatest-fitness:" (format "%.2f" (float greatest-fitness)) "count(most-fit)" (count most-fit)))
      (when *ddebug-output*
        (let [top (take-n-greatest-by 2 cost-fn most-fit)]
          (if (< 1 (count top))
            (let [[a b] top]
              (println (format "top 2: %.2f vs %.2f" (float (cost-fn a)) (float (cost-fn b))))
              (println (show-map-compares a b)))
            (do
              (println (format "top 1: %.2f" (float (cost-fn (first top)))))
              (println (map-to-string (first top)))))))
      (if (or (>= cur-generation max-generations)
              (>= generations-with-this-fitness max-generations-without-max-fitness-change))
        (first (take-n-greatest-by 1 cost-fn most-fit))
        (let [most-fit (ptake-n-greatest-by number-of-fit-to-keep cost-fn
                                            (distinct (apply concat
                                                             (map (fn [state]
                                                                    (cons state
                                                                          (remove nil? (map (fn [move]
                                                                                              (if (legal-move? state move)
                                                                                                (let [result-state (apply-move-fn state move)]
                                                                                                  (if (legal-state? result-state)
                                                                                                    result-state))))
                                                                                            (choose descendants-from-each (moves-for-state-fn state))))))
                                                                  most-fit))))
              new-greatest-fitness (reduce max (map cost-fn most-fit))]
          (recur (inc cur-generation)
                 most-fit
                 new-greatest-fitness
                 (if (< (Math/abs (- new-greatest-fitness greatest-fitness))
                        epsilon)
                   (inc generations-with-this-fitness)
                   1)))))))

(defn a*-shortest-creep-path [mapdata]
  "Confirm that every spawn can reach every exit, then return the shortest path"
  (let [spawns (matching-blocks spawn? mapdata)
        exits (matching-blocks exit? mapdata)
        path (fn [start finish]
               (a* start finish #(all-legal-ground-creep-moves %1 mapdata) mapdata))]
    (let [all-paths (pmap (fn [[start finish]] (path start finish))
                          (remove (fn [[a b]] (adjacent? a b))
                                  (cartesian-product spawns exits)))]
      (if (every? not-empty all-paths)
        (first (take-n-greatest-by 1 count < all-paths))))))

(defn update-map-path [mapinfo]
  (let [shortest-path (a*-shortest-creep-path mapinfo)]
    (struct mapdata (:blocks mapinfo) (:row-count mapinfo) (:column-count mapinfo) shortest-path (creep-path-cost shortest-path) (:towers mapinfo))))
(defn shortest-map-path [mapinfo]
  (let [mapinfo-with-path (if (nil? (:shortest-path mapinfo))
                            (update-map-path mapinfo)
                            mapinfo)]
    (:shortest-path mapinfo-with-path)))

(defn creep-path-cost [path]
  "Cost of moving along this path"
  (if (empty? path)
    0
    (reduce +
            (map #(crow-distance %1 %2) path (rest path)))))

(defn shortest-map-path-cost [mapinfo]
  (let [mapinfo-with-path-cost (if (nil? (:shortest-path-cost mapinfo))
                                 (update-map-path mapinfo)
                                 mapinfo)]
    (:shortest-path-cost mapinfo-with-path-cost)))

(defn draw-shortest-path [mapdata]
  (draw-path (shortest-map-path mapdata) mapdata))

(defn best-towers-for-creeps [current-map]
  (genetic-search current-map
                  (fn [mapdata] (distinct (apply concat
                                                 (map #(tower-placements-covering-pos %1 mapdata)
                                                      (shortest-map-path mapdata)))))
                  (fn [map pos] (update-map-path (place-tower pos map)))
                  (fn [map pos] (room-for-tower? pos map))
                  (fn [map] (not (empty? (shortest-map-path map))))
                  (fn [map] (shortest-map-path-cost map))
                  >))