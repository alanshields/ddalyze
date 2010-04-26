(ns ddalyze.search
  "Pathing and analysis functions"
  (:use clojure.core
        [clojure.contrib.combinatorics :only (cartesian-product)]
        [clojure.contrib.greatest-least :only (least-by)]
        ddalyze.binds
        ddalyze.map
        ddalyze.util
        ddalyze.parse))


(defn all-legal-ground-creep-moves
  ([pos map] (all-legal-ground-creep-moves pos creepable? map))
  ([pos match-fn map]
     (let [offset-block (fn [r c]
                          (block-at-offset pos r c map))
           creep-or-nil (fn [block]
                          (if (and block (match-fn block))
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
         (filter match-fn
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
                                 ul))))))))


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
             openset #{(pos-id from mapdata)}
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
                            (contains? openset (pos-id pos mapdata)))]
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
                           (conj openset (pos-id y mapdata))
                           (assoc! came-from (vp y) x)
                           (assoc! g-score (vp y) tentative-g-score)
                           (assoc! h-score (vp y) (estimated-path-distance y))
                           (assoc! f-score (vp y) (+ tentative-g-score
                                                    (estimated-path-distance y))))
                    (recur (rest add-queue) x closedset openset came-from g-score h-score f-score)))))
            ; generate next add-queue
            (if (empty? openset)
              nil
              (let [x (block-at (first (sort-by #(f-score %1) openset)) mapdata)]
                (if (pos= x goal)
                  (reconstruct-path x)
                  (recur (legal-moves-for-pos-fn x)
                         x
                         (conj closedset (pos-id x mapdata))
                         (disj openset (pos-id x mapdata))
                         came-from g-score h-score f-score))))))))))

(def *max-generations* 100)
(def *descendants-from-each* 5)
(def *number-of-fit-to-keep* 50)
(def *max-generations-without-max-fitness-change* 5)
(def *epsilon* 0.01)

(defn genetic-search [state new-state-fn cost-fn cmp]
  "Finds best moves to apply to achieve best cost via genetic search
new-state-fn: (fn [oldstate]) -> returns a new state from the old state
cost-fn: (fn [state]) -> comparable via cmp
cmp: (fn [cost-a cost-b] -> boolean"
  (let [max-generations *max-generations*
        descendants-from-each *descendants-from-each*
        number-of-fit-to-keep *number-of-fit-to-keep*
        max-generations-without-max-fitness-change *max-generations-without-max-fitness-change*
        epsilon *epsilon*]
    (loop [cur-generation 1
           most-fit (list state)
           greatest-fitness (reduce max (map cost-fn most-fit))
           generations-with-this-fitness 1]
      (when *debug-output*
        (println "generation:" cur-generation "greatest-fitness:" (format "%.2f" (float greatest-fitness)) "count(most-fit)" (count most-fit) "map cost:" (map-price (first most-fit))))
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
                                            (distinct (mapcat (fn [state]
                                                                (map new-state-fn (repeat descendants-from-each state)))
                                                              most-fit)))
              new-greatest-fitness (reduce max (map cost-fn most-fit))]
          (recur (inc cur-generation)
                 most-fit
                 new-greatest-fitness
                 (if (< (Math/abs (- new-greatest-fitness greatest-fitness))
                        epsilon)
                   (inc generations-with-this-fitness)
                   1)))))))

(defn a*-path-along [start finish maptype mapdata]
  (a* start finish (fn [pos] (all-legal-ground-creep-moves pos #(is-type maptype %1) mapdata)) mapdata))

(defn a*-all-creep-paths [mapdata]
  (let [spawns (matching-blocks spawn? mapdata)
        exits (matching-blocks exit? mapdata)
        path (fn [start finish]
               (a* start finish #(all-legal-ground-creep-moves %1 mapdata) mapdata))]
    (let [all-paths (pmap (fn [[start finish]] (list start finish (path start finish)))
                          (remove (fn [[a b]] (or (adjacent? a b)
                                                  (a*-path-along a b 'spawnexit mapdata)))
                                  (cartesian-product spawns exits)))]
      (if (every? not-empty all-paths)
        (let [exit-paths-for-spawn (fn [spawn]
                                     (remove nil?
                                             (map (fn [[start finish path]]
                                                    (if (pos= start spawn)
                                                      path))
                                                  all-paths)))]
          (map (fn [spawn]
                 (apply least-by (cons count (exit-paths-for-spawn spawn))))
               spawns))))))
(defn a*-shortest-creep-path [mapdata cost-fn]
  "Confirm that every spawn can reach every exit, then return the shortest path"
  (first (take-n-greatest-by 1 cost-fn < (a*-all-creep-paths mapdata))))

(defn creep-path-cost [path mapdata]
  "Cost of moving along this path"
  (if (empty? path)
    0
    (reduce +
            (map (fn [towers]
                   (reduce + (map tower-damage towers)))
                 (map (fn [pos]
                        (towers-within-range pos mapdata))
                      path)))))

(defn update-map-path [mapinfo]
  (let [shortest-path (a*-shortest-creep-path mapinfo #(creep-path-cost %1 mapinfo))]
    (struct mapdata (:blocks mapinfo) (:row-count mapinfo) (:column-count mapinfo) shortest-path (creep-path-cost shortest-path mapinfo) (:towers mapinfo))))
(defn shortest-map-path [mapinfo]
  (let [mapinfo-with-path (if (nil? (:shortest-path mapinfo))
                            (update-map-path mapinfo)
                            mapinfo)]
    (:shortest-path mapinfo-with-path)))



(defn shortest-map-path-cost [mapinfo]
  (let [mapinfo-with-path-cost (if (nil? (:shortest-path-cost mapinfo))
                                 (update-map-path mapinfo)
                                 mapinfo)]
    (:shortest-path-cost mapinfo-with-path-cost)))

(defn draw-shortest-path [mapdata]
  (draw-path (shortest-map-path mapdata) mapdata))


;;;;;; Different analyses

(defn make-move [type & moreargs]
  (cons type moreargs))
(defn moves [& move-list]
  move-list)

(defn legal-tower-placements-reaching-path [mapdata path tower]
  (filter (fn [tower-pos]
            (room-for-tower? tower-pos mapdata))
          (distinct (apply concat
                           (map (fn [path-pos]
                                  (tower-placements-reaching-pos path-pos tower mapdata))
                                path)))))
(defn move-lists-for-state-with-different-towers [mapdata possible-towers]
  (mapcat (fn [tower-type]
            (let [tower (get-tower tower-type)]
              (map (fn [tower-pos]
                     (moves (make-move 'add tower tower-pos)))
                   (legal-tower-placements-reaching-path mapdata (shortest-map-path mapdata) tower))))
          possible-towers))

(defn move-lists-for-state-with-different-towers-and-replacement [mapdata budget]
  (mapcat (fn [remove-tower]
            (mapcat (fn [add-tower-type]
                      (let [add-tower (get-tower add-tower-type)]
                        (map (fn [add-tower-pos]
                               (if (nil? remove-tower)
                                 (moves (make-move 'add add-tower add-tower-pos))
                                 (moves (make-move 'drop remove-tower)
                                        (make-move 'add add-tower add-tower-pos))))
                             (let [legal-tower-placements (legal-tower-placements-reaching-path mapdata (shortest-map-path mapdata) add-tower)]
                               (if (nil? remove-tower)
                                 legal-tower-placements
                                 (cons (tower-position remove-tower) legal-tower-placements))))))
                    (if (nil? remove-tower)
                      (towers-at-price budget)
                      (towers-at-price (+ budget (tower-price remove-tower))))))
          (cons nil
                (towers-on-map mapdata))))
                

(defn apply-move-list [mapdata moves]
  {:pre [(is-map? mapdata)]
   :post [(is-map? %)]}
  (loop [moves moves
         mapdata mapdata]
    (if (empty? moves)
      (update-map-path mapdata)
      (let [[movetype & rest-of-move] (first moves)]
        (cond
         (= movetype 'add) (let [[tower pos] rest-of-move]
                             (recur (rest moves)
                                    (place-tower pos tower mapdata)))
         (= movetype 'drop) (let [[tower] rest-of-move]
                              (recur (rest moves)
                                     (remove-tower-at-pos (tower-position tower) mapdata)))
         true (throw (Exception. (format "Unknown movetype %s" movetype))))))))

(defn best-towers-for-budget-with-upgrades-fn [budget]
  (fn [current-map]
    (genetic-search current-map
                    (fn [state]
                      (let [tower (choose (towers-at-price (- budget (map-price state))))
                            path-pos (choose (shortest-map-path state))]
                        (if (and tower path-pos)
                          (let [placement (choose (tower-placements-reaching-pos path-pos (get-tower tower) state))]
                            (if placement
                              (place-tower placement (get-tower tower) state)
                              state))
                          state)))
                    shortest-map-path-cost
                    >)))

(defn best-towers-for-budget-fn [budget]
  (fn [current-map]
    (genetic-search current-map
                    (fn [state]
                      (let [tower (choose (towers-at-price (- budget (map-price state))))
                            path-pos (choose (shortest-map-path state))]
                        (if (and tower path-pos)
                          (let [placement (choose (tower-placements-reaching-pos path-pos (get-tower tower) state))]
                            (if placement
                              (place-tower placement (get-tower tower) state)
                              state))
                          state)))
                    shortest-map-path-cost
                    >)))
;; (defn best-towers-for-price-with-replacement-fn [price]
;;   (fn [current-map]
;;     (let [moves-for-state-fn (fn [mapdata]
;;                                (move-lists-for-state-with-different-towers-and-replacement mapdata (- price (map-price mapdata))))
;;           apply-move-fn apply-move-list
;;           legal-state? (fn [map]
;;                          (and (not (empty? (shortest-map-path map)))
;;                               (<= (map-price map) price)))
;;           cost-fn (fn [map]
;;                     (shortest-map-path-cost map))]
;;       (genetic-search current-map moves-for-state-fn apply-move-fn legal-state? cost-fn >))))

;; (defn best-towers-for-price-fn [price]
;;   (fn [current-map]
;;     (let [moves-for-state-fn (fn [mapdata]
;;                                (move-lists-for-state-with-different-towers mapdata (towers-at-price (- price (map-price mapdata)))))
;;           apply-move-fn apply-move-list
;;           legal-state? (fn [map]
;;                          (and (not (empty? (shortest-map-path map)))
;;                               (<= (map-price map) price)))
;;           cost-fn (fn [map]
;;                     (shortest-map-path-cost map))]
;;       (genetic-search current-map moves-for-state-fn apply-move-fn legal-state? cost-fn >))))

;; (defn best-towers-for-creeps [current-map]
;;   ((best-towers-for-price-fn 100000) current-map))

