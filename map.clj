(ns ddalyze
  "Namespace for DDalyze"
  (:use clojure.core
        [clojure.contrib.duck-streams :only (read-lines write-lines)]
        [clojure.contrib.seq-utils :only (shuffle partition-all indexed)]
        clojure.contrib.greatest-least
        [clojure.contrib.combinatorics :only (cartesian-product)])
  (:import (java.io.File)))

(def *debug-output* true)
(def *ddebug-output* true)

;; Map block types
;;   "A map block is a square on the board. A tower takes up 4 map blocks."
(defstruct mapdata
  :blocks :row-count :column-count :shortest-path :shortest-path-cost)
(defstruct map-block
  :row :column :type)
(defstruct map-pos
  :row :column)

(defn is-pos? [p]
  (and (:row p)
       (:column p)))
(defn is-map? [m]
  (:blocks m))
(defn is-map-block? [b]
  (and (is-pos? b)
       (:type b)))

;; Block functions
(defn pos= [a b]
  "Determine if two blocks are positionally equal"
  (and (= (:row a) (:row b))
       (= (:column a) (:column b))))

(defn playable? [{map-type :type}]
  (= map-type 'playable))
(defn out-of-bounds? [{map-type :type}]
  (= map-type 'out-of-bounds))
(defn spawn? [{map-type :type}]
  (map-type #{'spawn 'spawnexit}))
(defn exit? [{map-type :type}]
  (map-type #{'exit 'spawnexit}))
(defn creepable? [{map-type :type}]
  "Creep can be on this block?"
  (map-type #{'playable 'exit 'path 'spawnexit}))

(defn is-pos [row col block]
  (and (= (:row block) row)
       (= (:column block) col)))
(defn is-type [type block]
  (= (:type block) type))
(defn pos-id
  "If two positions have the same pos-id and the same map, they are the same position.
Can also be used to do a fast lookup on a position via block-at"
  ([block map] (pos-id (:row block) (:column block) map))
  ([r c map] (+ (* (:column-count map) r)
                c)))
  

;; Map functions
(defn new-map
  ([blockdata] (new-map blockdata
                        (+ 1 (apply max (map :row blockdata)))
                        (+ 1 (apply max (map :column blockdata)))))
  ([blockdata row-count column-count]
     (struct mapdata (vec blockdata) row-count column-count)))
(defn set-pos-type [pos type mapdata]
  "For pos in the mapdata, set its type to TYPE"
  (new-map (map #(if (pos= pos %1)
                   (struct map-block (:row %1) (:column %1) type)
                   %1)
                (:blocks mapdata))))
(defn set-pos-types [poss type mapdata]
  (loop [mapdata mapdata
         [curpos & restpos :as poss] poss]
    (if (empty? poss)
      mapdata
      (recur (set-pos-type curpos type mapdata)
             restpos))))

(defn block-at
  ([row column map]
     (block-at (pos-id row column map) map))
  ([pos-id map]
     (if (contains? (:blocks map) pos-id)
       (get (:blocks map) pos-id))))
(defn block-at-offset [pos r c map]
  {:pre [(is-pos? pos)
         (number? r)
         (number? c)
         (is-map? map)]}
  (block-at (+ r (:row pos))
            (+ c (:column pos))
            map))
  
(defn matching-blocks [pred? map]
  (filter pred? (:blocks map)))
(defn block-map [fn mapdata]
  (map fn (:blocks mapdata)))

(defn order-map [map]
  (new-map 
   (sort
    (fn [a b]
      (let [row-comp (compare (:row a) (:row b))]
        (if (zero? row-comp)
          (compare (:column a) (:column b))
          row-comp)))
    (:blocks map))))

(defn row-map [map op]
  "Perform operation OP on each block per row, returning list rows of results"
  (loop [map (:blocks (order-map map))
         row -1
         currow nil
         acc '()]
    (if (empty? map)
      (if (nil? currow)
        (reverse acc)
        (reverse (cons (reverse currow) acc)))
      (let [[block & map-rest] map
            chr (op (:type block))]
        (if (not (= row (:row block)))
          (recur map-rest (:row block) (list chr) (if (nil? currow)
                                                    acc
                                                    (cons (reverse currow) acc)))
          (recur map-rest (:row block) (cons chr currow) acc))))))

; More interesting questions about maps
(defn streetwise-distance [from to]
  "Distance when a diagonal is counted as adjacent"
  (let [row-distance (Math/abs (- (:row from) (:row to)))
        column-distance (Math/abs (- (:column from) (:column to)))]
    (max row-distance column-distance)))
(defn crow-distance [from to]
  "Distance as the crow flies"
  (let [row-distance (- (:row from) (:row to))
        column-distance (- (:column from) (:column to))]
    (Math/sqrt (+ (* row-distance row-distance)
                  (* column-distance column-distance)))))

(defn adjacent?
  "Next to or on top of
Yes, I know adjacent usually doesn't mean congruent, but it helps a lot here"
  ([pos-a pos-b] (adjacent? pos-a pos-b streetwise-distance))
  ([pos-a pos-b distance-fn]
     (>= 1 (distance-fn pos-a pos-b))))

(defn room-for-tower? [pos mapdata]
  {:pre [(is-pos? pos)
         (is-map? mapdata)]}
  (let [b #(block-at-offset pos %1 %2 mapdata)]
    (every? #(and (not (nil? %1))
                  (playable? %1))
            (list (b 0 0) (b 0 1) (b 1 0) (b 1 1)))))
(defn place-tower
  "Will fail only if there is no room for the tower - tower will be allowed to block the path"
  ([pos mapdata] (let [result (place-tower pos mapdata 'fail)]
                   (if (= result 'fail)
                     (throw (Exception. (format "Could not place tower at %s" pos)))
                     result)))
  ([pos mapdata on-fail]
     {:pre [(is-pos? pos)
            (is-map? mapdata)]}
     (let [b #(block-at-offset pos %1 %2 mapdata)]
       (if (room-for-tower? pos mapdata)
         (set-pos-types (list (b 0 0) (b 0 1) (b 1 0) (b 1 1)) 'tower mapdata)
         on-fail))))
(defn tower-placements-covering-pos [pos mapdata]
  "Returns a list of legal tower placements that covers position pos"
  {:pre [(is-pos? pos)
         (is-map? mapdata)]}
  (let [possible-moves (map #(let [[r c] %1]
                               (block-at-offset pos r c mapdata))
                            '((0 0) (-1 0) (0 -1) (-1 -1)))]
    
    (filter #(and (not (nil? %1))
                  (room-for-tower? %1 mapdata))
            possible-moves)))

(defn blocks-within-distance
  ([pos distance map] (blocks-within-distance pos distance map crow-distance))
  ([pos distance map comparator] (matching-blocks #(<= (comparator pos %1) distance) map)))

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

;;;; Map IO Functions
(defn map-file-comment? [str]
  (= \; (first str)))

(def *map-char->-type*
     {\X 'out-of-bounds
      \space 'playable
      \I 'spawn
      \O 'exit
      \i 'spawnexit
      \a 'path
      \T 'tower})
(def *map-type->-char*
     (zipmap (vals *map-char->-type*)
             (keys *map-char->-type*)))
     
      
(defn map-file-char-to-type [chr]
  (if-let [type (get *map-char->-type* chr false)]
    type
    (throw (Exception. (format "Unknown map character \"%s\"." chr)))))
(defn map-type-to-file-char [type]
  (if-let [chr (get *map-type->-char* type false)]
    chr
    (throw (Exception. (format "Unknown type \"%s\"." type)))))
  

(defn map-from-map-file [file]
  (new-map
   (apply concat
          (map (fn [[line-number contents]]
                 (map (fn [[column-number column-type]]
                        (struct map-block line-number column-number column-type))
                      contents))
               (indexed
                (map indexed
                     (map #(map map-file-char-to-type %1)
                          (remove map-file-comment? (read-lines file)))))))))

(defn map-strings [mapdata]
  (map #(apply str %1)
       (row-map mapdata map-type-to-file-char)))
(defn map-to-string [mapdata]
  (let [lines (map-strings mapdata)]
    (apply str (interpose "\n" lines))))

(defn map-to-map-file [file mapdata]
  (write-lines file
               (map-strings mapdata)))

(defn show-map-compares [map-a map-b]
  (with-out-str
    (let [map-a-strings (map-strings map-a)
          map-b-strings (map-strings map-b)]
      (do
        (println (first map-a-strings) "  vs  " (first map-b-strings))
        (doseq [ln (map #(str %1 "        " %2)
                        (rest map-a-strings) (rest map-b-strings))]
          (println ln))))))

;; Search functions
(defn drop-first
  ([val coll] (drop-first val identity coll))
  ([val keyfn coll]
     (loop [accum '()
            queue coll]
       (if (or (empty? queue)
               (= val (keyfn (first queue))))
         (concat (reverse accum)
                 (rest queue))
         (recur (cons (first queue) accum)
                (rest queue))))))
(defn take-n-greatest-by
  ([n keyfn coll] (take-n-greatest-by n keyfn > coll))
  ([n keyfn cmp coll]
     (if (empty? coll)
       coll
       (let [vm (map #(list %1 (keyfn %1)) coll)
             cost #(second %1)]
         (loop [top-n (take n vm)
                lowest (reduce min (map cost top-n))
                queue (drop n vm)]
           (if (empty? queue)
             (map first top-n)
             (let [[next & queue] queue]
               (if (> n (count top-n))
                 (recur (cons next top-n)
                        (min (cost next) lowest)
                        queue)
                 (if (cmp (cost next) lowest)
                   (let [top-n (cons next (drop-first lowest cost top-n))]
                     (recur top-n
                            (reduce min (map cost top-n))
                            queue))
                   (recur top-n lowest queue))))))))))
(defn ptake-n-greatest-by
  "Parallel take-n-greatest-by"
  ([n keyfn coll] (ptake-n-greatest-by n keyfn > coll))
  ([n keyfn cmp coll]
     (if (> 4 n)
       (take-n-greatest-by n keyfn cmp coll) ; if it's fewer than 4 n, it's just not worth it.
       (let [coll-size (count coll)
             partition-size (int (/ coll-size 2))]
         (take-n-greatest-by n keyfn cmp
                             (reduce concat
                                     (pmap #(take-n-greatest-by n keyfn cmp %1)
                                           (partition-all partition-size coll))))))))
(defn choose [n coll]
  (take n (shuffle coll)))

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
                 
               
      

(def *simple-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")
(def *simple-map* (map-from-map-file *simple-map-file*))

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
(defn creep-path-cost [path]
  "Cost of moving along this path"
  (if (empty? path)
    0
    (reduce +
            (map #(crow-distance %1 %2) path (rest path)))))

(defn update-map-path [mapinfo]
  (let [shortest-path (a*-shortest-creep-path mapinfo)]
    (struct mapdata (:blocks mapinfo) (:row-count mapinfo) (:column-count mapinfo) shortest-path (creep-path-cost shortest-path))))
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

(defn draw-path [path mapdata]
  (set-pos-types path
                 'path
                 mapdata))
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

(defn analyze-map-file [file]
  (let [current-map (map-from-map-file file)
        best-towers-map (best-towers-for-creeps current-map)]
    (when *debug-output*
      (println)(println)
      (println "Result cost: " (shortest-map-path-cost best-towers-map))
      (println (show-map-compares (draw-shortest-path best-towers-map) best-towers-map)))))

;;(analyze-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")
;;(time (analyze-map-file "/Users/alanshields/code/desktop_defender/maps/basic2.map"))