(ns ddalyze
  "Namespace for DDalyze"
  (:use clojure.core
        [clojure.contrib.duck-streams :only (read-lines write-lines)]
        clojure.contrib.seq-utils)
  (:import (java.io.File)))

(def *debug-output* false)

;; Map block types
;;   "A map block is a square on the board. A tower takes up 4 map blocks."
(defstruct map-block
  :row :column :type)
(defstruct map-pos
  :row :column)

(defn pos= [a b]
  "Determine if two blocks are positionally equal"
  (and (= (:row a) (:row b))
       (= (:column a) (:column b))))

(defn playable? [{map-type :type}]
  (= map-type 'playable))
(defn out-of-bounds? [{map-type :type}]
  (= map-type 'out-of-bounds))
(defn spawn? [{map-type :type}]
  (= map-type 'spawn))
(defn creepable? [{map-type :type}]
  "Creep can be on this block?"
  (map-type #{'playable 'spawn 'path}))

(defn is-pos [row col block]
  (and (= (:row block) row)
       (= (:column block) col)))
(defn is-type [type block]
  (= (:type block) type))
(defn set-pos-type [pos type mapdata]
  "For pos in the mapdata, set its type to TYPE"
  (map #(if (pos= pos %1)
          (struct map-block (:row %1) (:column %1) type)
          %1)
       mapdata))
(defn set-pos-types [poss type mapdata]
  (loop [mapdata mapdata
         [curpos & restpos :as poss] poss]
    (if (empty? poss)
      mapdata
      (recur (set-pos-type curpos type mapdata)
             restpos))))

; More interesting questions about maps
(defn block-at [row column map]
  (first (filter #(is-pos row column %1)
                 map)))
(defn block-at-offset [pos r c map]
  (block-at (+ r (:row pos))
            (+ c (:column pos))
            map))
(defn matching-blocks [pred? map]
  (filter pred? map))

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
(defn creep-path-cost [path]
  "Cost of moving along this path"
  (reduce +
          (map #(crow-distance %1 %2) path (rest path))))

(defn room-for-tower? [pos mapdata]
  (let [b #(block-at-offset pos %1 %2 mapdata)]
    (every? playable? (list (b 0 0) (b 0 1) (b 1 0) (b 1 1)))))
(defn place-tower
  "Will fail only if there is no room for the tower - tower will be allowed to block the path"
  ([pos mapdata] (let [result (place-tower pos mapdata 'fail)]
                   (if (= result 'fail)
                     (throw (Exception. (format "Could not place tower at %s" pos)))
                     result)))
  ([pos mapdata on-fail]
     (let [b #(block-at-offset pos %1 %2 mapdata)]
       (if (room-for-tower? pos mapdata)
         (set-pos-types (list (b 0 0) (b 0 1) (b 1 0) (b 1 1)) 'tower mapdata)
         on-fail))))
(defn tower-placements-covering-pos [pos mapdata]
  "Returns a list of legal tower placements that covers position pos"
  (let [possible-moves (map #(let [[r c] %1]
                               (block-at-offset pos r c mapdata))
                            '((0 0) (-1 0) (0 -1) (-1 -1)))]
    (filter #(room-for-tower? %1 mapdata) possible-moves)))

(defn blocks-within-distance
  ([pos distance map] (blocks-within-distance pos distance map crow-distance))
  ([pos distance map comparator] (filter #(<= (comparator pos %1) distance) map)))

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
          l  (offset-block 0 -1)]
      (filter creepable?
              (remove nil?
                      (list u
                            r
                            d
                            l
                            (if (and (creep-or-nil u)
                                     (creep-or-nil r))
                              ur)
                            (if (and (creep-or-nil r)
                                     (creep-or-nil d))
                              lr)
                            (if (and (creep-or-nil d)
                                     (creep-or-nil l))
                              ll)
                            (if (and (creep-or-nil l)
                                     (creep-or-nil u))
                              ul)))))))

(defn a* [from goal legal-moves-for-pos-fn mapdata]
  "f(x) = g(x) + h(x)
g is current path-cost
h is estimated future path-cost

Uses terminology and pseudocode from Wikipedia:
http://en.wikipedia.org/wiki/A*_search_algorithm"
  (let [number-of-rows (+ 1 (apply max (map :row mapdata)))
        number-of-columns (+ 1 (apply max (map :column mapdata)))
        vecsize (* number-of-columns number-of-rows)
        ; v: convert 1-based (r,c) to vector position
        v (fn [r c] (+ (* number-of-columns r) c))
        vp (fn [pos] (v (:row pos) (:column pos)))
        estimated-path-distance (fn [pos] (crow-distance pos goal))]
    (let [f-score (vec (take vecsize (repeat nil)))
          g-score (vec (take vecsize (repeat nil)))
          h-score (vec (take vecsize (repeat nil)))]
      (loop [add-queue '() ; queue of adjacent positions to x (implements the "for next adjacent")
             x nil
             closedset '()
             openset (list from)
             came-from (vec (take vecsize (repeat nil)))
             g-score (assoc g-score (vp from) 0)
             h-score (assoc h-score (vp from) (estimated-path-distance from))
             f-score (assoc f-score (vp from) (estimated-path-distance from))]
        (let [reconstruct-path (fn [pos]
                                 (loop [pos pos
                                        accum '()]
                                   (if-let [from (get came-from (vp pos))]
                                     (recur from (cons pos accum))
                                     accum)))
              in-closedset? (fn [pos]
                             (some #(pos= pos %1) closedset))
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
                           (assoc came-from (vp y) x)
                           (assoc g-score (vp y) tentative-g-score)
                           (assoc h-score (vp y) (estimated-path-distance y))
                           (assoc f-score (vp y) (+ tentative-g-score
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
                         (cons x closedset) (rest openset)
                         came-from g-score h-score f-score))))))))))

(defn draw-path [mapdata]
  "Draw shortest path between two first goals a map"
  (set-pos-types (let [[start goal & _] (matching-blocks spawn? mapdata)]
                   (a* start goal
                       #(all-legal-ground-creep-moves %1 mapdata)
                       mapdata))
                 'path
                 mapdata))
      
;;;; Map IO Functions
(defn map-file-comment? [str]
  (= \; (first str)))

(def *map-char->-type*
     {\X 'out-of-bounds
      \space 'playable
      \O 'spawn
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
  (apply concat
         (map (fn [[line-number contents]]
                (map (fn [[column-number column-type]]
                       (struct map-block line-number column-number column-type))
                     contents))
              (indexed
               (map indexed
                    (map #(map map-file-char-to-type %1)
                         (remove map-file-comment? (read-lines file))))))))

(defn order-map [map]
  (sort
   (fn [a b]
     (let [row-comp (compare (:row a) (:row b))]
       (if (zero? row-comp)
         (compare (:column a) (:column b))
         row-comp)))
   map))

(defn row-map [map op]
  "Perform operation OP on each block per row, returning list rows of results"
  (loop [map (order-map map)
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
(defn exhaustive-search [state moves-for-state-fn apply-move-fn legal-move? legal-state? cost-fn cmp]
  "Finds best moves to apply to achieve best cost via exhaustive search
moves-for-state-fn: (fn [state]) -> list of 
apply-move-fn: (fn [state move]) -> state
legal-move?: (fn [state move]) -> bool
legal-state?: (fn [state]) -> bool
cost-fn: (fn [state]) -> comparable via cmp
cmp: (fn [cost-a cost-b] -> boolean"
  (loop [current-winner state
         current-winner-cost (cost-fn state)
         queue (list state)]
    (println "Q:" (count queue) "C:" current-winner-cost)
    (if (empty? queue)
      (do (println "winner:") (println (map-to-string (draw-path current-winner))) current-winner)
      (let [candidate (first queue)
            candidate-cost (cost-fn candidate)
            current-winner (do
                             (println "COMP:" candidate-cost "v" current-winner-cost)
                             (print (show-map-compares (draw-path candidate)
                                                       (draw-path current-winner)))
                             (println)
                             (if (cmp candidate-cost current-winner-cost)
                               candidate
                               current-winner))
            current-winner-cost (if (cmp candidate-cost current-winner-cost)
                                  candidate-cost
                                  current-winner-cost)]
        (println "current winner:")
        (println (map-to-string (draw-path current-winner)))
        (let [next-moves (moves-for-state-fn candidate)]
          (if (empty? next-moves)
            (recur current-winner current-winner-cost
                   (rest queue))
            (let [next-states (remove nil?
                                      (map (fn [move]
                                             (if (legal-move? candidate move)
                                               (let [result (apply-move-fn candidate move)]
                                                 (if (legal-state? result)
                                                   result))))
                                           next-moves))]
              (recur current-winner current-winner-cost
                     (concat (remove nil? next-states)
                             (rest queue))))))))))

(defn take-n-greatest-by
  ([n keyfn coll] (take-n-greatest-by n keyfn > coll))
  ([n keyfn cmp coll]
     (take n (sort-by keyfn cmp coll))))
(defn choose [n coll]
  (take n (shuffle coll)))

(defn genetic-search [state moves-for-state-fn apply-move-fn legal-move? legal-state? cost-fn cmp]
  "Finds best moves to apply to achieve best cost via exhaustive search
moves-for-state-fn: (fn [state]) -> list of 
apply-move-fn: (fn [state move]) -> state
legal-move?: (fn [state move]) -> bool
legal-state?: (fn [state]) -> bool
cost-fn: (fn [state]) -> comparable via cmp
cmp: (fn [cost-a cost-b] -> boolean"
  (let [max-generations 100
        descendants-from-each 10
        number-of-fit-to-keep 20]
    (loop [cur-generation 1
           most-fit (list state)]
      (if (>= cur-generation max-generations)
        (first (take-n-greatest-by 1 cost-fn cmp most-fit))
        (recur (inc cur-generation)
               (take-n-greatest-by number-of-fit-to-keep cost-fn
                                   (apply concat
                                          (pmap (fn [state]
                                                  (cons state
                                                        (remove nil? (map (fn [move]
                                                                            (if (legal-move? state move)
                                                                              (let [result-state (apply-move-fn state move)]
                                                                                (if (legal-state? result-state)
                                                                                  result-state))))
                                                                          (choose descendants-from-each (moves-for-state-fn state))))))
                                                most-fit))))))))
      

(def *simple-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")
(def *simple-map* (map-from-map-file *simple-map-file*))

(defn shortest-creep-path-cost [mapdata]
  (let [[start finish & _] (matching-blocks spawn? mapdata)]
    (creep-path-cost (a* start finish #(all-legal-ground-creep-moves %1 mapdata) mapdata))))

(defn best-towers-for-creeps [current-map]
  (let [spawns (matching-blocks spawn? current-map)
        start (nth spawns 0)
        finish (nth spawns 1)]
    (exhaustive-search current-map
                       (fn [mapdata] (distinct (apply concat
                                                      (map #(tower-placements-covering-pos %1 mapdata)
                                                           (a* start finish #(all-legal-ground-creep-moves %1 mapdata) mapdata)))))
                       (fn [map pos] (place-tower pos map))
                       (fn [map pos] (room-for-tower? pos map))
                       (fn [map] (not (empty? (a* start finish #(all-legal-ground-creep-moves %1 map) map))))
                       (fn [map] (creep-path-cost (a* start finish #(all-legal-ground-creep-moves %1 map) map)))
                       >)))

(defn best-towers-for-creeps* [current-map]
  (let [spawns (matching-blocks spawn? current-map)
        start (nth spawns 0)
        finish (nth spawns 1)]
    (genetic-search current-map
                    (fn [mapdata] (distinct (apply concat
                                                   (map #(tower-placements-covering-pos %1 mapdata)
                                                        (a* start finish #(all-legal-ground-creep-moves %1 mapdata) mapdata)))))
                    (fn [map pos] (place-tower pos map))
                    (fn [map pos] (room-for-tower? pos map))
                    (fn [map] (not (empty? (a* start finish #(all-legal-ground-creep-moves %1 map) map))))
                    (fn [map] (creep-path-cost (a* start finish #(all-legal-ground-creep-moves %1 map) map)))
                    >)))

(defn analyze-map-file [file]
  (let [current-map (map-from-map-file file)
        best-towers-map (best-towers-for-creeps current-map)]
    (println)(println)
    (println "Result cost: " (shortest-creep-path-cost current-map))
    (println (map-to-string (draw-path best-towers-map)))))

(defn analyze-map-file* [file]
  (let [current-map (map-from-map-file file)
        best-towers-map (best-towers-for-creeps* current-map)]
    (println)(println)
    (println "Result cost: " (shortest-creep-path-cost current-map))
    (println (map-to-string (draw-path best-towers-map)))))

;;(analyze-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")