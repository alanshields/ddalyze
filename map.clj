(ns ddalyze
  "Namespace for DDalyze"
  (:use clojure.core
        [clojure.contrib.duck-streams :only (read-lines write-lines)]
        clojure.contrib.seq-utils)
  (:import (java.io.File)))

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
         [curpos & restpos] poss]
    (if (empty? restpos)
      mapdata
      (recur (set-pos-type curpos type mapdata)
             restpos))))

; More interesting questions about maps
(defn block-at [row column map]
  (first (filter #(is-pos row column %1)
                 map)))
(defn matching-type [type map]
  (filter #(is-type type %1) map))

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
    

(defn blocks-within-distance
  ([pos distance map] (blocks-within-distance pos distance map crow-distance))
  ([pos distance map comparator] (filter #(<= (comparator pos %1) distance) map)))

(defn all-legal-ground-creep-moves [pos map]
  (let [offset-block (fn [r c]
                       (block-at (+ r (:row pos)) (+ c (:column pos)) map))
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
                            (if (or (creep-or-nil u)
                                    (creep-or-nil r))
                              ur)
                            (if (or (creep-or-nil r)
                                    (creep-or-nil d))
                              lr)
                            (if (or (creep-or-nil d)
                                    (creep-or-nil l))
                              ll)
                            (if (or (creep-or-nil l)
                                    (creep-or-nil u))
                              ul)))))))

(defn a* [from goal legal-moves-for-pos-fn mapdata]
  "f(x) = g(x) + h(x)
g is current path-cost
h is estimated future path-cost

Uses terminology and pseudocode from Wikipedia:
http://en.wikipedia.org/wiki/A*_search_algorithm"
  (let [number-of-rows (+ 1 (reduce max (map :row mapdata)))
        number-of-columns (+ 1 (reduce max (map :column mapdata)))
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
      
(defn breadth-first-search
  "Starting at FROM, find the shortest path to any block in POSSIBLE-EXITS.
legal-moves-for-pos-fn must take one argument - the current position."
  ([from possible-exits legal-moves-for-pos-fn]
     (let [goal? (fn [pos]
                   (let [found-matches (filter #(pos= pos %1) possible-exits)]
                     (if (empty? found-matches)
                       nil
                       found-matches)))]
       (if (goal? from)
         (list from)
         (loop [queue (list (list from))]
           (if (empty? queue)
             queue
             (let [[pos & _ :as path] (first queue)
                   new-positions (remove (fn [newmove] ; only consider legal moves that are not in path already
                                           (some #(pos= %1 newmove) path))
                                         (legal-moves-for-pos-fn pos))]
               (if (empty? new-positions)
                 (recur (rest queue))
                 (if-let [goal-found (some goal? new-positions)]
                   (cons goal-found path)
                   (recur (concat (rest queue)
                                  (map #(cons %1 path) new-positions))))))))))))


    
;;;; Map IO Functions
(defn map-file-comment? [str]
  (= \; (first str)))

(def *map-char->-type*
     {\X 'out-of-bounds
      \space 'playable
      \O 'spawn
      \a 'path})
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
(defn map-to-map-file [file mapdata]
  (write-lines file
               (map #(apply str %1)
                    (row-map mapdata map-type-to-file-char))))



(def *simple-map-file* "/Users/alanshields/code/desktop_defender/maps/basic.map")
(def *simple-map* (map-from-map-file *simple-map-file*))

(time (let [current-map (map-from-map-file *simple-map-file*)]
        (map-to-map-file "/Users/alanshields/tmp.map"
                         (set-pos-types
                          (breadth-first-search (first (filter spawn? current-map))
                                                (rest (filter spawn? current-map))
                                                #(all-legal-ground-creep-moves %1 current-map))
                          'path
                          current-map))))
(time (let [current-map (map-from-map-file *simple-map-file*)]
        (map-to-map-file "/Users/alanshields/tmp.map"
                         (set-pos-types
                          (a* (first (filter spawn? current-map))
                              (nth (filter spawn? current-map) 1)
                              #(all-legal-ground-creep-moves %1 current-map)
                              current-map)
                          'path
                          current-map))))

(let [current-map (map-from-map-file "/Users/alanshields/code/desktop_defender/maps/week1.map")
      goals (filter spawn? current-map)]
  (a* (nth goals 0) (nth goals 1) #(all-legal-ground-creep-moves %1 current-map) current-map))
(let [current-map (map-from-map-file "/Users/alanshields/code/desktop_defender/maps/basic.map")
      goals (filter spawn? current-map)]
  (a* (nth goals 0) (nth goals 1) #(all-legal-ground-creep-moves %1 current-map) current-map))

(time (let [current-map (map-from-map-file "/Users/alanshields/code/desktop_defender/maps/week1.map")]
        (map-to-map-file "/Users/alanshields/tmp.map"
                         (set-pos-types
                          (a* (first (filter spawn? current-map))
                              (nth (filter spawn? current-map) 1)
                              #(all-legal-ground-creep-moves %1 current-map)
                              current-map)
                          'path
                          current-map))))

;; (let [current-map (map-from-map-file "/Users/alanshields/code/desktop_defender/maps/week1.map")]
;;   (map-to-map-file "/Users/alanshields/tmp.map"
;;                    current-map))