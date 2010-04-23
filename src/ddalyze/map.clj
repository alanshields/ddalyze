(ns ddalyze.map
  "Map and block functions"
  (:use clojure.core
        ddalyze.binds))

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

(defn draw-path [path mapdata]
  (set-pos-types path
                 'path
                 mapdata))
