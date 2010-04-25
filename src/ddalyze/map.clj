(ns ddalyze.map
  "Map and block functions"
  (:use clojure.core
        ddalyze.binds))

;; Map block types
;;   "A map block is a square on the board. A tower takes up 4 map blocks."
(defstruct mapdata
  :blocks :row-count :column-count :shortest-path :shortest-path-cost :towers)
(defstruct map-block
  :row :column :type)
(defstruct map-pos
  :row :column)

; range is in blocks, not towers!
(def *towers*
     (hash-map 'pellet {:name 'pellet :range 4 :damage 5 :attacks-per-second 1 :price 5}
               'squirt {:name 'squirt :range 5 :damage 10 :attacks-per-second 2 :price 15}))
(defn possible-tower-types []
  (keys *towers*))
(defn towers-at-price [d]
  "At or BELOW price"
  (filter #(>= d (:price %1)) (vals *towers*)))
(defn get-tower [tower-type]
  (if (symbol? tower-type)
    (get *towers* tower-type)
    tower-type))

(defn is-pos? [p]
  (and (ifn? p)
       (:row p)
       (:column p)))
(defn is-map? [m]
  (and (ifn? m)
       (:blocks m)))
(defn is-map-block? [b]
  (and (ifn? b)
       (is-pos? b)
       (:type b)))
(defn is-tower? [t]
  (and (ifn? t)
       (:name t)
       (:range t)
       (:damage t)
       (:attacks-per-second t)
       (:price t)))
(defn is-placed-tower? [t]
  (and (is-tower? t)
       (:position t)))

;; Block functions
(defn pos= [a b]
  "Determine if two blocks are positionally equal"
  (and (= (:row a) (:row b))
       (= (:column a) (:column b))))

(defmacro defmap-type-match [pred-name matching-types]
  `(let [match-set# (apply hash-set ~matching-types)]
     (defn ~pred-name [{map-type# :type}]
       (map-type# match-set#))))

(defmap-type-match playable? '(playable))
(defmap-type-match out-of-bounds? '(out-of-bounds))
(defmap-type-match spawn? '(spawn spawnexit))
(defmap-type-match exit? '(exit spawnexit))
(defmap-type-match creepable? '(playable exit path spawnexit))

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
(defn new-pos [r c]
  (struct map-pos r c))
(defn new-map
  ([blockdata] (struct mapdata
                       (vec blockdata)
                        (+ 1 (apply max (map :row blockdata)))
                        (+ 1 (apply max (map :column blockdata)))))
  ([new-blockdata oldmap]
     (struct mapdata (vec new-blockdata) (:row-count oldmap) (:column-count oldmap) nil nil (:towers oldmap))))

(defn set-pos-type [pos type mapdata]
  "For pos in the mapdata, set its type to TYPE"
  (let [pid (pos-id pos mapdata)
        old-block (get (:blocks mapdata) pid)]
    (new-map (assoc (:blocks mapdata) pid (assoc old-block :type type))
             mapdata)))
(defn set-pos-types [poss type mapdata]
  (loop [mapdata mapdata
         poss poss]
    (if (empty? poss)
      mapdata
      (let [[curpos & restpos] poss]
        (recur (set-pos-type curpos type mapdata)
               restpos)))))

(defn block-at
  ([row column map]
     (block-at (pos-id row column map) map))
  ([pos-id map]
     (if (contains? (:blocks map) pos-id)
       (get (:blocks map) pos-id))))
(defn pos-offset [pos r c]
  (new-pos (+ r (:row pos))
           (+ c (:column pos))))
(defn block-at-offset [pos r c map]
  {:pre [(is-pos? pos)
         (number? r)
         (number? c)
         (is-map? map)]}
  (block-at (pos-id (pos-offset pos r c) map) map))
  
(defn matching-blocks [pred? map]
  (filter pred? (:blocks map)))
(defn block-map [fn mapdata]
  (map fn (:blocks mapdata)))
(defn towers-on-map [mapdata]
  (:towers mapdata))

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
(defn tower-with-position [tower pos]
  (assoc tower :position pos))
(defn tower-position [tower]
  {:pre [(is-placed-tower? tower)]
   :post [(is-pos? %)]}
  (:position tower))
(defn pos-to-tower-center [pos]
  (pos-offset pos 0.5 0.5))
(defn tower-center-position [tower]
  (pos-to-tower-center (tower-position tower)))
(defn tower-name [tower]
  (:name tower))
(defn tower-range [tower]
  (:range tower))
(defn tower-damage [tower]
  (:damage tower))
(defn tower-price [tower]
  (:price tower))
(defn map-price [mapdata]
  "Price for buying all the towers in a map"
  (reduce + (map tower-price (towers-on-map mapdata))))

(defn place-tower
  "Will fail only if there is no room for the tower - tower will be allowed to block the path"
  ([pos tower mapdata]
     {:pre [(is-pos? pos)
            (is-map? mapdata)
            (is-tower? tower)]}
     (let [result (place-tower pos tower mapdata 'fail)]
       (if (= result 'fail)
         (throw (Exception. (format "Could not place tower at %s" pos)))
         result)))
  ([pos tower mapdata on-fail]
     {:pre [(is-pos? pos)
            (is-map? mapdata)]}
     (let [b #(block-at-offset pos %1 %2 mapdata)]
       (if (room-for-tower? pos mapdata)
         (let [newmap (set-pos-types (list (b 0 0) (b 0 1) (b 1 0) (b 1 1)) (tower-name tower) mapdata)
               current-towers (:towers newmap)]
           (assoc newmap :towers (cons (tower-with-position tower pos) current-towers)))
         on-fail))))
(defn remove-tower-at-pos [pos mapdata]
  {:pre [(is-pos? pos)
         (is-map? mapdata)]
   :post [(is-map? %)]}
  (let [tower (first (filter #(pos= (tower-position %1) pos) (towers-on-map mapdata)))]
    (if tower
      (let [b #(block-at-offset pos %1 %2 mapdata)]
        (let [newmap (set-pos-types (list (b 0 0) (b 0 1) (b 1 0) (b 1 1)) 'playable mapdata)
                current-towers (towers-on-map newmap)]
            (assoc newmap :towers (remove #(pos= tower %1) current-towers))))
      mapdata)))

(defn blocks-within-distance
  ([pos distance map] (blocks-within-distance pos distance map crow-distance))
  ([pos distance map comparator] (matching-blocks #(<= (comparator pos %1) distance) map)))

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
(defn tower-placements-reaching-pos [pos tower mapdata]
  "Returns a list of legal tower placements that would be in range of pos"
  {:pre [(is-pos? pos)
         (is-tower? tower)
         (is-map? mapdata)]}
  (filter #(room-for-tower? %1 mapdata)
          (blocks-within-distance (pos-to-tower-center pos) (tower-range tower) mapdata)))
(defn towers-within-range [pos mapdata]
  "Returns a list of towers that can hit pos"
  (filter #(>= (tower-range %1) (crow-distance pos (tower-center-position %1)))
          (towers-on-map mapdata)))


(defn draw-path [path mapdata]
  (set-pos-types path
                 'path
                 mapdata))
