(ns ddalyze.parse
  "Map parsing and unparsing"
  (:use clojure.core
        [clojure.contrib.duck-streams :only (read-lines write-lines)]
        [clojure.contrib.seq-utils :only (indexed)]
        ddalyze.binds
        ddalyze.map))

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
