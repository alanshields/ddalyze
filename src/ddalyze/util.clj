(ns ddalyze.util
  "Utility functions"
  (:use clojure.core
        ddalyze.binds
        [clojure.contrib.seq-utils :only (shuffle partition-all indexed rand-elt)]))

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
  (if (= n 1)
    (rand-elt coll)
    (take n (shuffle coll))))

(defmacro choose-exec [& execs]
  (let [chosen (gensym "chosen")]
    `(let [~chosen (rand-int ~(count execs))]
       (cond
        ~@(mapcat (fn [i e]
                    `((= ~i ~chosen) ~e))
                  (range (count execs))
                  execs)))))
  