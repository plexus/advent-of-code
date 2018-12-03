(ns advent2018.day2
  (:require [advent2018.util :refer [str-line-seq]]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]))

(def input (-> "advent2018/day2.txt" io/resource slurp str-line-seq))

(def char-counts (map (fn [s]
                        (set (vals (frequencies s))))))

(def only-two (filter (fn [s] (s 2))))
(def only-three (filter (fn [s] (s 3))))

(time
 (transduce (comp char-counts
                  (x/multiplex [(comp only-two x/count)
                                (comp only-three x/count)]))
            (fn
              ([] [])
              ([[x y]] (* x y))
              ([x y] (conj x y)))
            input))
"Elapsed time: 5.859766 msecs"
;;=> 4712

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(def box-id-set (map (fn [id]
                       (set (map (fn [i]
                                   (str (subs id 0 i)
                                        (subs id (inc i))))
                                 (range (count id)))))))

(time
 (transduce
  box-id-set
  (fn
    ([] #{})
    ([x] x)
    ([acc ids]
     (if-let [id (some acc ids)]
       (reduced id)
       (into acc ids))))
  input))
"Elapsed time: 4.787275 msecs"
;;=> "lufjygedpvfbhftxiwnaorzmq"
