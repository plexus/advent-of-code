(ns advent2018.day1
  (:require [clojure.java.io :as io]
            [advent2018.util :refer [str-line-seq]]))

(def input (slurp (io/resource "advent2018/day1.txt")))
(def split-tokens (map (fn [l]
                         [({\- -
                            \+ +}
                           (first l))
                          (Integer/parseInt
                           (subs l 1))])))

(transduce
 split-tokens
 (completing #(apply (first %2) %1 (rest %2)))
 0
 (str-line-seq input))
;;=> 484

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

(defn repeat-seq [s]
  (lazy-seq
   (concat s (repeat-seq s))))

(reduce
 (fn [acc x]
   (if (acc x)
     (reduced x)
     (conj acc x)))
 #{}
 (reductions
  #(apply (first %2) %1 (rest %2))
  0
  (repeat-seq (into [] split-tokens (str-line-seq input)))))
;;=> 367
