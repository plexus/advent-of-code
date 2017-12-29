(ns advent.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split (str/trim (slurp (io/resource "day11.txt"))) #","))

(defn step [[x y z] dir]
  (case dir
    "se"  [(inc x) (dec y) z]
    "s" [x (dec y) (inc z)]
    "sw" [(dec x) y (inc z)]
    "nw"  [(dec x) (inc y) z]
    "n" [x (inc y) (dec z)]
    "ne" [(inc x) y (dec z)]))

(reduce step [0 0 0] (str/split "se,sw,se,sw,sw" #","));;=> [-1 -2 3]

(reduce step [0 0 0] (str/split "ne,ne,ne" #","));;=> [3 0 -3]

(reduce step [0 0 0] (str/split "se,sw,ne" #","));;=> [1 -1 0]

(defn magnitude [v]
  (/ (apply + (map #(Math/abs %) v)) 2))

(magnitude [-1 -2 3])

;; part 1
(magnitude (reduce step [0 0 0] input))        ;;=> 670

;; part 2
(apply max (map magnitude (reductions step [0 0 0] input)))
;;=> 1426
