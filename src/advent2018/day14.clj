(ns advent2018.day14
  (:require [clojure.string :as str]))

(def *warn-on-reflection* true)
(def *unchecked-math* :warn-on-boxed)

(def input [3 6 0 7 8 1])

(defn digits [^long n]
  (map #(- (long %) (long \0)) (str n)))

(defn next-scoreboard [[scoreboard idx1 idx2]]
  (let [e1 (get scoreboard idx1)
        e2 (get scoreboard idx2)
        scoreboard' (into scoreboard (digits (+ e1 e2)))
        idx1' (mod (+ idx1 e1 1) (count scoreboard'))
        idx2' (mod (+ idx2 e2 1) (count scoreboard'))]
    [scoreboard' idx1' idx2']))

(defn viz-board [[s i1 i2]]
  (println (str/join (map-indexed #(cond
                                     (= %1 i1) (str "(" %2 ")")
                                     (= %1 i2) (str "[" %2 "]")
                                     :else (str " " %2 " "))
                                  s))))


(defn next-10-recipes [input after-n-recipes]
  (let [start [input 0 1]
        [scoreboard']
        (->> start
             (iterate next-scoreboard)
             (drop-while #(< (count (first %))
                             (+ after-n-recipes 10)))
             first)]
    (str/join (take 10 (drop after-n-recipes scoreboard')))))

(let [start [[3 7] 0 1]]
  (run! viz-board (take 10 (iterate next-scoreboard start))))

(next-10-recipes [3 7] 360781)
;; => 6521571010


;; part 2

(defn find-first-occurence [input]
  (let [scoreboard [[3 7] 0 1]
        incnt (long (count input))]
    (reduce
     (fn [^long pos [scoreboard _ _ :as b]]
       (print pos (count scoreboard) incnt ": ")
       (viz-board b)
       (loop [pos (long pos)]
         (if (<= pos (- (count scoreboard) incnt))
           (if (= input (subvec scoreboard pos (+ pos incnt)))
             (reduced pos)
             (if (< pos (- (count scoreboard) incnt 1))
               (recur (inc pos))
               pos))
           pos)))
     0
     (iterate next-scoreboard scoreboard))))

#_
(find-first-occurence (digits 59414))
