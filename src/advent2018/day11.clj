(ns advent2018.day11
  (:require [clojure.test :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn power-level
  {:test (fn []
           (assert (= (power-level 3 5 8) 4))
           (assert (= (power-level 122 79 57) -5))
           (assert (= (power-level 217 196 39) 0))
           (assert (= (power-level 101 153 71) 4)))}
  [^long x ^long y ^long serial]
  (let [rack-id (+ x 10)
        power-level (* rack-id (+ (* rack-id y) serial))
        hundred-digit (first (drop 2 (reverse (str power-level))))]
    (if hundred-digit
      (- (long hundred-digit) (long \0) 5)
      0)))

(test #'power-level)

(defn power-grid [^long serial]
  (into {}
        (for [x (range 1 301)
              y (range 1 301)]
          [[x y] (power-level x y serial)])))

(defn coords3x3 [^long top-left-x ^long top-left-y]
  (for [^long x (range 3)
        ^long y (range 3)]
    [(+ x top-left-x) (+ y top-left-y)]))

(defn summed-grid [^long serial]
  (let [grid (power-grid serial)]
    (into {}
          (for [x (range 1 298)
                y (range 1 298)
                :let [coords (coords3x3 x y)]]
            [[x y] (apply + (map grid coords))]))))

#_
(last
 (sort-by val (summed-grid 7139)))

;; part 2

(def pg (power-grid 7139))

(def sag
  (let [pg (power-grid 7139)
        sag {}]
    (reduce (fn [sag [x y :as coord]]
              (assoc sag coord (- (+ (get pg coord)
                                     (get sag [(dec x) y] 0)
                                     (get sag [x (dec y)] 0))
                                  (get sag [(dec x) (dec y)] 0))))
            sag
            (for [x (range 1 301)
                  y (range 1 301)]
              [x y]))))

(defn area-size [sag [^long x ^long y ^long size]]
  (try
    (- (+ ^long (get sag [(dec (+ x size)) (dec (+ y size))])
          ^long (get sag [(dec x) (dec y)] 0))
       ^long (get sag [(dec (+ x size)) (dec y)] 0)
       ^long (get sag [(dec x) (dec (+ y size))] 0))
    (catch Exception e
      (println [x y size]))))

(defn part-2 []
  (time
   (reduce
    (fn [[id ^long current-max :as old] id']
      (let [^long total (area-size sag id')]
        (if (> total current-max)
          [id' current-max]
          old)))
    [nil 0]
    (for [^long size (range 1 301)
          ^long x (range 1 (- 302 size))
          ^long y (range 1 (- 302 size))]
      [x y size]))))
