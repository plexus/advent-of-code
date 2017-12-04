(ns advent.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "day04_input.txt"
                io/resource
                io/reader
                line-seq
                (map #(str/split % #"\s"))))

(defn val-gt-one? [[_ cnt]]
  (> cnt 1))

(def part1
  (->> input
       (map frequencies)
       (remove (partial some val-gt-one?))
       count))

(def part2
  (->> input
       (map (partial map sort))
       (map frequencies)
       (remove (partial some val-gt-one?))
       count))
