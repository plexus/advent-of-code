(ns day02
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn split-rows [input]
  (str/split input #"\n"))

(defn split-cells [input]
  (str/split input #"\s+"))

(defn row-max-diff [row]
  (->> row
       (map #(Long/parseLong %))
       ((juxt (partial apply max) (partial apply min)))
       sort
       reverse
       (apply -)))

(print (->> input
            split-rows
            (map split-cells)
            (map row-max-diff)
            (apply +)))
