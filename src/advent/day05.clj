(ns advent.day05
  (:require [clojure.java.io :as io]))

(def input
  (->> "day05_input.txt"
       io/resource
       io/reader
       line-seq
       (mapv #(Long/parseLong %))))

(defn out-of-bounds? [index input]
  (not (< -1 index (count input))))

(defn solve-1 [steps index input]
  (if (out-of-bounds? index input)
    steps
    (recur (inc steps)
           (+ index (get input index))
           (update input index inc))))

(defn solve-2 [steps index input]
  (if (out-of-bounds? index input)
    steps
    (let [offset (get input index)]
      (recur (inc steps)
             (+ index offset)
             (update input index (if (>= offset 3) dec inc))))))

(solve-2 0 0 [0 3 0 1 -3])                     ;;=> 10
(solve-2 0 0 input)                            ;;=> 28707598

(time
 (solve-2 0 0 input))                          ;;=> 28707598
