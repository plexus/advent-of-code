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
  #_(when (= (mod steps 10000) 0)
      (prn [:vec steps index]))
  (if (out-of-bounds? index input)
    steps
    (let [offset (get input index)]
      (recur (inc steps)
             (+ index offset)
             (update input index (if (>= offset 3) dec inc))))))

;; Java array version of solve-2

(defn solve-array [steps index input limit]
  #_(when (= (mod steps 10000) 0)
      (prn [:arr steps index]))
  (if (not (< -1 index limit))
    steps
    (let [offset (aget input index)
          new-offset (if (>= offset 3)
                       (dec offset)
                       (inc offset))]
      (aset input index new-offset)
      (recur (inc steps)
             (+ index offset)
             input
             limit))))

;; Java array version that avoids boxing/unboxing and reflection

(defn aget-long ^long [arr ^long idx]
  (. java.lang.reflect.Array (get arr idx)))

(defn aset-long [arr ^long idx ^long val]
  (. java.lang.reflect.Array (set arr idx val)))

(defn update-offset ^long [^long offset]
  (if (>= offset 3)
    (dec offset)
    (inc offset)))

(defn solve-array-fast [^long steps ^long index input ^long limit]
  #_(when (= (mod steps 10000) 0)
      (prn [:arr steps index]))
  (if (not (< -1 index limit))
    steps
    (let [offset (aget-long input index)
          new-offset (update-offset offset)]
      (aset-long input index new-offset)
      (recur (inc steps)
             (+ index offset)
             input
             limit))))

(comment
  (solve-2 0 0 [0 3 0 1 -3])                     ;;=> 10

  (solve-2 0 0 input)                            ;;=> 28707598
  ;; "Elapsed time: 21478.656249 msecs"

  (solve-array 0 0 (into-array Long/TYPE input) (count input))
  ;; This seems to be about 38 times slower than the vector version
  ;; maybe boxing/unboxing is to blame?

  (solve-array-fast 0 0 (into-array Long/TYPE input) (count input))
  ;;"Elapsed time: 7459.838384 msecs"
  ;; Now we're getting somewhere, about 3x faster than the pure Clojure version,
  ;; by avoiding boxing and reflection.
  )
