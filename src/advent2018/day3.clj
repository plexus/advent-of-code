(ns advent2018.day3
  (:require [advent2018.util :refer [str-line-seq]]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [clojure.set :as set]))

(def input (str-line-seq (slurp (io/resource "advent2018/day3.txt"))))

(def line-regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"  )

(def parse-claims (map (fn [l]
                         (if-let [tokens (re-find line-regex l)]
                           (let [[id x y w h] (sequence (comp (drop 1) (map #(Integer/parseInt %))) tokens)]
                             {:id id
                              :x x
                              :y y
                              :w w
                              :h h})))))

(def claim->coords (map (fn [{:keys [id x y w h]}]
                          (for [x (range x (+ x w))
                                y (range y (+ y h))]
                            (vary-meta [x y] assoc :claim/id id)))))

(x/some (comp parse-line
              claim->coords
              cat
              (x/by-key identity x/count)
              (filter (fn [[_ n]] (> n 1)))
              x/count)
        input)
;;=> 116489

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

(set/difference (into #{} (comp parse-line (map :id)) input)
                (into #{} (comp parse-line
                                claim->coords
                                cat
                                (x/by-key identity (x/into []))
                                (filter (fn [[_ n]] (> (count n) 1)))
                                (map (fn [[_ coords]] (map #(:claim/id (meta %)) coords)))
                                cat)
                      input))
;;=> #{1260}
