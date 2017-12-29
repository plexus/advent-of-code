(ns advent.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(def input
  (->> "day12.txt"
       io/resource
       io/reader
       line-seq
       (map #(re-find #"(\d+) <-> ([\d, ]+)" %))
       (map (fn [[_ src dest]]
              [(Long/parseLong src)
               (set (map #(Long/parseLong %) (str/split dest #", ")))]))
       (into {})))

(defn part [result input src]
  (let [result (conj result src)]
    (reduce
     (fn [result dest]
       (into result (part result input dest)))
     result
     (set/difference (get input src) result))))

;; part 1
(count (part #{} input 0))
;;=> 141

;; part 2
(loop [results #{}
       nodes (set (keys input))]
  (if (empty? nodes)
    (count results)
    (let [[key & rest] nodes
          group (part #{} input key)]
      (recur (conj results group)
             (set/difference nodes group)))))
;;=> 171
