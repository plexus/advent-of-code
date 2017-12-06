(ns advent.day06
  (:require [clojure.string :as str]))

(def input (-> "0	5	10	0	11	14	13	4	11	8	8	7	1	4	12	11"
               (str/split #"\t")
               (->> (mapv #(Long/parseLong %)))))

(defn highest-bank-pos [bs]
  (count
   (take-while #(not= % (apply max bs)) bs)))

(defn redistribute [bs idx]
  (let [cnt (get bs idx)
        wrap #(mod % (count bs))]
    (reduce (fn [bs idx]
              (update bs (wrap idx) inc))
            (assoc bs idx 0)
            (range (inc idx) (inc (+ idx cnt))))))

;; part 1

(reduce (fn [[history bs] step]
          (if (contains? history bs)
            (reduced step)
            [(conj history bs)
             (redistribute bs (highest-bank-pos bs))]))
        [#{} input]
        (range))

;; part 2

(reduce (fn [[history bs] step]
          (if (contains? history bs)
            (reduced (- step (get history bs)))
            [(assoc history bs step)
             (redistribute bs (highest-bank-pos bs))]))
        [{} input]
        (range))
