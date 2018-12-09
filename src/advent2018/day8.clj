(ns advent2018.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def input (map #(Integer/parseInt (str/trim %)) (str/split (slurp (io/resource "advent2018/day8.txt")) #" ")))
(def test-input [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn parse-node* [[n m & rst]]
  (loop [state {:nodes-left n
                :children []
                :rest rst}]
    (let [{:keys [nodes-left children rest]} state]
      (if (> nodes-left 0)
        (let [[rst node] (parse-node* rest)]
          (recur {:nodes-left (dec nodes-left)
                  :children (conj children node)
                  :rest rst}))
        [(drop m rest) {:children children
                        :metadata (take m rest)}]))))

(defn parse-node [input]
  (second (parse-node* input)))

(defn meta-sum [tree]
  (+ (apply + (map meta-sum (:children tree)))
     (apply + (:metadata tree))))

(meta-sum (parse-node input))
;; => 35911

(defn node-value [node]
  (if (seq (:children node))
    (transduce (map (fn [m]
                      (if-let [ch (get (:children node) (dec m))]
                        (node-value ch)
                        0)))
               +
               (:metadata node))
    (apply + (:metadata node))))

(node-value (parse-node input))
