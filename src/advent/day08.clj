(ns advent.day08
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn parse-line [l]
  (when-let [[_ opreg op oparg predreg pred predarg] (re-find #"(\w+) (\w+) ([\d-]+) if (\w+) ([^ ]+) ([\d-]+)" l)]
    {:opreg (symbol opreg)
     :op ({"inc" + "dec" -} op)
     :oparg (Long/parseLong oparg)
     :predreg (symbol predreg)
     :pred ({"<" <
             "<=" <=
             "==" =
             "!=" not=
             ">=" >=
             ">" >} pred)
     :predarg (Long/parseLong predarg)}))

(def input (->> "day08.txt"
                io/resource
                io/reader
                line-seq
                (map parse-line)))

(apply max
       (vals
        (reduce (fn [state {:keys [opreg op oparg predreg pred predarg]}]
                  (if (pred (state predreg 0) predarg)
                    (assoc state opreg (op (state opreg 0) oparg))
                    [maxval state state]))
                {} input)))

(first (reduce (fn [[maxval state] {:keys [opreg op oparg predreg pred predarg]}]
                 (if (pred (state predreg 0) predarg)
                   (let [newval (op (state opreg 0) oparg)]
                     [(max maxval newval) (assoc state opreg newval)])
                   [maxval state state]))
               [0 {}] input))
