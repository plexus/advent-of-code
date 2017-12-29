(ns advent.day13
  (:require [clojure.java.io :as io]))

(def input
  (->> "day13.txt"
       io/resource
       io/reader
       line-seq
       (map #(re-find #"(\d+): (\d+)" %))
       (map (fn [[_ layer range]]
              [(Long/parseLong layer) (Long/parseLong range)]))
       (into {})))

(def firewall (map (fn [layer]
                     (if-let [range (get input layer)]
                       {:layer layer
                        :range range
                        :scanner 0
                        :severity (* layer range)}))
                   (range (apply max (vals input)))))

(defn move-scanner [{:keys [scanner range] :as layer}]
  (assoc layer :scanner (if (= scanner (dec range))
                          (- (dec scanner))
                          (inc scanner))))

(first firewall)                               ;;=> {:layer 0, :range 3, :scanner 0, :severity 0}

(take 10 (map :scanner (iterate move-scanner {:layer 0, :range 3, :scanner 0, :severity 0})));;=> (0 1 2 -1 0 1 2 -1 0 1)

(defn move-scanners [layers]
  (map #(and % (move-scanner %)) layers))

(map (fn [firewall pos]
       (when (some-> firewall (nth pos) :scanner (= 0))
         (:severity (nth firewall pos))))
     (iterate move-scanners firewall)
     (range (count firewall)))
