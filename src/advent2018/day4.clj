(ns advent2018.day4
  (:require [advent2018.util :refer [line-reader-resource]]
            [net.cgrand.xforms :as x]
            [clojure.java.io :as io]))

(def input (line-reader-resource (io/resource "advent2018/day4.txt")))

(def date-pattern #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\]")
(def begin-pattern #"Guard #(\d+) begins shift")
(def wake-pattern #"wakes up")
(def sleep-pattern #"falls asleep")

(defn parse-line [l]
  (let [[_ y M d h m] (re-find date-pattern l)
        m (Long/parseLong m)
        h (Long/parseLong h)]
    (if-let [[_ id] (re-find begin-pattern l)]
      {:type :begin :id (Long/parseLong id) :min m :hour h}
      (cond
        (re-find wake-pattern l)
        {:type :wake :min m :hour h}

        (re-find sleep-pattern l)
        {:type :sleep :min m :hour h}))))

(time
 (x/some (comp (x/sort)
               (map parse-line)
               (x/reduce (fn
                           ([] {})
                           ([x] (:naps x))
                           ([acc {:keys [type min hour id] :as m}]
                            #_(prn [acc m])
                            (case type
                              :begin
                              (assoc acc :id id)
                              :sleep
                              (assoc acc :hour hour :min min)
                              :wake
                              (update-in acc [:naps (:id acc)] conj [(:min acc) min])))))
               cat
               (map (fn [[id naps]]
                      {:id id
                       :total (reduce (fn [acc [b e]]
                                        (+ acc (- e b)))
                                      0 naps)
                       :minutes (map (fn [m]
                                       (count (filter (fn [[b e]]
                                                        (<= b m (dec e)))
                                                      naps)))
                                     (range 60))}))

               (x/maximum (comparator #(< (:total %1) (:total %2))))
               (map (fn [{:keys [id minutes]}]
                      (* id (first
                             (reduce (fn [[m cnt] [m' cnt']]
                                       (if (> cnt' cnt)
                                         [m' cnt']
                                         [m cnt]))
                                     [0 0]
                                     (map vector (range) minutes)))))))
         input))
"Elapsed time: 12.139923 msecs"
;;=> 11367


;; part 2

(time
 (x/some (comp (x/sort)
               (map parse-line)
               (x/reduce (fn
                           ([] {})
                           ([x] (:naps x))
                           ([acc {:keys [type min hour id] :as m}]
                            #_(prn [acc m])
                            (case type
                              :begin
                              (assoc acc :id id)
                              :sleep
                              (assoc acc :hour hour :min min)
                              :wake
                              (update-in acc [:naps (:id acc)] conj [(:min acc) min])))))
               cat
               (map (fn [[id naps]]
                      {:id id
                       :total (reduce (fn [acc [b e]]
                                        (+ acc (- e b)))
                                      0 naps)
                       :minutes (map (fn [m]
                                       (count (filter (fn [[b e]]
                                                        (<= b m (dec e)))
                                                      naps)))
                                     (range 60))}))

               (map (fn [{:keys [id minutes] :as guard}]
                      (assoc guard :record (reduce (fn [[m cnt] [m' cnt']]
                                                     (if (> cnt' cnt)
                                                       [m' cnt']
                                                       [m cnt]))
                                                   [0 0]
                                                   (map vector (range) minutes)))))
               (x/maximum (comparator #(< (second (:record %1))
                                          (second (:record %2)))))
               (map (fn [{:keys [id record]}]
                      (* id (first record)))))
         input))
"Elapsed time: 20.219886 msecs"
36896
