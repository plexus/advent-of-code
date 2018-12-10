(ns advent2018.day14-helios)

(def day-input 846601)
(def day-input-vec [8 4 6 6 0 1])

(set! *unchecked-math* :warn-on-boxed)

(def initial-state {:workers #{0 1}
                    :table   [3 7]})


(defn iteration [{:keys [workers table]}]
  (let [scores (map (partial nth table)
                    workers)
        ^long recipe-sum (apply + scores)
        new-recipes (if (< recipe-sum 10) [recipe-sum]
                        [(quot recipe-sum 10) (rem recipe-sum 10)])
        new-table (reduce conj table new-recipes)]

    {:workers (map (fn [^long index] (mod (+ index (inc ^long (nth table index)))
                                          (count new-table)))
                   workers)
     :table   new-table}))


(defn compute-score-after [^long n]
  (subvec (->> (iterate iteration initial-state)
               (drop-while #(< (count (:table %))
                               (+ 10 n)))
               first
               :table)
          n (+ n 10))
  )


(defn contains-sequence-at-end? [table sequence]
  (let [c1 (count table)
        c2 (count sequence)]
    (and (>= c1 c2)
         (= sequence (subvec table (- c1 c2)))))
  )


(defn solution2 [input]
  (->> (nth (iterate iteration initial-state) (count input))
       (iterate iteration)
       (drop-while (comp not #(contains-sequence-at-end? (:table %) input)))
       first
       :table
       (drop-last (count input))
       (count))
  )


(solution2 day-input-vec)
