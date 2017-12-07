(ns advent.day07
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn parse-line [l]
  (when-let [[_ name weight children] (re-find #"(\w+) \((\d+)\)( -> ([\w, ]+))?" l)]
    [name (Long/parseLong weight) (re-seq #"\w+" (or children ""))]))

(def input (->> "day07.txt"
                io/resource
                io/reader
                line-seq
                (map parse-line)
                (into {} (map (fn [[n w ch]] [n {:name n
                                                 :weight w
                                                 :children ch}])))))

;; part 1
;; the "bottom" program is the one that isn't being supported by any other one,
;; i.e. that is not any other program's "child"

(set/difference
 (set (keys input))
 (into #{} cat (keep :children (vals input))))

;; part 2
(defn tower-weight [input name]
  (let [{:keys [weight children]} (get input name)]
    (apply + weight
           (map (partial tower-weight input) children))))

(keep (fn [[name {:keys [children]}]]
        (when (and children (apply not= (map (partial tower-weight input) children)))
          name))
      input)
;;=> ("mkxke" "onnfacs" "ftaxy")

;; these are the ones whose subtrees are unbalanced, but we need to find the
;; smallest unbalanced subtree. From here I'm just gonna look at the data and
;; REPL it.

(input "mkxke")                                ;;=> {:weight 74, :children ("uhcrfl" "bnhfnlw" "hdzls" "pcfilur" "onnfacs" "wdugfj" "jakdiea")}
(input "onnfacs")                              ;;=> {:weight 63612, :children ("ftaxy" "nrkctj" "tfnsk")}
(input "ftaxy")                                ;;=> {:weight 51, :children ("lkjbvw" "gexwzw" "cqoca" "xezjpye" "ftquc" "gggibf" "ferhk")}

;; the hierarchy is mkxke --> onnfacs --> ftaxy, so let's look at ftaxy

(map (partial tower-weight input) (:children (input "ftaxy")))
;;=> (1144 1153 1144 1144 1144 1144 1144)

;; yup, that's unbalanced alright. Seems the second child is too heavy. Time to
;; send gexwzw to the gym. (author's note: we're not judging, little gexwzw, but
;; we do need to solve this puzzle).

(input "gexwzw")                               ;;=> {:weight 277, :children ("ehvljub" "abuszt" "vfkzaix" "ghzsq")}

;; we need to get the subtree's weight from 1153 to 1144, so subtract that from
;; gexwzw's weight

(- (:weight (input "gexwzw")) (- 1153 1144))
;;=> 268
;; and there's your answer.


;; ok and now for a general solution. Boy this is messy.
(let [unbalanced-trees (keep (fn [[name {:keys [children] :as node}]]
                               (when (and children (apply not= (map (partial tower-weight input) children)))
                                 node))
                             input)
      smallest-subtree (first (remove (fn [node]
                                        (some (set (:children node)) (map :name (remove #{node} unbalanced-trees))))
                                      unbalanced-trees))
      subtree-weights (map (partial tower-weight input) (:children smallest-subtree))
      odd-weight-out (some (fn [[weight freq]] (if (= 1 freq) weight))
                           (frequencies subtree-weights))
      odd-one-out (first (filter #(= odd-weight-out (tower-weight input %)) (:children smallest-subtree)))
      ]
  (+ (:weight (input odd-one-out))
     (- (first (remove #{odd-weight-out} subtree-weights))
        odd-weight-out)))
