(ns advent2018.day7)

(def test-input '[C A C F A B A D B E D E F E])

(def input '[T P Q W N A Z E L M R S F V C H V S J I I S A B M H B O H D K E P G X W Y U E U U D G W W O D O S O X Y D S P Y H W I P J H I K V H T Y U W J A M S H Y Y E R K V I G D J G T C Q C R D H S F S N Z N J K P Z S K D L F C X T X F A P X A S E D I B N U G S B Y Q H U G R V K Y M P P D X S P S N E A K R B R W Z U F Y E W I X U S I U P E E S W S F B P O N C N I C K P W Z O T Q R O Z I I A F O W D E G M D Z C])

(defn pairs->graphs
  "Maps nodes to a set of incoming edges"
  [input]
  (reduce (fn [g [n m]]
            (update g m (fnil conj #{}) n)) {} input))

(defn remove-edge [g from to]
  (let [g (update g to disj from)]
    (if (seq (get g to))
      g
      (dissoc g to))))

(defn remove-edges-from [g from]
  (let [tos (map key (filter (comp #(contains? % from) val) g))]
    (reduce #(remove-edge %1 from %2) g tos)))

(let [nodes (sort (distinct input))
      graph (->> input (partition 2) pairs->graphs)]
  (loop [nodes     nodes
         graph     graph
         selection (filter #(not (seq (graph %))) nodes)
         result    []]
    (if (seq selection)
      (let [[from & selection] selection
            tos (map key (filter (comp #(contains? % from) val) graph))
            nodes (remove #{from} nodes)
            graph (reduce #(remove-edge %1 from %2) graph tos)]
        (recur nodes
               graph
               (filter #(not (seq (graph %))) nodes)
               (conj result from)))
      (if (seq graph)
        (recur nodes
               graph
               (filter #(not (seq (graph %))) nodes)
               result)
        (apply str result)))))
;; => LFMNJRTQVZCHIABKPXYEUGWDSO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

(defn ready-for-work [{:keys [graph nodes]}]
  (filter #(not (seq (graph %))) nodes))

(defn tick [{:keys [wip time result] :as state}]
  (let [wip (into {} (map (juxt key (comp dec val))) wip)
        ready (map key (filter #(= 0 (val %)) wip))]
    (-> state
        (assoc :wip (apply dissoc wip ready)
               :result (concat result ready))
        (update :time inc)
        (update :graph (partial reduce remove-edges-from) ready))))

(defn letter-score [s]
  (- (long (first (str s))) (long \@)))

(defn assign-workers [{:keys [wip nodes workers penalty] :as state}]
  (let [wip-cnt (count wip)]
    (if (< wip-cnt workers)
      (let [rfw (take (- workers wip-cnt) (ready-for-work state))]
        (assoc state
               :wip (reduce #(assoc %1 %2 (+ penalty (letter-score %2))) wip rfw)
               :nodes (remove (set rfw) nodes)))
      state)))

(defn part2 [input workers penalty]
  (let [nodes   (sort (distinct input))
        graph   (->> input (partition 2) pairs->graphs)]
    (loop [state {:nodes     nodes
                  :graph     graph
                  :result    []
                  :wip       {}
                  :workers   workers
                  :penalty   penalty
                  :time      0}]
      (if (or (seq (:nodes state)) (seq (:wip state)))
        (recur (tick (assign-workers state)))
        (:time state)))))

(part2 test-input 2 0)
(part2 input 5 60)
