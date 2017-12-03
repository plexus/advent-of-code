(def +input+ (last js/process.argv))

(print
 (->> +input+
      (into [])
      (#(conj % (first +input+)))
      (map js/parseInt)
      (partition 2 1)
      (filter (partial apply =))
      (map first)
      (apply +)))
