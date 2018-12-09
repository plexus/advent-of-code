(ns advent2018.day6)

(def test-input (partition 2 [1, 1 1, 6 8, 3 3, 4 5, 5 8, 9]))

(def input (partition 2 [268 273 211 325 320 225 320 207 109 222 267 283 119 70 138 277 202 177 251 233 305 107 230 279 243 137 74 109 56 106 258 97 248 346 71 199 332 215 208 292 154 80 74 256 325 305 174 133 148 51 112 71 243 202 136 237 227 90 191 145 345 133 340 299 322 256 86 323 341 310 342 221 50 172 284 160 267 142 244 153 131 147 245 323 42 241 90 207 245 167 335 106 299 158 181 186 349 286 327 108]))

(defn reduce-for* [binding-pairs init body]
  (if (next binding-pairs)
    (let [[[v val]] binding-pairs
          res (gensym "res")]
      `(reduce (fn [~res ~v]
                 ~(reduce-for* (next binding-pairs) res body))
               ~init
               ~val))
    (let [[[v val]] binding-pairs]
      `(reduce (fn [~'% ~v]
                 ~@body)
               ~init
               ~val))))

(defmacro reduce-for
  {:style/indent 2}
  [init bindings & body]
  (let [binding-pairs (partition 2 bindings)]
    (reduce-for* binding-pairs init body)))

(defn matrix [x y & [init]]
  (vec (repeat y (vec (repeat x init)))))

(defn manhatten [[^long x ^long y] [^long x' ^long y']]
  (+ (Math/abs (- x x'))
     (Math/abs (- y y'))))

(defn point-distances [coord input]
  (reduce (fn [acc coord']
            (update acc (manhatten coord' coord) conj coord'))
          ^{:coord coord} {}
          input))

(defn distances [input]
  (let [max-x (apply max (map first input))
        max-y (apply max (map second input))]
    (map (fn [y]
           (map (fn [x]
                  (point-distances [x y] input))
                (range (inc max-x))))
         (range (inc max-y)))))

(defn winners [distance-matrix]
  (map (fn [row]
         (map (fn [ds]
                (assert (map? ds))
                (let [mx (apply min (keys ds))
                      ms (get ds mx)]
                  (when (= (count ms) 1)
                    (first ms))))
              row))
       distance-matrix))

(defn remove-edges [winners]
  (let [edges (set (distinct
                    (concat (first winners)
                            (last winners)
                            (map first winners)
                            (map last winners))))]
    (remove nil? (remove edges (mapcat identity winners)))))

(->> test-input
     distances
     winners
     remove-edges
     frequencies)

(->> input
     distances
     winners
     remove-edges
     frequencies
     vals
     (apply max))

;; part 2

(defn total-distance [m]
  (reduce (fn [acc [dx coords]]
            (+ acc (* dx (count coords)))) 0 m))

(->> test-input
     distances
     (mapcat identity)
     (map total-distance)
     (filter #(< % 32))
     count
     )

(->> input
     distances
     (mapcat identity)
     (map total-distance)
     (filter #(< % 10000))
     count)
;; => 46542
