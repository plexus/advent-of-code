(ns advent2018.day9
  (:import clojure.lang.Volatile))

(set! *warn-on-reflection* true)

(defprotocol ICircle
  (insert ^Circle [this number])
  (remove ^Circle [this])
  (forward ^Circle [this n])
  (backward ^Circle [this n])
  (current ^Marble [_]))

(defprotocol IMarble
  (nextm ^Marble [this])
  (prevm ^Marble [this]))

(deftype Marble [number ^Volatile previous ^Volatile next]
  IMarble
  (nextm ^Marble [_] @next)
  (prevm ^Marble [_] @previous))

(deftype Circle [^long count ^Marble current]
  ICircle
  (current [_]
    current)
  (insert [this number]
    (if current
      (let [^Marble marble (->Marble number (volatile! current) (volatile! (nextm current)))]
        (vreset! (.previous (nextm current)) marble)
        (vreset! (.next current) marble)
        (->Circle (inc count) marble))
      (let [^Marble marble (->Marble number (volatile! nil) (volatile! nil))]
        (vreset! (.next marble) marble)
        (vreset! (.previous marble) marble)
        (->Circle 1 marble))))
  (remove [this]
    (let [^Marble old-current current
          ^Marble new-current (nextm current)]
      (vreset! (.previous new-current) (prevm old-current))
      (vreset! (.next (prevm old-current)) new-current)
      (->Circle (dec count) new-current)))
  (forward [this n]
    (if (= count 0)
      this
      (->Circle count (nth (iterate nextm current) n))))
  (backward [this n]
    (if (= count 0)
      this
      (->Circle count (nth (iterate prevm current) n))))

  clojure.lang.Counted
  (count [this]
    count)
  clojure.lang.Seqable
  (seq [this]
    (if (= 0 count)
      nil
      (loop [cnt count
             ^Marble cur @(.previous current)
             res ()]
        (if (= 0 cnt)
          res
          (recur (dec cnt)
                 @(.previous cur)
                 (cons (.number cur) res)))))))

(defn circle []
  (->Circle 0 nil))

(defn init-game [player-count]
  {:players (vec (repeat player-count 0))
   :marble 0
   :circle (circle)})

(defn play-marble [{:keys [^Circle circle marble players] :as game}]
  (if (and (> marble 0) (= 0 (mod marble 23)))
    (let [player (mod marble (count players))
          ^Circle circle (backward circle 7)
          score (.number (current circle))]
      (assoc game
             :players (update players player + marble score)
             :circle (remove circle)
             :marble (inc marble)))
    (-> game
        (update :marble inc)
        (update :circle forward 1)
        (update :circle insert marble))))

(defn play-rounds [game rounds]
  (nth (iterate play-marble game) rounds))

(defn max-score [player-count marble-count]
  (apply max (:players (play-rounds (init-game player-count) marble-count))))


(comment
  (max-scrore 9 25)
  (max-score 10 1618)
  (max-score 13 7999)

  ;; part 1

  (time
   (max-score 493 71863))
  "Elapsed time: 107.725309 msecs"
  367802

  ;; part 2

  (time
   (max-score 493 7186300))
  "Elapsed time: 16634.470548 msecs"
  2996043280

  )
