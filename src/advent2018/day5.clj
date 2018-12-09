(ns advent2018.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]))

(def input (str/trim (slurp (io/resource "advent2018/day5.txt"))))

(defn react? [x y]
  (= 32 (Math/abs (- (long x) (long y)))))

(defn process-reactions [input]
  (loop [[x & input] input
         output ()]
    (let [previous (first output)]
      (if x
        (if (and previous (react? x previous))
          (recur input (next output))
          (recur input (conj output x)))
        (reverse output)))))

(time
 (count (process-reactions input)))
"Elapsed time: 20.407612 msecs"
;;=> 11720

;; part 2

(time
 (sequence (comp
            (map (fn [c] #{(char c) (char (- c 32))}))
            (map (fn [type?]
                   (remove type? input)))
            (map process-reactions)
            (map count)
            x/min)
           (range (long \a) (long \z))))
"Elapsed time: 514.169904 msecs"
;;=> (4956)
