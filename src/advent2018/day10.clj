(ns advent2018.day10
  (:require [advent2018.util :as util]
            [clojure.java.io :as io]
            [trikl.core :as t]))

(def input (util/line-reader-resource (io/resource "advent2018/day10.txt")))
(def test-input (util/line-reader-resource (io/resource "advent2018/day10_test.txt")))

(def line-pattern #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")

(defn parse-line [l]
  (let [[_ x y x' y'] (re-find line-pattern l)]
    (into [] (map #(Long/parseLong %)) [x y x' y'])))

(def clients (atom []))

(def stop! (t/start-server #(swap! clients conj %)))
#_(stop!)

(def points (atom (map parse-line input)))
(def ticks (atom 0))

(defn move-point [[x y vx vy]]
  [(+ x vx) (+ y vy) vx vy])

(defn move-points [points]
  (map move-point points))

(defn tick! []
  (swap! points move-points)
  (swap! ticks inc))

(defn app [_ _]
  (let [points @points]
    [:box
     [:box {:x 0 :y 0}
      (prn-str {:min-x (apply min (map first points))
                :min-y (apply min (map second points))
                :max-x(apply max (map first points))
                :max-y (apply max (map second points))})]
     (for [[x y vx vy] points]
       (if (and (>= x 0) (>= y 0))
         [:box {:x x :y y} "x"] ;; 134/90 added to center the final result a bit more
         ""))]))

(defn render! []
  (run! #(t/render % [app]) @clients))

;; part 1

(render!)

(comment
  ;; telnet localhost 1357
  (t/add-listener (last @clients)
                  ::input
                  (fn [{:keys [key]}]
                    (when (= :space key)
                      (tick!)
                      (render!))))

  (def stop? (atom false))
  (def points (atom (map parse-line input)))

  (reset! stop? true)
  (reset! stop? false)

  ;; I let this run until it was fairly close together, then stepped through it
  ;; manually with spacebar
  (future
    (while (and (not @stop?) (> (apply max (map first @points)) 1000))
      (tick!)
      (render!)
      ))
  )

;; part 2

(comment
  (loop [points' @points]
    (let [stats {:min-x (apply min (map first points'))
                 :min-y (apply min (map second points'))
                 :max-x(apply max (map first points'))
                 :max-y (apply max (map second points'))}]
      (if (= stats {:min-x 144, :min-y 100, :max-x 205, :max-y 109}) ;; copied this map from the visualization after it completed
        @ticks
        (do
          (tick!)
          (recur @points))))))

(time
 (render!))
