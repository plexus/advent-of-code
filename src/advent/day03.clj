(ns advent.day03)

;; The numbers form squares of n^2 numbers, with n being odd. This returns the
;; largest square whose size is still smaller than the input.
(defn contained-square-size [input]
  (last
   (take-while
    #(< % input)
    (map #(* % %)
         (iterate (comp inc inc) 1)))))

(contained-square-size 10)                     ;;=> 9
(contained-square-size 25)                     ;;=> 9
(contained-square-size 26)                     ;;=> 25

(let [input 265149
      square (contained-square-size input)
      square-side (int (Math/sqrt square))

      ;; distance to step to corner of the `square`
      square-steps (dec square-side)

      ;; distance to step to the center tile on the edge of the square
      center-steps (int (/ square-steps 2))

      ;; steps between square corner and center tile on edge
      side-center (int (Math/ceil (/ square-side 2)))

      ;; numeric distance between input and the square's size, normalized to be
      ;; on the right side of the square
      remaining-steps (mod (- input square) (inc square-side))

      ;; steps to add to get from the middle of the edge of the inner square to
      ;; the given input tile
      distance-from-middle (mod remaining-steps side-center)
      ]
  {:input input
   :square square
   :square-side square-side
   :square-steps square-steps
   :remaining-steps remaining-steps
   :distance-from-middle distance-from-middle
   :result (+ center-steps 1 distance-from-middle)})
