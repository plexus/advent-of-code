(ns advent.day10)

(def input [83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100])
(def input-2 "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100")

(defn rotate [n coll]
  (let [n (mod n (count coll))]
    (concat (drop n coll)
            (take n coll))))

(rotate 3 (range 10))                          ;;=> (3 4 5 6 7 8 9 0 1 2)
(rotate -1 (range 10))                         ;;=> (9 0 1 2 3 4 5 6 7 8)

(defn reverse-range [n coll]
  (concat (reverse (take n coll))
          (drop n coll)))

(reverse-range 5 (range 10))                   ;;=> (4 3 2 1 0 5 6 7 8 9)

(defn knot [[pos skip list] length]
  [(+ pos skip length)
   (inc skip)
   (->> list
        (rotate pos)
        (reverse-range length)
        (rotate (- pos)))])

(defn solve-1 [list lengths]
  (reduce knot
          [0 0 list]
          lengths))

(solve-1 (range 5) [3 4 1 5])
;;=> ([0 0 (0 1 2 3 4)] [3 1 (2 1 0 3 4)] [8 2 (4 3 0 1 2)] [11 3 (4 3 0 1 2)] [19 4 (3 4 2 1 0)])
;;=> ([0 0 (0 1 2 3 4)] [3 1 (2 1 0 3 4)] [5 2 (4 3 0 1 2)] [3 3 (4 3 0 1 2)] [8 4 (4 2 1 0 3)])
;;=> [8 4 (4 2 1 0 3)]
;;=> [8 4 (4 2 1 0 3)]

(solve-1 (range 256) input)
;;=> [1807 16 (92 218 219 220 221 222 223 224 225 252 251 250 249 248 247 246 245 244 243 242 241 240 239 238 237 236 235 234 233 232 231 230 229 228 227 226 253 254 255 82 81 80 79 78 77 75 130 129 128 127 126 125 124 123 122 121 120 119 49 48 47 46 145 144 143 142 141 140 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 50 51 52 53 54 55 57 56 58 59 40 39 38 64 63 62 83 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 65 66 67 68 69 70 71 72 73 74 76 131 132 133 91 90 89 88 87 86 85 84 61 60 41 42 43 44 45 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 93 94 95 96 97 98 99 139 138 137 136 135 134)]

(* 92 218)                                     ;;=> 20056


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

(def lengths (concat (map long input-2) [17, 31, 73, 47, 23]));;=> #'advent.day10/lengths

lengths                                        ;;=> (56 51 44 48 44 49 57 51 44 49 44 50 53 52 44 50 51 55 44 49 56 55 44 52 48 44 56 56 44 50 55 44 50 44 50 53 53 44 49 52 57 44 50 57 44 52 50 44 49 48 48 17 31 73 47 23)

(->> lengths
     (repeat 64)
     (apply concat)
     (solve-1 (range 256))
     last
     (partition 16)
     (map (partial apply bit-xor))
     (map (partial format "%02x"))
     (apply str))
;;=> "d9a7de4a809c56bf3a9465cb84392c8e"
