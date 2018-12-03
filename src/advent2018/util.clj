(ns advent2018.util
  (:import [java.io BufferedReader StringReader]))

(defn str-line-seq [input]
  (line-seq (BufferedReader. (StringReader. input))))
