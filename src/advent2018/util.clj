(ns advent2018.util
  (:import [java.io BufferedReader StringReader])
  (:require [clojure.java.io :as io]))

(defn str-line-seq [input]
  (line-seq (BufferedReader. (StringReader. input))))

(defn line-reader-resource [resource]
  (let [resource (io/as-url resource)]
    (reify clojure.lang.IReduceInit
      (reduce [this f init]
        (let [rdr (io/reader resource)]
          (try
            (loop [state init]
              (if (reduced? state)
                @state
                (if-let [line (.readLine rdr)]
                  (recur (f state line))
                  state)))
            (finally
              (.close rdr)))))

      java.lang.Iterable
      (iterator [this]
        (let [rdr (io/reader resource)
              next-line (volatile! nil)]
          (reify java.util.Iterator
            (hasNext [this]
              (when-not @next-line
                (vreset! next-line (.readLine rdr)))
              (boolean @next-line))
            (next [this]
              (when (.hasNext this)
                (let [line @next-line]
                  (vreset! next-line nil)
                  line)))))))))
