(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(def inputs
  (map #(Integer/parseInt %) input)
)

(def diff
  (map - ( rest inputs ) (butlast inputs))
)

(def cnt
  (count (filter pos? diff))
)

(print cnt)