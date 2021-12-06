(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(def inputs
  (map #(Integer/parseInt %) input)
)

(def sums
  (map #(+ %1 %2 %3) (butlast (butlast inputs)) (rest (butlast inputs) ) (rest (rest inputs)))
)

(def diff
  (map - ( rest sums ) (butlast sums))
)

(def cnt
  (count (filter pos? diff))
)

(print cnt)