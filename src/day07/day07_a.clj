(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn abs [n] (max n (- n)))

(def numbers
  (map #(Integer/parseInt %) (clojure.string/split (first input) #","))
)

(defn calcScore [num]
  (reduce + (map #(abs (- num %)) numbers))
)

(def scores
  (map calcScore (range (inc (reduce max numbers))))
)

(def res
  (reduce min scores)
)

(println res)