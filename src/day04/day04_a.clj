(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn transpose [m]
  (apply mapv vector m)
)

(defn my-some [pred coll]
  (true? (some pred coll))
)

(defn in? [coll elem]
  (my-some #(== elem %) coll)
)

(defn parseInt [str]
  (Integer/parseInt (clojure.string/trim str))
)

(def numbers
  (map parseInt (clojure.string/split (first input) #","))
)

(defn parseLine [line]
  (map #(parseInt (clojure.string/join %)) (partition 2 3 line))
)

(def boards
  (partition 5 6 (map parseLine (rest (rest input))))
)

(defn hasRowComplete? [board numbers]
  (or
    (my-some
      (fn [row] (every? (fn [elm] (in? numbers elm)) row))
      board
    )
    (my-some
      (fn [row] (every? (fn [elm] (in? numbers elm)) row))
      (transpose board)
    )
  )
)

(defn findWinner [boards numbers]
  (if (empty? boards)
    nil
    (if (hasRowComplete? (first boards) numbers)
      (first boards)
      (findWinner (rest boards) numbers)
    )
  )
)

(defn boardScore [board numbers]
  (reduce + (filter #(not (in? numbers %) ) (flatten board)))
)

(defn solve [boards numHead numTail]
  (let [ winningBoard (findWinner boards numHead)]
    (if (nil? winningBoard)
        (solve boards (conj numHead (first numTail)) (rest numTail))
        (* (last numHead) (boardScore winningBoard numHead))
      )
  )
)

(def res
  (solve boards [] numbers)
)

(println res)