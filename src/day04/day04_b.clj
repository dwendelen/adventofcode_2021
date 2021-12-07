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

(defn findFirstNotWinner [boards numbers]
  (if (empty? boards)
    nil
    (if (hasRowComplete? (first boards) numbers)
      (findFirstNotWinner (rest boards) numbers)
      (first boards)
    )
  )
)

(defn boardScore [board numbers]
  (reduce + (filter #(not (in? numbers %) ) (flatten board)))
)

(defn solve [boards numHead numTail lastBoard]
  (let [ firstNotWinningBoard (findFirstNotWinner boards numHead)
         nextNumHead (conj numHead (first numTail))
        ]
    (if (nil? firstNotWinningBoard)
      (* (last numHead) (boardScore lastBoard numHead))
      (solve boards nextNumHead (rest numTail) firstNotWinningBoard)
    )
  )
)

(def res
  (solve boards [] numbers nil)
)

(println res)