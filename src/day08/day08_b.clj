(require 'clojure.set)

(defn my-some [pred coll]
  (true? (some pred coll))
)

(defn in? [coll elem]
  (my-some #(= elem %) coll)
)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parseLine [line]
  (let [ parts (clojure.string/split line #" \| ")
         left (clojure.string/split (first parts) #" ")
         right (clojure.string/split (second parts) #" ")
        ]
    {:left (map set left) , :right (map set right)}
  )
)

(def inputs
  (map parseLine input)
)

(defn groupByCount [cnt groups]
  (first (filter #(== cnt (count %)) groups))
)

(defn charByCount [cnt freq]
  (first (first (filter #(== cnt (second %) ) freq)))
)

(defn solveLine [line]
  (let [ one (groupByCount 2 (:left line))
         four (groupByCount 4 (:left line))
         seven (groupByCount 3 (:left line))
         eight (groupByCount 7 (:left line))
         freq (frequencies (mapcat #(apply vector %) (:left line)))
         chars8 (map first (filter #(== 8 (second %)) freq))
         chars7 (map first (filter #(== 7 (second %)) freq))
         sa (first (clojure.set/difference seven one))
         sb (charByCount 6 freq)
         sc (first (filter #(not (= sa %)) chars8))
         sd (first (filter #(in? four %) chars7))
         se (charByCount 4 freq)
         sf (charByCount 9 freq)
         sg (first (filter #(not (= sd %)) chars7))
         two #{sa sc sd se sg}
         three #{sa sc sd sf sg}
         five #{sa sb sd sf sg}
         six #{sa sb sd se sf sg}
         seven #{sa sc sf}
         nine #{sa sb sc sd sf sg}
         zero #{sa sb sc se sf sg}
         numMatrix {one 1, two 2, three 3, four 4, five 5, six 6, seven 7, eight 8, nine 9, zero 0}
       ]
      (reduce #(+ (* 10 %1) %2) 0 (map #(get numMatrix %) (:right line)))
   )
)

(def res
  (reduce + (map solveLine inputs))
)

(println res)