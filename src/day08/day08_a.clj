(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parseLine [line]
  (let [ parts (clojure.string/split line #" \| ")
         left (clojure.string/split (first parts) #" ")
         right (clojure.string/split (second parts) #" ")
        ]
    {:left left, :right right}
  )
)

(def inputs
  (map parseLine input)
)

(defn solveLine [line]
  (let [ one (count (filter #(== 2 (count %)) (:right line)))
       four (count (filter #(== 4 (count %)) (:right line)))
       seven (count (filter #(== 3 (count %)) (:right line)))
       eight (count (filter #(== 7 (count %)) (:right line)))
       ]
      (+ one four seven eight)
   )
)

(def res
  (reduce + (map solveLine inputs))
)

(println res)