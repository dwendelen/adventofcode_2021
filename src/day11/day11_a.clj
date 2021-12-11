(def nbSteps 100)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parseLine [line]
  (vec (map #(- (int %) (int \0)) line))
)

(def inputs
  (vec (map parseLine input))
)

(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(defn neighbours [matrix yx]
  (let [ ny (count matrix)
         nx (count (first matrix))
         y (first yx)
         x (second yx)
         rangeX (range (dec x) (inc (inc x)))
         rangeY (range (dec y) (inc (inc y)))
         all (cross rangeY rangeX)
         filtered (filter #(and
                     (>= (first %) 0)
                     (>= (second %) 0)
                     (< (first %) ny)
                     (< (second %) nx)
                     (or (not= (first %) y) (not= (second %) x))
                   ) all)
        ]
      filtered
  )
)

(defn doStep [matrix todo flashed]
  (if (empty? todo)
    (let [ newMatrix (reduce #(assoc-in %1 %2 0) matrix flashed) ]
      {:matrix newMatrix, :nbFlashes (count flashed)}
    )
    (let [ yx (first todo)
           val (get-in matrix yx)
           incVal (inc val)
           flashTriggered (== incVal 10)
           newFlashed (if flashTriggered (conj flashed yx) flashed)
           newTodo (if flashTriggered
                     (concat (rest todo) (neighbours matrix yx))
                     (rest todo)
                   )
           newMatrix (assoc-in matrix yx incVal)
         ]
      (doStep newMatrix newTodo newFlashed)
    )
  )
)

(def allCoords
    (cross (range (count inputs)) (range (count (first inputs))))
)

(def res
  (reduce
    (fn [state _]
      (let [ stp (doStep (:matrix state) allCoords [])
            ]
        { :matrix (:matrix stp),
         :nbFlashes (+ (:nbFlashes state) (:nbFlashes stp))
         }
        )
      )
      { :matrix inputs, :nbFlashes 0 }
      (range nbSteps)
  )
)

(println (:nbFlashes res ))