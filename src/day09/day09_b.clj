(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parseLine [line]
  (vec (map #(- (int %) (int \0)) line))
)

(def inputs
  (vec (map parseLine input))
)

(defn my-some [pred coll]
  (true? (some pred coll))
)

(defn in? [coll elem]
  (my-some #(= elem %) coll)
)

(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(defn neighbours [matrix x y]
  (let [ ny (count matrix)
        nx (count (first matrix))
        nx- (if (> x 0) [[y (dec x)]] [])
        nx+ (if (< x (dec nx) ) (conj nx- [y (inc x)]) nx-)
        ny- (if (> y 0) (conj nx+ [(dec y) x]) nx+)
        ny+ (if (< y (dec ny)) (conj ny- [(inc y) x]) ny-)
        ]
    ny+
  )
)

(defn neighbourVals [matrix x y]
  (map #(get-in matrix %) (neighbours matrix x y))
)

(defn flood [acc edge matrix]
  (if (empty? edge) acc
    (let [ yx (first edge)
           value (get-in matrix yx)
           neig (neighbours matrix (second yx) (first yx))
           superEdge (concat (rest edge) neig)
          ]
        (if (in? acc yx)
          (flood acc (rest edge) matrix)
          (if (== 9 value)
            (flood acc (rest edge) matrix)
            (flood (conj acc yx) superEdge matrix)
          )
        )
      )
    )
  )


(defn minimum? [matrix yx]
  (let [ neigh (neighbourVals matrix (second yx) (first yx))
        ]
    (every? #(> % (get-in matrix yx)) neigh)
  )
)

(def res
  (let [ ny (count inputs)
         nx (count (first inputs))
         coords (cross (range 0 ny) (range 0 nx))
         startingPoints (filter #(minimum? inputs %) coords)
         sizes (map #(count (flood [] [%] inputs)) startingPoints)
         sortedSizes (reverse (sort sizes))
        ]
    (reduce * (take 3 sortedSizes))
  )
)

(println res)