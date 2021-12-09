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

(defn neighbours [matrix x y]
  (let [ ny (count matrix)
         nx (count (first matrix))
         nx- (if (> x 0) [(get-in matrix [y (dec x)])] [])
         nx+ (if (< x (dec nx) ) (conj nx- (get-in matrix [y (inc x)])) nx-)
         ny- (if (> y 0) (conj nx+ (get-in matrix [(dec y) x])) nx+)
         ny+ (if (< y (dec ny)) (conj ny- (get-in matrix [(inc y) x])) ny-)
        ]
    ny+
  )
)

(defn minimum? [matrix x y]
  (let [ neigh (neighbours matrix x y)
        ]
    (every? #(> % (get-in matrix [y x])) neigh)
  )
)

(def res
  (let [ ny (count inputs)
         nx (count (first inputs))
         coords (cross (range 0 nx) (range 0 ny))
         withMin (filter #(minimum? inputs (first %) (second %)) coords)
         nums (map #(get-in inputs [(second %) (first %)]) withMin)
         numsInc (map inc nums)
        ]
    (reduce + numsInc)
  )
)

(println res)