(require '[clojure.data.priority-map :refer [priority-map]])

(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parseInt [str]
  (Integer/parseInt str)
)

(defn parseLine [line]
  (vec (map #(- (int %) (int \0)) line))
)

(def grid
  (vec (map parseLine input))
)

(defn neighbours [grid coord]
  (let [ y (first coord)
         x (second coord)
         ny (count grid)
         nx (count (first grid))
         nx+ (if (< x (dec nx)) [[y (inc x)]] [])
         ny+ (if (< y (dec ny)) (conj nx+ [(inc y) x]) nx+)
         nx- (if (> x 0) (conj ny+ [y (dec x)]) ny+)
         ny- (if (> y 0) (conj nx- [(dec y) x]) nx-)
        ]
    ny-
  )
)

(defn dijkstra2 [coord sourceVal todos grid]
  (let [ gridVal (get-in grid coord)
         maybeValue (+ sourceVal gridVal)
         currentVal (get todos coord Integer/MAX_VALUE)
       ]
    (if (< maybeValue currentVal)
       (assoc todos coord maybeValue)
       todos
    )
  )
)

(defn dijkstra [todo target grid]
  (let [ [coord myVal] (peek todo)]
    (if (= coord target)
      myVal
      (let [ friends (filter #(contains? todo %) (neighbours grid coord))
             rtodo (pop todo)
             newTodos (reduce #(dijkstra2 %2 myVal %1 grid) rtodo friends)
           ]
        (recur newTodos target grid)
      )
    )
  )
)

(def res
  (let [ ny (count grid)
         nx (count (first grid))
         allPoints (cross (range ny) (range nx))
         allInfinite (reduce #(assoc %1 %2 Integer/MAX_VALUE) (priority-map) allPoints)
         initialTodo (assoc allInfinite [0 0] 0)
         result (dijkstra initialTodo [(dec ny) (dec nx)] grid)
       ]
    result
  )
)

(println res)