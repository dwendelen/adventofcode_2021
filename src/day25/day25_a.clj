(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parse-line [line]
  (mapv #(case % \> 1 \v 0 \. nil) line)
)

(def initial-grid
  (mapv parse-line input)
)

(def ny
  (count initial-grid)
)

(def nx
  (count (first initial-grid))
)

(def n [ny nx])

(def empty-grid
  (vec (repeat ny (vec (repeat nx nil))))
)

(defn print-grid [grid]
  (doseq [line grid]
    (doseq [item line]
      (case item
        nil (print ".")
        0 (print "v")
        1 (print ">")
      )
    )
    (println)
  )
  (println)
)

(defn handle-item [item new-grid old-grid turn]
  (let [ current (get-in old-grid item)
       ]
    (if (nil? current)
      new-grid
      (let [ dest (update item current #(mod (inc %) (get n current)))
             dest-item (get-in old-grid dest)
           ]
        (if (and (nil? dest-item) (= current turn))
          (assoc-in new-grid dest current)
          (assoc-in new-grid item current)
        )
      )
    )
  )
)

(defn loop1 [grid i]
  (let [ items (cross (range ny) (range nx))
         new-grid1 (reduce #(handle-item %2 %1 grid 1) empty-grid items)
         new-grid2 (reduce #(handle-item %2 %1 new-grid1 0) empty-grid items)
         new-i (inc i)
       ]
    (print-grid grid)
    (if (= grid new-grid2)
      new-i
      (recur new-grid2 new-i)
    )
  )
)

(println initial-grid)
(println empty-grid)

(println (loop1 initial-grid 0))