(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parse [line]
  (let [ mtch (re-matches #"(\d*),(\d*) -> (\d*),(\d*)" line)
         mapped (map #(Integer/parseInt %) (rest mtch) )
        ]
    (partition 2 mapped)
  )
)

(def inputs
  (filter #(let [
                 p1 (first %)
                 p2 (second %)
                 x1 (first p1)
                 y1 (second p1)
                 x2 (first p2)
                 y2 (second p2)
   ] (or (== x1 x2) (== y1 y2)))
          (map parse input))
)

(defn cross [x y]
   (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(defn toBlocks [from to]
  (let [ minX (min (nth from 0) (nth to 0))
        maxX (max (nth from 0) (nth to 0))
        minY (min (nth from 1) (nth to 1))
        maxY (max (nth from 1) (nth to 1))
        xRange (range minX (inc maxX))
        yRange (range minY (inc maxY))
       ]
    (cross xRange yRange)
  )
)

(def points
  (mapcat #(toBlocks
             (nth % 0)
             (nth % 1))
          inputs)
)

(filter #(>= (second %) 2) (frequencies points))
(println (count (filter #(>= (second %) 2) (frequencies points)))  )