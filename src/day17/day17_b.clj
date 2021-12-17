;(def minX 20)
;(def maxX 30)
;(def minY -10)
;(def maxY -5)

(def minX 240)
(def maxX 292)
(def minY -90)
(def maxY -57)

(def maxInitialYVelocity (- minY))
(def maxInitialXVelocity maxX)

(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(defn simulate [x y dx dy]
  (let [ xx (+ x dx)
         yy (+ y dy)
         dxx (max 0 (dec dx))
         dyy (dec dy)
       ]
    (if (or (> x maxX) (< y minY))
      false
      (if (and (>= x minX) (<= x maxX) (>= y minY) (<= y maxY))
        true
        (recur xx yy dxx dyy)
      )
    )
  )
)

(def res
  (filter #(simulate 0 0 (first %) (second %)) (cross (range (inc maxInitialXVelocity)) (range (dec minY) (inc maxInitialYVelocity))))
)

(println (count res) )