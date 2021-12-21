;(def start1 4)
;(def start2 8)
(def start1 1)
(def start2 3)

(def initial-dice 1)

(defn roll [dice acc]
  [dice (inc (mod dice 100)) (inc acc)]
)

(defn doTurn [pos score dice acc]
  (let [ [s1 d1 a1] (roll dice acc)
         [s2 d2 a2] (roll d1 a1)
         [s3 d3 a3] (roll d2 a2)
         new-pos (inc (mod (dec (+ pos s1 s2 s3)) 10))
         new-score (+ score new-pos)
       ]
    [new-pos new-score d3 a3]
  )
)

(defn loop1 [p1 s1 p2 s2 dice acc]
  (let [ [new-p1 new-s1 new-dice new-acc] (doTurn p1 s1 dice acc)
       ]
    (if (>= new-s1 1000)
      (* s2 new-acc)
      (recur p2 s2 new-p1 new-s1 new-dice new-acc)
    )
  )
)

(println (loop1 start1 0 start2 0 1 0))

