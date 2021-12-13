(defn my-update [map key fun]
  (assoc map key (fun (get map key)))
)

(def input
  (line-seq (clojure.java.io/reader "input.txt"))
)

(defn parsePoint [line]
  (let [ mtch (re-matches #"(\d*),(\d*)" line)
        ]
    {"x" (Integer/parseInt (get mtch 1)), "y" (Integer/parseInt (get mtch 2))}
  )
)

(defn parseFold [line]
  (let [ mtch (re-matches #"fold along (x|y)=(\d*)" line)
        ]
    {:axis (get mtch 1), :value (Integer/parseInt (get mtch 2))}
  )
)

(def sections
  (split-with #(not= "" %) input)
)

(def points
  (set (map parsePoint (first sections)))
)

(def folds
  (map parseFold (rest (second sections)))
)

(defn newVal [oldVal ref]
  (if (>= oldVal ref)
    (- (* 2 ref) oldVal)
    oldVal
  )
)

(defn applyFoldToPoint [point fold]
  (my-update point (:axis fold) #(newVal % (:value fold)))
)

(defn applyFold [points fold]
  (set (map #(applyFoldToPoint % fold)  points))
)

(println (count (applyFold points (first folds))))
