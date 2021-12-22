(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parseLine [line]
  (let [ mtch (re-matches #"(on|off) x=(-?\d*)..(-?\d*),y=(-?\d*)..(-?\d*),z=(-?\d*)..(-?\d*)" line)
         on (= "on" (nth mtch 1))
         x1 (Integer/parseInt (nth mtch 2))
         x2 (Integer/parseInt (nth mtch 3))
         y1 (Integer/parseInt (nth mtch 4))
         y2 (Integer/parseInt (nth mtch 5))
         z1 (Integer/parseInt (nth mtch 6))
         z2 (Integer/parseInt (nth mtch 7))
        ]
    [[x1 x2 y1 y2 z1 z2] on]
  )
)

(def reverseBoxes
  (reverse (map parseLine input))
)

(def initial-space {:type :empty})

(defn box-to-space [box]
  (let [[[x1 x2 y1 y2 z1 z2] on] box]
    {:type :tree, :val x1, :comp 0,
      :left {:type :empty},
      :right {:type :tree, :val (inc x2) , :comp 0,
        :left {:type :tree, :val y1, :comp 1,
          :left {:type :empty},
          :right {:type :tree, :val (inc y2) , :comp 1,
            :left {:type :tree, :val z1, :comp 2,
              :left {:type :empty},
              :right {:type :tree, :val (inc z2), :comp 2,
                :left {:type (if on :on :off)}
                :right {:type :empty}
              }
            }
            :right {:type :empty}
          }
        }
        :right {:type :empty}
      }
    }
  )
)

(defn cut-tree-across [tree comp val cut-across]
  (if (= comp (:comp tree))
    (if (= val (:val tree))
      [(:left tree) (:right tree)]
      (if (< val (:val tree))
        (let [[c1 c2] (cut-across (:left tree) comp val)]
          [c1 (assoc tree :left c2)]
        )
        (let [[c1 c2] (cut-across (:right tree) comp val)]
          [(assoc tree :right c1) c2]
        )
      )
    )
    (let [ [ll lr] (cut-across (:left tree) comp val)
           [rl rr] (cut-across (:right tree) comp val)
         ]
      [(assoc (assoc tree :left ll) :right rl) (assoc (assoc tree :left lr) :right rr)]
    )
  )
)

(defn cut-across [space comp val]
  (case (:type space)
    :tree (cut-tree-across space comp val cut-across)
    [space space]
  )
)

(defn add [my-space other-space]
  (case (:type my-space)
    :empty other-space
    :tree (let [ [l r] (cut-across other-space (:comp my-space) (:val my-space))
                 up1 (update my-space :left add l)
                 up2 (update up1 :right add r)
               ]
             up2
          )
    my-space
  )
)

(defn update-box [box component side val]
  (update box component #(assoc % side val))
)

(defn num-on [space box]
  (case (:type space)
    :on (reduce * (map #(- (second %) (first %)) box))
    :off 0
    :empty 0
    :tree (+
      (num-on (:left space) (update-box box (:comp space) 1 (:val space)))
      (num-on (:right space) (update-box box (:comp space) 0 (:val space)))
    )
  )
)

(def space-after-input
  (reduce #(add %1 (box-to-space %2)) {:type :empty}  reverseBoxes)
)

(def initial-box [[Integer/MIN_VALUE Integer/MAX_VALUE] [Integer/MIN_VALUE Integer/MAX_VALUE] [Integer/MIN_VALUE Integer/MAX_VALUE]])

(println (num-on space-after-input initial-box))
