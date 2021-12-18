(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

; [tree restInput]
(defn parse2 [input]
  (let [ head (first input)]
    (if (= \[ head)
      (let [ [left input2] (parse2 (rest input))
             input3 (rest input2) ; Skip comma
             [right input4] (parse2 input3)
             input5 (rest input4) ; Skip closing bracket
           ]
        [[left right] input5]
      )
      [(- (int head) (int \0)) (rest input)]
    )
  )
)

(defn parse [input]
  (first (parse2 input))
)

(defn applyExplodeL [tree val]
  (if (nil? val)
    tree
    (if (vector? tree)
      [(applyExplodeL (first tree) val) (second tree)]
      (+ tree val)
    )
  )
)

(defn applyExplodeR [tree val]
  (if (nil? val)
    tree
    (if (vector? tree)
      [(first tree) (applyExplodeR (second tree) val)]
      (+ tree val)
    )
  )
)

; [toLeft newTree toRight]
(defn explode [tree depth]
  (if (vector? tree)
    (if (= 4 depth)
      [(first tree) 0 (second tree)]
      (let [ left (first tree)
             right (second tree)
             [toLL left1 toRL] (explode left (inc depth))
             right1 (applyExplodeL right toRL)
             [toLR right2 toRR] (explode right1 (inc depth))
             left2 (applyExplodeR left1 toLR)
           ]
        [toLL [left2 right2] toRR]
      )
    )
    [nil tree nil]
  )
)

; [newTree split?]
(defn split [tree]
  (if (vector? tree)
    (let [ left (first tree)
           right (second tree)
           [left1 isLeftSplit] (split left)
         ]
      (if isLeftSplit
        [[left1 right] true]
        (let [[right1 isRightSplit] (split right)]
          [[left right1] isRightSplit]
        )
      )
    )
    (if (>= tree 10)
      (let [ newL (quot tree 2)
             newR (quot (inc tree ) 2)
           ]
        [[newL newR] true]
      )
      [tree false]
    )
  )
)

(defn redu [tree]
  (let [ [_ exp _] (explode tree 0)
         [spl isSpl] (split exp)
       ]
    (if isSpl
      (recur spl)
      spl
    )
  )
)

(defn snailAdd [tree1 tree2]
  (redu [tree1 tree2])
)

(defn magnitude [tree]
  (if (vector? tree)
    (+ (* 3 (magnitude (first tree))) (* 2 (magnitude (second tree))))
    tree
  )
)

(def res
  (magnitude (reduce snailAdd (map parse input)) )
)

(println res)
