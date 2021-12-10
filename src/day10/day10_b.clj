(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

; result:
; - EOL: ()
; - OK: (nextIdx)
; - Wrong: (char)

(defn res-eol [score]
  (fn [onEol _ _] (onEol score))
)

(defn res-ok [nextId]
  (fn [_ onOk _] (onOk nextId))
)

(def res-wrong
  (fn [_ _ onWrong] (onWrong))
)

(defn handleChunkBody [line idx closeOn point startNew]
  (if (= closeOn (get line idx))
    (res-ok (inc idx))
    (let [ resNext (startNew line idx)
         ]
      (resNext
        (fn [score] (res-eol (+ (* 5 score) point)))
        (fn [nextIdx]
          (handleChunkBody line nextIdx closeOn point startNew)
        )
        (fn [] res-wrong)
      )
    )
  )
)

(defn startNew [line idx]
  ( case (get line idx)
    nil (res-eol 0)
    \( (handleChunkBody line (inc idx) \) 1 startNew)
    \[ (handleChunkBody line (inc idx) \] 2 startNew)
    \{ (handleChunkBody line (inc idx) \} 3 startNew)
    \< (handleChunkBody line (inc idx) \> 4 startNew)
    res-wrong
  )
)

(defn takeMiddle [coll]
  (nth coll (/ (count coll) 2))
)

(defn calcScore [line]
  (let [ res (startNew line 0)
        ]
    (res
      (fn [score] score)
      (fn [_] 0)
      (fn [] 0)
    )
  )
)

(def res
  (takeMiddle (sort (filter #(not (zero? %)) (map calcScore input))))
)

(println res)