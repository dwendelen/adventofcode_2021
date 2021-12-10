(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

; result:
; - EOL: ()
; - OK: (nextIdx)
; - Wrong: (char)

(def res-eol
  (fn [onEol _ _] (onEol))
)

(defn res-ok [nextId]
  (fn [_ onOk _] (onOk nextId))
)

(defn res-wrong [char]
  (fn [_ _ onWrong] (onWrong char))
)

(defn handleChunkBody [line idx closeOn startNew]
  (if (= closeOn (get line idx))
    (res-ok (inc idx))
    (let [ resNext (startNew line idx)
         ]
      (resNext
        (fn [] res-eol)
        (fn [nextIdx]
          (handleChunkBody line nextIdx closeOn startNew)
        )
        (fn [char] (res-wrong char))
      )
    )
  )
)

(defn startNew [line idx]
  ( case (get line idx)
    nil res-eol
    \( (handleChunkBody line (inc idx) \) startNew)
    \[ (handleChunkBody line (inc idx) \] startNew)
    \{ (handleChunkBody line (inc idx) \} startNew)
    \< (handleChunkBody line (inc idx) \> startNew)
    (res-wrong (nth line idx))
  )
)


(defn calcScore [line]
  (let [ res (startNew line 0)
       ]
    (res
        (fn [] 0)
        (fn [_] 0)
        (fn [char]
          (case char
            \) 3
            \] 57
            \} 1197
            \> 25137
          )
        )
    )
  )
)

(def res
  (reduce + (map calcScore input))
)

(println res)