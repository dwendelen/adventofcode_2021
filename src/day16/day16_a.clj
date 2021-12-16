(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(def inputs
  (first input)
  ;"A0016C880162017C3686B18A3D4780"
)

(defn hexToBits [hex]
  (case hex
    \0 [0 0 0 0]
    \1 [0 0 0 1]
    \2 [0 0 1 0]
    \3 [0 0 1 1]
    \4 [0 1 0 0]
    \5 [0 1 0 1]
    \6 [0 1 1 0]
    \7 [0 1 1 1]
    \8 [1 0 0 0]
    \9 [1 0 0 1]
    \A [1 0 1 0]
    \B [1 0 1 1]
    \C [1 1 0 0]
    \D [1 1 0 1]
    \E [1 1 1 0]
    \F [1 1 1 1]
  )
)

(def bits
  (mapcat hexToBits inputs)
)

; [num rest]
(defn parseNumber [bits length]
  [(reduce #(+ (* %1 2) %2) 0 (take length bits)) (drop length bits)]
)

(defn parseLiteralBlock [input acc]
  (let [ isLast (= 0 (first input))
         [number input2] (parseNumber (rest input) 4)
         newAcc (+ (* acc 16)  number)
       ]
    (if isLast
      [newAcc input2]
      (recur input2 newAcc)
    )
  )
)

; [packet rest]
(defn parseLiteral [version input]
  (let [[literal input2] (parseLiteralBlock input 0)]
    [{:type :literal, :version version, :number literal} input2]
  )
)

; item[]
(defn parseOperatorItems00 [input acc parsePacket]
  (if (empty? input)
    acc
    (let [[packet rest] (parsePacket input)]
      (recur rest (conj acc packet) parsePacket)
    )
  )
)

(defn parseOperatorItems0 [input parsePacket]
  (let [ [length input2] (parseNumber input 15)
         segment (take length input2)
         rest (drop length input2)
         items (parseOperatorItems00 segment [] parsePacket)
      ]
    [items rest]
  )
)

(defn parseOperatorItems11 [input acc parsePacket]
  (let [[item rest] (parsePacket input)] [(conj acc item) rest])
)

(defn parseOperatorItems1 [input parsePacket]
  (let [[amount input2] (parseNumber input 11)]
    (reduce (fn [[acc input] _] (parseOperatorItems11 input acc parsePacket)) [[] input2] (range amount))
  )
)

; [packet rest]
(defn parseOperator [version op input parsePacket]
  (let [ type (first input)
         [items input2] (if (== 0 type)
             (parseOperatorItems0 (rest input) parsePacket)
             (parseOperatorItems1 (rest input) parsePacket)
           )
       ]
    [{:type :operator, :operator op, :version version, :items items} input2]
  )
)

; [packet rest]
(defn parsePacket [input]
  (let [ [version input1] (parseNumber input 3)
         [type input2] (parseNumber input1 3)
       ]
    (case type
      4 (parseLiteral version input2)
      (parseOperator version type input2 parsePacket)
    )
  )
)

(defn getVersionSum [packet]
  (case (:type packet)
    :literal (:version packet)
    :operator (+ (:version packet) (reduce + (map getVersionSum (:items packet))))
  )
)

(println (getVersionSum (first (parsePacket bits))) )
