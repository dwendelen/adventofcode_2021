(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn transpose [m]
  (apply mapv vector m))

(defn invertDigit [digit]
  (case digit
    \0 \1
    \1 \0
  )
)

(defn solveDigit [digits]
  (let [freq (frequencies digits)
       d0 (freq \0)
       d1 (freq \1)
       ]
    (if (> d0 d1) \0 \1)
  )
)

(defn digitsToInt [digits]
  (reduce
    #(+ (* 2 %1) (case %2 \0 0 \1 1))
    0
    digits
  )
)

(def gamma
  (map solveDigit (transpose input))
)

(def epsilon
  (map invertDigit gamma)
)

(println (* (digitsToInt gamma) (digitsToInt epsilon)))