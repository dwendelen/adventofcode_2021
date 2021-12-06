(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
  )

(defn bitCriteria [listOfList idx fun]
  (let [digits (map #(get % idx) listOfList)
        freq (frequencies digits)
        d0 (get freq \0 0)
        d1 (get freq \1 0)
       ]
     (if (fun d0 d1) \0 \1)
   )
)

(def mostCommon >)
(def leastCommon <=)

(defn digitsToInt [digits]
  (reduce
    #(+ (* 2 %1) (case %2 \0 0 \1 1))
    0
    digits
  )
)

(defn looper [inp idx fun]
  (let [ cret    (bitCriteria inp idx fun)
         newList (filter #(= (get % idx) cret) inp)
        ]
    (if (== (count inp) 1)
      (nth inp 0)
      (looper newList (inc idx) fun)
    )
  )
)



;(println (bitCriteria input 0 mostCommon))
(println (*
 (digitsToInt (looper input 0 mostCommon))
 (digitsToInt (looper input 0 leastCommon))
))

