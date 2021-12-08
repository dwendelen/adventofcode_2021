(def iterations 256)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(def numbers
  (map #(Integer/parseInt %) (clojure.string/split (first input) #","))
)

(def initial-counters
  (frequencies numbers)
)

(defn do-step [state _]
  ( let [ state8 (assoc state 8 (get state 0 0))
          state7 (assoc state8 7 (get state 8 0))
          state6 (assoc state7 6 (+ (get state 7 0) (get state 0 0)))
          state5 (assoc state6 5 (get state 6 0))
          state4 (assoc state5 4 (get state 5 0))
          state3 (assoc state4 3 (get state 4 0))
          state2 (assoc state3 2 (get state 3 0))
          state1 (assoc state2 1 (get state 2 0))
          state0 (assoc state1 0 (get state 1 0))
         ]
      state0
  )
)

(def res
  (reduce + 0 (vals (reduce do-step initial-counters (range iterations)) ))
)


(println res)

