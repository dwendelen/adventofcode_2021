(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn to-bit [char]
  (if (= \# char) 1 0)
)

(def table
  (vec (map to-bit (first input)) )
)


(def initial-image
  (mapv #(mapv to-bit %) (rest (rest input)))
)

(defn toIdx [bits]
  (reduce #(+ (* %1 2) %2) bits)
)

(defn process2 [e11 e12 e13 r1 e21 e22 e23 r2 e31 e32 e33 r3 acc]
  (let [ idx (toIdx [e11 e12 e13 e21 e22 e23 e31 e32 e33])
         val (get table idx)
         new-acc (conj acc val)
       ]
    (if (empty? r1)
      new-acc
      (recur e12 e13 (first r1) (rest r1) e22 e23 (first r2) (rest r2) e32 e33 (first r3) (rest r3) new-acc)
    )
  )
)

(defn process1 [r1 r2 r3 rst acc emp]
  (let [ two-zeros (repeat 2 emp)
         e13 (first r1)
         rr1 (concat (rest r1) two-zeros)
         e23 (first r2)
         rr2 (concat (rest r2) two-zeros)
         e33 (first r3)
         rr3 (concat (rest r3) two-zeros)
         new-row (process2 emp emp e13 rr1 emp emp e23 rr2 emp emp e33 rr3 [])
         new-acc (conj acc new-row)
       ]
    (if (empty? rst)
      new-acc
      (recur r2 r3 (first rst) (rest rst) new-acc emp)
    )
  )
)

; [lines empty]
(defn process [lines emp]
  (let [ n (count lines)
         zeros (repeat n emp)
         first-line (first lines)
         rest-line (rest lines)
         two-zeros (repeat 2 zeros)
         others (concat rest-line two-zeros)
         new-line (process1 zeros zeros first-line others [] emp)
         new-empty (first table)
       ]
    [new-line new-empty]
  )
)


; not 5423 is too low
(let [ [p1 e1] (process initial-image 0)
       [p2 e2] (process p1 e1)
       m1 (map #(reduce + %) p2)
       r (reduce + m1)
     ]
  (println (count p2) (count (first p2)))
  (println r)
)

