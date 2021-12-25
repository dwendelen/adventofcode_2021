(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(def blocks
  (partition 18 input)
)

(defn fetch-num [lines idx]
  (Integer/parseInt (subs (nth lines idx) 6))
)

(def vars
  (map #(do [(fetch-num % 4) (fetch-num % 5) (fetch-num % 15)]) blocks)
)

(doseq [x vars]
  (println x)
)

; Derived from analysing input
;   w = inp
;   rest = z % 26
;   aaa = z / _a
;
;   z = if(rest + _b == w) {
;     aaa
;   } else {
;     26 * aaa + w + _c
;   }
;
(defn algo [inp vars acc]
  (if (empty? inp)
    acc
    (let [ in (- (int (first inp)) (int \0))
           [a b c] (first vars)
           rst (mod acc 26)
           head (quot acc a)
           new-acc (if (= (+ rst b) in)
                     head
                     (+ (* 26 head) in c)
                   )
         ]
      (algo (rest inp) (rest vars) new-acc)
    )
  )
)
