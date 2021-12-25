(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn resolv [regs key]
  (if (number? key)
    key
    (get regs key)
  )
)

(defn rang [a]
  (if (number? a)
    [a a]
    (:range a)
  )
)

(defn calc-range [a b op]
  (let [ ra (rang a)
         rb (rang b)
         combos (map #(op (first %) (second %)) (cross ra rb))
       ]
    [(apply min combos) (apply max combos)]
  )
)

(defn parse-add [ra rb]
  (if (= ra 0) rb
  (if (= rb 0) ra
  {:type :op, :fun +, :sym "+", :a ra, :b rb, :range (calc-range ra rb +)}
)))

(defn parse-mul [ra rb]
  (if (= ra 0) 0
  (if (= ra 1) rb
  (if (= rb 0) 0
  (if (= rb 1) ra
  {:type :op, :fun *, :sym "*", :a ra, :b rb, :range (calc-range ra rb *)}
)))))

(defn parse-div [ra rb]
  (if (= ra 0) 0
  (if (= rb 1) ra
  {:type :op, :fun quot, :sym "/", :a ra, :b rb, :range (calc-range ra rb quot)}
)))

(defn fix-mod-range [[mn mx]]
  [(max mn 0) (max mx 0)]
)

(defn parse-mod [ra rb]
  (if (= ra 0) 0
  (if (= rb 1) 0
  {:type :op, :fun mod, :sym "%", :a ra, :b rb, :range (calc-range ra rb mod) }
)))

(defn eql [a b]
  (if (= a b) 1 0)
)

(defn parse-eql [ra rb]
  (if (= ra rb) 1
  {:type :op, :fun mod, :sym "==", :a ra, :b rb, :range (calc-range ra rb eql) }
))

(defn parse-b [b]
  (case b
    nil nil
    "x" "x"
    "y" "y"
    "z" "z"
    "w" "w"
    (read-string b)
  )
)

(defn parse [[registers next-input-pos] line]
  (let [ [op a bb] (clojure.string/split line #" ")
         b (parse-b bb)
         ra (get registers a)
         rb (resolv registers b)
         new-val (case op
                   "inp" {:type :input, :pos next-input-pos, :range [1 9]}
                   "add" (parse-add ra rb)
                   "mul" (parse-mul ra rb)
                   "div" (parse-div ra rb)
                   "mod" (parse-mod ra rb)
                   "eql" (parse-eql ra rb)
                 )
         new-new-val (if (and (:range new-val) (apply = (:range new-val)))  (first (:range new-val)) new-val)
       ]
    [(assoc registers a new-new-val) (if (= "inp" op) (inc next-input-pos) next-input-pos)]
  )
)

(defn print-code [code var-names i]
  (if (number? code)
    [code var-names i]
    (let [ known (get var-names code)]
      (if (nil? known)
        (if (= (:type code) :input)
          (do
            (println (str "r" i) "= input" (:pos code) (:range code))
            [(str "r" i) (assoc var-names code i) (inc i)]
          )
          (let [ [aa v1 i1]  (print-code (:a code) var-names i)
                 [bb v2 i2]  (print-code (:b code) v1 i1)
               ]
            (println (str "r" i2) "=" aa (:sym code) bb (:range code))
            [(str "r" i2) (assoc v2 code i) (inc i2)]
          )
        )
        [(str "r" known) var-names i]
      )
    )
  )
)

(def parsed-code
  (reduce parse [{"x" 0, "y" 0, "z" 0, "w" 0} 0] input)
)

(println (first  (print-code (get (first parsed-code) "z")  {} 1)))
