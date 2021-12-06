(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parse [line]
  (let [mtch (re-matches #"(\w*) (.*)" line)]
    [
      (nth mtch 1)
      (Integer/parseInt (nth mtch 2))
    ]
  )
)

(def commands
  (map parse input)
)

(def init {:x 0, :y 0})

(defn step [state cmd]
  (case (get cmd 0)
    "forward" (assoc state :x (+ (:x state) (get cmd 1)))
    "up" (assoc state :y (- (:y state) (get cmd 1)))
    "down" (assoc state :y (+ (:y state) (get cmd 1)))
  )
)

(def res (reduce step init commands))

(println (* (:x res) (:y res) ))