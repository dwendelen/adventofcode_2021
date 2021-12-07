(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

(defn parse [line]
  (let [ mtch (re-matches #"(\d*),(\d*) -> (\d*),(\d*)" line)
         mapped (map #(Integer/parseInt %) (rest mtch) )
        ]
    (partition 2 mapped)
  )
)

(def inputs
  (map parse input)
)

(defn cross [x y]
   (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(defn sign [x] (if (pos-int? x) 1 -1 ))

(defn toBlocks [from to]
  (let [xf (first from)
        yf (second from)
        xt (first to)
        yt (second to)
        minX (min xf xt)
        maxX (max xf xt)
        minY (min yf yt)
        maxY (max yf yt)
        xRange (range minX (inc maxX))
        yRange (range minY (inc maxY))
        sig (* (sign (- xt xf)) (sign (- yt yf)))
        yStart (if (pos-int? sig) minY maxY)
        ]
    (if (or (== xf xt) (== yf yt))
      (cross xRange yRange) ; line
      (map #(vector % (+ (* sig (- % minX)) yStart)) xRange)  ; diagonal
    )
  )
)

(def points
  (mapcat #(toBlocks
             (first %)
             (second %))
          inputs)
)

(println (count (filter #(>= (second %) 2) (frequencies points))))
