(defn map-map [fn map]
  (reduce #(update %1 %2 fn) map (keys map))
)

(def depth 40)

(def input
  (line-seq (clojure.java.io/reader "input.txt"))
)

(def sections
  (split-with #(not= "" %) input)
)

(def inputs
  (first (first sections))
)

(defn parseRule [line]
  (let [ mtch (re-matches #"(\w)(\w) -> (\w)" line)
        ]
    {:start (first (get mtch 1)), :stop (first (get mtch 2)), :result (first (get mtch 3))}
  )
)

(defn mapUpper [list]
  (let [ grouped (group-by :stop list)
         mapped (map-map #(:result (first %)) grouped)
       ]
    mapped
  )
)

(def rules
  (let [ parsed (map parseRule (rest (second sections)))
         grouped1 (group-by :start parsed)
         grouped2 (map-map mapUpper grouped1)
       ]
    grouped2
  )
)

(defn resolve2 [start stop depth cache rules resolve]
  (if (zero? depth)
    [{} cache]
    (let [ lookup (get-in rules [start stop])
           [part1 cache1] (resolve start lookup (dec depth) cache rules)
           [part2 cache2] (resolve lookup stop (dec depth) cache1 rules)
           lookupMap {lookup 1}
           result (merge-with + part1 part2 lookupMap)
           newCache (assoc cache2 [start stop depth] result)
         ]
      [result newCache]
    )
  )
)

(defn resolve1 [start stop depth cache rules]
  (let [ val (get cache [start stop depth])
       ]
    (if (nil? val)
      (resolve2 start stop depth cache rules resolve1)
      [val cache]
    )
  )
)

(defn oneStep [[start freq cache] stop]
  (let [ [freq2 newCache] (resolve1 start stop depth cache rules)
         combined (merge-with + freq freq2)
       ]
    [stop combined newCache]
  )
)

(def res
  (let [[_, frq, _] (reduce oneStep [(first inputs) (frequencies inputs) {}] (rest inputs) )
         mx (reduce max (vals frq))
         mn (reduce min (vals frq))
        ]
    (- mx mn)
  )
)

(println res)