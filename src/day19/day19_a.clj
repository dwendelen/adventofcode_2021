(defn cross [x y]
  (mapcat (fn [xx] (map (fn [yy] [xx yy]) y)) x)
)

(defn map-from-keys [fn keys]
  (reduce #(assoc %1 %2 (fn %2)) {} keys)
)

(def input
  (line-seq ( clojure.java.io/reader "input.txt"))
)

; resolved = {:points [[a b c]] :refs [ {:base [a b c], :rels [[a b c]]}]}
; unresolved = {:points [[a b c]] :refs-by-orien {orien [{:base [a b c], :rels [[a b c]]}]}}

(def scannerStringInputs
  (map rest (filter #(not= "" (first %)) (partition-by #(= "" %) input)))
)

(defn parseLineInput [line]
  (vec (map #(Integer/parseInt %) (clojure.string/split line #",")))
)

(defn parseScannerInput [lines]
  (vec (map parseLineInput lines))
)

(def scannerInputs
  (map parseScannerInput scannerStringInputs)
)

(def all-orientations
    ; Rotation = 1 swap + negate one of them
    ; Double rotation = two negates
  [
    [[0 1] [1 1] [2 1]]
    [[0 1] [1 -1] [2 -1]]
    [[0 1] [2 -1] [1 1]]
    [[0 1] [2 1] [1 -1]]
    [[0 -1] [1 1] [2 -1]]
    [[0 -1] [1 -1] [2 1]]
    [[0 -1] [2 1] [1 1]]
    [[0 -1] [2 -1] [1 -1]]
    [[1 1] [0 1] [2 -1]]
    [[1 1] [0 -1] [2 1]]
    [[1 1] [2 -1] [0 -1]]
    [[1 1] [2 1] [0 1]]
    [[1 -1] [0 1] [2 1]]
    [[1 -1] [0 -1] [2 -1]]
    [[1 -1] [2 1] [0 -1]]
    [[1 -1] [2 -1] [0 1]]
    [[2 1] [0 1] [1 1]]
    [[2 1] [0 -1] [1 -1]]
    [[2 1] [1 1] [0 -1]]
    [[2 1] [1 -1] [0 1]]
    [[2 -1] [0 1] [1 -1]]
    [[2 -1] [0 -1] [1 1]]
    [[2 -1] [1 1] [0 1]]
    [[2 -1] [1 -1] [0 -1]]
  ]
)

(defn subtract [point base]
  (mapv - point base)
)

(defn plus [point1 point2]
  (mapv + point1 point2)
)

(defn lte2 [_ _] true) ; trick to disable optimation

(defn lte [point1 point2]
  (do
      (if (empty? point1)
        true
        (if (= (first point1) (first point2))
          (recur (rest point1) (rest point2))
          (< (first point1) (first point2))
        )
      )
  )
)

(defn transform [coor orient]
  (do
    (mapv #(* (second %) (get coor (first %))) orient)
  )
)

(defn createReferenceOrNil [base points]
  (let [
         rel-points (mapv #(subtract % base) points)
         filtered (filter #(lte [0 0 0] %) rel-points)
         sorted (sort lte filtered)
       ]
    (if (>= (count sorted) 12)
      {:base base, :rel-points sorted}
      nil
    )
  )
)

(defn transformScannerInput2 [scanner orien]
  (let [
         points (map #(transform % orien) scanner)
         refs (map #(createReferenceOrNil % points) points)
         filtered (filter #(not (nil? %)) refs)
        ]
    filtered
  )
)

(defn transformScannerInput [scanner]
  (let [
         refs (map-from-keys #(transformScannerInput2 scanner %) all-orientations)
       ]
    {:points scanner, :refs-by-orien refs}
  )
)

(def scanners
  (map transformScannerInput scannerInputs)
)

; Assumes both lists are sorted with lte
(defn countOverlap [points1 points2 acc]
  (let [ item1 (first points1)
         item2 (first points2)
       ]
    (if (or (empty? points1) (empty? points2))
      acc
      (if (= item1 item2)
        (recur (rest points1) (rest points2) (inc acc))
        (if (lte item1 item2)
          (recur (rest points1) points2 acc)
          (recur points1 (rest points2) acc)
        )
      )
    )
  )
)

(defn findOverlap4 [points1 points2]
  (let [ count (countOverlap points1 points2 0)
       ]
    (>= count 12)
  )
)

; returns matching-ref1
(defn findOverlap3 [refs1 points2]
  (if (or (empty? refs1) (nil? points2))
    nil
    (let [
          ref1 (first refs1)
          points1 (:rel-points ref1)
          ]
      (if (findOverlap4 points1 points2)
        ref1
        (recur (rest refs1) points2)
      )
    )
  )
)

(defn findOverlap2a [refs1 refs2 orientation]
  (if (or (nil? refs2) (empty? refs2))
    nil
    (let [ ref2 (first refs2)
           points2 (:rel-points ref2)
           matching-ref1 (do (findOverlap3 refs1 points2))
          ]
      (if (nil? matching-ref1)
        (recur refs1 (rest refs2) orientation)
        [orientation matching-ref1 ref2]
      )
    )
  )
)

(defn findOverlap2 [refs1 refs-by-or2 orientations]
  (if (empty? orientations)
    nil
    (let [ orien (first orientations)
           refs2 (get refs-by-or2 orien)
           found (findOverlap2a refs1 refs2 orien)
         ]
      (if (nil? found)
        (recur refs1 refs-by-or2 (rest orientations))
        found
      )
    )
  )
)

;[orientation ref1 ref2] or nil
; orientation means: transformation to apply to ref2
(defn findOverlap [scanner1 scanner2]
  (findOverlap2 (:refs scanner1) (:refs-by-orien scanner2) all-orientations)
)

(defn resolveRef [ref pos]
  (if (nil? ref)
    nil
    (update ref :base #(plus % pos))
  )
)

(defn resolv [scanner pos orien]
  (let [ moved-points (map #(plus (transform % orien) pos) (:points scanner))
         refs (get (:refs-by-orien scanner) orien)
         refs2 (map #(resolveRef % pos) refs)
       ]
    {:points moved-points, :refs refs2 }
  )
)

(defn findRelPos [rel1 rel2]
  ; This is reversed because:
  ; 1 ----a---> P <-----b---- 2
  ; 2 = a + (-b) = a - b
  (subtract (:base rel1) (:base rel2))
)

(defn loop1 [resolved-done resolved resolved-rest unresolved-done unresolved-rest]
  (if (empty? unresolved-rest)
    (if (empty? resolved-rest)
      (conj resolved-done resolved)
      (recur (conj resolved-done resolved) (first resolved-rest) (rest resolved-rest) [] unresolved-done)
    )
    (let [ head (first unresolved-rest)
           tail (rest unresolved-rest)
           overlap (findOverlap resolved head)
          ]
      (if (nil? overlap)
        (recur resolved-done resolved resolved-rest (conj unresolved-done head) tail)
        (let [
               [o r1 r2] overlap
               rel-pos (findRelPos r1 r2)
               res (resolv head rel-pos o)
             ]
          (recur resolved-done resolved (conj resolved-rest res) unresolved-done tail)
        )
      )
    )
  )
)

(def all-matched
  (let [ init-pos [0 0 0]
        no-rot [[0 1] [1 1] [2 1]]
        res (resolv (first scanners) init-pos no-rot)
        ]
    (loop1 [] res [] [] (rest scanners))
  )
)

(def all-points
  (set (mapcat #(get % :points) all-matched))
)

(println (count all-points))
