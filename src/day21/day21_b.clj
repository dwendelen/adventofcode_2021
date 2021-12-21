;(def start1 4)
;(def start2 8)
(def start1 1)
(def start2 3)

; [val cache]
(defn get-or-calc [cache key fun]
  (if (contains? cache key)
    [(get cache key) cache]
    (let [[new-val new-cache] (fun)]
      [new-val (assoc new-cache key new-val)]
    )
  )
)

; outcome = [pp1 pp2 pp3]
; [cache [dice cache]->outcome]->outcome
(defn roll [cache cnt]
  (let [ [pp1_1 pp2_1 c1] (cnt 1 cache)
         [pp1_2 pp2_2 c2] (cnt 2 c1)
         [pp1_3 pp2_3 c3] (cnt 3 c2)
       ]
    [(+ pp1_1 pp1_2 pp1_3) (+ pp2_1 pp2_2 pp2_3) c3]
  )
)

;[pos score cache [pos score cache]->outcome]->outcome
(defn do-turn [pos score cache cnt]
  (roll cache (fn [d1 c1]
    (roll c1 (fn [d2 c2]
      (roll c2 (fn [d3 c3]
        (let [ new-pos (inc (mod (dec (+ pos d1 d2 d3)) 10))
               new-score (+ score new-pos)
             ]
          (cnt new-pos new-score c3)
        )
      ))
    ))
  ))
)

(defn cached-loop1 [p1 s1 p2 s2 cache loop1]
  (let [[[p1 p2] new-cache] (get-or-calc cache [p1 s1 p2 s2] (fn [] (let [[p1 p2 cc] (loop1 p1 s1 p2 s2 cache)] [[p1 p2] cc] )))]
    [p1 p2 new-cache]
  )
)

; [pos score pos score cache] -> outcome
(defn loop1 [p1 s1 p2 s2 cache]
  (do-turn p1 s1 cache (fn [new-pos new-score cache1]
      (if (>= new-score 21)
        [1 0 cache1]
        (let [[pp2 pp1 cache2] (cached-loop1 p2 s2 new-pos new-score cache1 loop1)]
          [pp1 pp2 cache2]
        )
      )
    )
  )
)


(println (let [[p1 p2 _] (loop1 start1 0 start2 0 {})] (max p1 p2 )))

