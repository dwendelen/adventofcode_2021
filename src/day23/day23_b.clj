(require '[clojure.data.priority-map :refer [priority-map]])

(def input
  (line-seq ( clojure.java.io/reader "input_b.txt"))
)

(def inputs
  (mapv #(vec %) input)
)

(defn map-from-vals [vals fn]
  (reduce #(assoc %1 (fn %2) %2) {} vals)
)

(def cost
  {\A 1, \B 10, \C 100, \D 1000}
)

; Arena
; {:type :store, :expected A, :id 1, neighbours [3]}
; {:type :hall, :id 2, neighbours: [1, 3]}

; State
; - room -> code
(def arena-list
  (let [ r1 {:type :free-hall, :id 1, :neighbours [2]}
         r2 {:type :free-hall, :id 2, :neighbours [1 3]}
         r3 {:type :store-hall, :id 3, :neighbours [2 4 21]}
         r4 {:type :free-hall, :id 4, :neighbours [3 5]}
         r5 {:type :store-hall, :id 5, :neighbours [4 6 31]}
         r6 {:type :free-hall, :id 6, :neighbours [5 7]}
         r7 {:type :store-hall, :id 7, :neighbours [6 8 41]}
         r8 {:type :free-hall, :id 8, :neighbours [7 9]}
         r9 {:type :store-hall, :id 9, :neighbours [8 10 51]}
         r10 {:type :free-hall, :id 10, :neighbours [9 11]}
         r11 {:type :free-hall, :id 11, :neighbours [10]}
         a1 {:type :store, :expected \A, :id 21, :neighbours [3 22], :other-deeper-stores [22 23 24] }
         a2 {:type :store, :expected \A, :id 22, :neighbours [21 23], :other-deeper-stores [23 24] }
         a3 {:type :store, :expected \A, :id 23, :neighbours [22 24], :other-deeper-stores [24] }
         a4 {:type :store, :expected \A, :id 24, :neighbours [23], :other-deeper-stores [] }
         b1 {:type :store, :expected \B, :id 31, :neighbours [5 32], :other-deeper-stores [32 33 34] }
         b2 {:type :store, :expected \B, :id 32, :neighbours [31 33], :other-deeper-stores [33 34] }
         b3 {:type :store, :expected \B, :id 33, :neighbours [32 34], :other-deeper-stores [34] }
         b4 {:type :store, :expected \B, :id 34, :neighbours [33], :other-deeper-stores [] }
         c1 {:type :store, :expected \C, :id 41, :neighbours [7 42], :other-deeper-stores [42 43 44] }
         c2 {:type :store, :expected \C, :id 42, :neighbours [41 43], :other-deeper-stores [43 44] }
         c3 {:type :store, :expected \C, :id 43, :neighbours [42 44], :other-deeper-stores [44] }
         c4 {:type :store, :expected \C, :id 44, :neighbours [43], :other-deeper-stores [] }
         d1 {:type :store, :expected \D, :id 51, :neighbours [9 52], :other-deeper-stores [52 53 54] }
         d2 {:type :store, :expected \D, :id 52, :neighbours [51 53], :other-deeper-stores [53 54] }
         d3 {:type :store, :expected \D, :id 53, :neighbours [52 54], :other-deeper-stores [54] }
         d4 {:type :store, :expected \D, :id 54, :neighbours [53], :other-deeper-stores [] }
       ]
    [r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4]
  )
)

(def arena (map-from-vals arena-list :id))

(def target-state
  (into {} (map #(do [(:id %) (:expected %)]) (filter #(= (:type %) :store) arena-list)))
)

; [[room energy]]
(defn find-non-blocked [from prev energy cost state add-from]
  (let [ neigh-rooms (filter #(not= prev %) (:neighbours (get arena from)))
         filtered (filter #(not (contains? state %)) neigh-rooms)
         recur (mapcat #(find-non-blocked % from (+ energy cost) cost state true) filtered)
       ]
    (if add-from
      (conj recur [from energy])
      recur
    )
  )
)

(defn is-valid? [from to pod-id state]
  (let [ from-data (get arena from)
         to-data (get arena to)
       ]
    (case (:type to-data)
      :free-hall (= (:type from-data) :store)
      :store-hall false
      :store (let [ proper-type (= (:expected to-data) pod-id)
                    other-store-pods (map #(get state %) (:other-deeper-stores to-data))
                    type-match (every? #(= pod-id %) other-store-pods)
                  ]
               (and proper-type type-match)
             )
    )
  )
)

; [[room energy]]
(defn reachable [from energy pod-id state]
  (let [ cst (get cost pod-id)
         non-blocked (find-non-blocked from nil energy cst state false)
         filtered (filter #(is-valid? from (first %) pod-id state) non-blocked)
       ]
    filtered
  )
)

(defn new-move? [state-energy already-visited]
  (not (contains? already-visited (first state-energy)))
)

; [[state energy]]
(defn moves [room-id pod-id energy state already-visited]
  (let [
         state1 (dissoc state room-id)
         all-reachable (reachable room-id energy pod-id state1)
         mapped (map #(do [(assoc state1 (first %) pod-id) (second %)]) all-reachable)
         new-moves (filter #(new-move? % already-visited) mapped)
       ]
    new-moves
  )
)

(defn next-states [state energy already-visited]
  (mapcat #(moves (first %) (second %) energy state already-visited) state)
)

(defn print-rooms [rooms]
  (do
    (println "#############")
    (print "#")
    (print (get rooms 1 "."))
    (print (get rooms 2 "."))
    (print (get rooms 3 "."))
    (print (get rooms 4 "."))
    (print (get rooms 5 "."))
    (print (get rooms 6 "."))
    (print (get rooms 7 "."))
    (print (get rooms 8 "."))
    (print (get rooms 9 "."))
    (print (get rooms 10 "."))
    (print (get rooms 11 "."))
    (println "#")
    (print "###")
    (print (get rooms 21 "."))
    (print "#")
    (print (get rooms 31 "."))
    (print "#")
    (print (get rooms 41 "."))
    (print "#")
    (print (get rooms 51 "."))
    (println "###")
    (print "  #")
    (print (get rooms 22 "."))
    (print "#")
    (print (get rooms 32 "."))
    (print "#")
    (print (get rooms 42 "."))
    (print "#")
    (print (get rooms 52 "."))
    (println "#")
    (println "  #########")
    (println)
  )
)

(defn add-if-better [queue state energy]
  (let [v (get queue state)]
    (if (or (nil? v) (< energy v))
      (assoc queue state energy)
      queue
    )
  )
)

(defn loop1 [todo already-visited]
  (let  [ [state energy] (peek todo) ]
    (if (= target-state state)
      energy
      (let [ new-already-visited (conj already-visited state)
             neigh (next-states state energy new-already-visited)
             new-todo (reduce #(add-if-better %1 (first %2) (second %2)) (pop todo) neigh)
           ]
        (recur new-todo new-already-visited)
      )
    )
  )
)

(def initial-state
  { 21 (get-in inputs [2 3])
    22 (get-in inputs [3 3])
    23 (get-in inputs [4 3])
    24 (get-in inputs [5 3])
    31 (get-in inputs [2 5])
    32 (get-in inputs [3 5])
    33 (get-in inputs [4 5])
    34 (get-in inputs [5 5])
    41 (get-in inputs [2 7])
    42 (get-in inputs [3 7])
    43 (get-in inputs [4 7])
    44 (get-in inputs [5 7])
    51 (get-in inputs [2 9])
    52 (get-in inputs [3 9])
    53 (get-in inputs [4 9])
    54 (get-in inputs [5 9])
  }
)

(println (loop1 (assoc (priority-map) initial-state 0) #{}))