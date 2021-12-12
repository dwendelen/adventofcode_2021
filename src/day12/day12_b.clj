(defn my-update [map key fun]
  (assoc map key (fun (get map key)))
)

(defn my-some [pred coll]
  (true? (some pred coll))
)

(defn in? [coll elem]
  (my-some #(= elem %) coll)
)

(defrecord Cave [name big neighbours])
(defrecord Path [names double])

(def input
  (line-seq (clojure.java.io/reader "input.txt"))
)

(defn createCave [name]
  (Cave. name (= name (clojure.string/upper-case name)) [])
)

(defn createIfNotExist [name caves]
  (if (contains? caves name)
    caves
    (assoc caves name (createCave name))
  )
)

(defn processCave [cave otherCave caves]
  (my-update caves cave (fn [c] (my-update c :neighbours #(conj % otherCave))))
)

(defn parse [caves line]
  (let [ mtch (re-matches #"(\w*)-(\w*)" line)
         name1 (get mtch 1)
         name2 (get mtch 2)
         caves1 (createIfNotExist name1 caves)
         caves2 (createIfNotExist name2 caves1)
         caves3 (processCave name1 name2 caves2)
         caves4 (processCave name2 name1 caves3)
       ]
    caves4
  )
)

(defn makesDouble [path cave]
  (and (in? (:names path) (:name cave)) (not (:big cave)))
)

(defn skipPath [cave path]
  (if (and (not (empty? (:names path))) (= "start" (:name cave)))
    true
    (and (:double path) (makesDouble path cave))
  )
)

(defn visitorNeighbours [caves cave path paths stuff]
  (reduce #(stuff caves %2 path %1) paths (:neighbours cave))
)

; returns paths
(defn stuff [caves caveName path paths]
  (let [cave (get caves caveName)
        newPath (Path. (conj (:names path) caveName) (or (:double path) (makesDouble path cave)))
        ]
     (if (= "end" caveName)
       (conj paths newPath)
       (if (skipPath cave path)
         paths
         (visitorNeighbours caves cave newPath paths stuff)
       )
     )
  )
)

(def caves
  (reduce parse {} input)
)

(def paths
  (stuff caves "start" (Path. [] false) [])
)

(println (count paths))
