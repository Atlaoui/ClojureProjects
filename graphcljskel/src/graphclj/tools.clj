(ns graphclj.tools
  (:require [clojure.string :as str]
     [graphclj.centrality :as central]
     [clojure.set :as set]))

(declare mapR-fromS)
(declare sort-rank)
(declare cumul-rank)
(declare pre-parsage-dot)
(declare  not-in?)
(declare  ntoString)
(declare colo-strFormat)
(declare c-t)
(declare generate-colors)
(declare readfile)
(declare rank-nodes)
(declare to-dot)
(declare color-map)
(declare colo-strFormat)
(declare link-to-str)
(declare to-string)
(declare parti-dot)

(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))

(defn rank-nodes
    "Ranks the nodes of the graph in relation to label l in accending order"
  [g,l]
  (loop [cpt 0 taille (count g) sortedR (sort-rank g l) res{}]
    (if (< cpt taille)
      (if (get g cpt)
       (recur (inc cpt) taille sortedR (assoc res cpt (merge (get g cpt) {:rank (get sortedR (l (get g cpt)))})))
       (recur (inc cpt) taille sortedR res))
     res)))


(defn generate-colors [n]
  (let [step 10]
    (loop [colors {}, current [255.0 160.0 122.0], c 0]
      (if (= c (inc n))
        colors
        (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
               (map #(mod (+ step %) 255) current) (inc c))))))



(defn to-dot
  "Returns a string in dot format for graph g, each node is colored in relation to its ranking"
   [g]
  (str "graph g{ "(color-map g)" " (reduce str (link-to-str g)) "}"))





;;------------------------------------------------Tools---------------------------

(defn color-map
  [g]
  (let [taille (count g)]
    (loop [i 0  res ""]
      (if (< i taille)
          (if (get g i)
           (recur (inc i) (str res (colo-strFormat i (:rank (get g i)))))
           (recur (inc i) res))
        res))))




(defn colo-strFormat
  [n rank]
  (str n" [style=filled color=" (c-t (get (generate-colors rank) rank)) "] "))


(defn c-t
   [st]
  (loop [s st res ""]
        (if (seq s)
            (recur (rest s) (str res " " (first s)))
         res)))




(defn link-to-str
  "convertie les lien en str"
  [graph]
 (let [taille (count graph)]
   (loop [i 0  res () visited []]
     (if (< i taille)
         (if (get graph i)
          (recur (inc i) (concat res (to-string i (:neigh (get graph i)) visited)) (distinct (concat visited (:neigh (get graph i)))))
          (recur (inc i) res visited))
       res))))


(defn to-string
      [node sett visited]
     (loop [c sett res ()]
       (if (seq c)
         (if (not (graphclj.centrality/visited? (first c) visited))
           (recur (rest c) (conj res (ntoString node (first c))))
          (recur (rest c) res))
        res)))




(defn ntoString
  "node ling ==> node--link"
  [node link]
  (str node"--"link" "))

(defn filter-seq
  [se]
  (loop [s se resProper ()]
      (if (seq s)
        (if (not-in? resProper (first s))
          (recur (rest s) (conj resProper (first s)))
         (recur (rest s) resProper))
       resProper)))


(defn big-seq
  "graph => (2 0)...."
  [g]
  (loop [pos (keys g)  res ()]
        (if (seq pos)
          (recur (rest pos) (concat res (parti-dot (first pos) (:neigh (get g (first pos))))))
         res)))

(defn not-in?
  "((2 0) (0 4) (0 1)) (0 2) => true"
  [coll elem]
  (let [elemInv (reverse elem)]
   (loop [index coll]
     (if (seq index)
       (if (or (= (first index) elem) (= (first index) elemInv))
         false
         (recur (rest index)))
      true))))

(defn parti-dot
  [val setkey]
  (partition 2 (into () (interleave setkey (repeat val)))))


(defn sort-rank
  "give all the keys by a special label"
   [g,l]
  (mapR-fromS (sort-by first > (frequencies (map l (vals g))))))



(defn mapR-fromS
    "(1 2) => {1 0 ,2 1}"
    [s]
    (loop [seqV s res {}]
      (if (seq seqV)
        (recur (rest seqV) (conj res {(ffirst seqV) (cumul-rank (rest seqV))}))
        res)))



(defn cumul-rank
  "([3 1] [2 2] [1 1]) = 4"
  [coll]
  (loop [c coll sommeR 0]
    (if (seq c)
      (recur (rest c) (+ sommeR (second (first c))))
      sommeR)))
