(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set]))

(declare visited?)
(declare find-neighbors)
(declare neighbors-direct?)
(declare vals-by-label)
(declare degrees)
(declare distance)
(declare closeness)
(declare closeness-all)
(declare last-neigh)
(declare find-last-map)
(declare if-not-peek)


(defn degrees
  "Calculates the degree centrality for each node"
  [g]
  (loop [s g res{}]
    (if (seq s)
      (recur (rest s) (assoc res (first (first s)) (merge (second (first s)) {:degree (count (get (second (first s)) :neigh))})))
      res)))




(defn distance
  "Calculate the distances of one node to all the others"
  [graph n]
  (loop [queue   (conj clojure.lang.PersistentQueue/EMPTY n)
         visited [] res {n (float 0)} cpt 1 lastN (last (find-neighbors n graph))]
    (if (empty? queue) res
        (let [v           (peek queue)
              neighbors   (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) neighbors)
              new-queue   (apply conj (pop queue) not-visited)]
          (if (visited? v visited)
            (recur new-queue visited  res (if (= v lastN)
                                             (inc cpt)
                                            cpt)  (if (= v lastN)
                                                    (last-neigh (find-neighbors v graph) visited v)
                                                    lastN))
            (recur new-queue (conj visited v) (merge {v (float cpt)} res) (if (= v lastN)
                                                                            (inc cpt)
                                                                            cpt)  (if (= v lastN)
                                                                                    (last-neigh (find-neighbors v graph) visited v)
                                                                                    lastN)))))))




(defn closeness
  "Returns the closeness for node n in graph g"
  [g n]
 (reduce + (map (fn [x]  (/ 1 x)) (filter (fn [x] (not (zero? x))) (vals (distance g n))))))



(defn closeness-all
  "Returns the closeness for all nodes in graph g"
  [g]
  (loop [s g res{}]
    (if (seq s)
      (recur (rest s) (assoc res (first (first s)) (merge (second (first s)) {:close (closeness g (first (first s)))})))
      res)))




;;------------------------------------------------UtilFonction-------------------------------------------

(defn visited?
  "verifie si l'elements a deja était visiter"
  [v coll]
  (some #(= % v) coll))

(defn find-neighbors
  "retourn les voisins d un noeud"
  [v coll]
  (apply sorted-set (get-in coll [v :neigh])))


(defn neighbors-direct?
  [graph pos base]
  (contains? (find-neighbors pos graph) base))

(defn last-neigh
  [sett coll v]
  (loop [c sett]
    (if (seq c)
      (if (not (visited? (last c) coll))
        (last c)
       (recur (butlast c)))
      v)))



(defn find-last-map
  [m]
  (loop [s m res (first m)]
    (if (seq s)
      (recur (rest s) (first s))
     res)))


(defn if-not-peek
  "coll set -> while elem in a coll take next"
  [coll set]
  (loop [revc (reverse set)]
    (if (seq revc)
        (if (some #(= (first revc) %) coll)
            (recur (rest revc))
         (first revc))
      nil)))
