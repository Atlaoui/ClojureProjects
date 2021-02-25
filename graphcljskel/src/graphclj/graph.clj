(ns graphclj.graph
  (:require [clojure.string :as str]
            [clojure.set :as set]))
;;ok
(declare rand-seq)
(declare link-by-node)
(declare pars-int)
(declare pars-key-g)
(declare pars-key-d)
(declare gen-graph)
(declare erdos-renyi-rnd)

;;parse "1 2" -> (1 2)
(defn pars-int
  [v]
 (map read-string (clojure.string/split v #" ")))

;;pars-key  "1 2" -> {0 {:neigh #{2}}}
(defn pars-key-g
  [ancient_val key_val]
  (let [kv (pars-int key_val)]
    (if (nil? ancient_val)
      {(first kv) {:neigh #{(second kv)}}}
     {(first kv) {:neigh (clojure.set/union ancient_val #{(second kv)})}})))


(defn pars-key-d
  [ancient_val key_val]
  (let [kv (pars-int key_val)]
    (if (nil? ancient_val)
      {(second kv) {:neigh #{(first kv)}}}
     {(second kv) {:neigh (clojure.set/union ancient_val #{(first kv)})}})))


;; Generate a graph from the lines
(defn gen-graph
  "Returns a hashmap contating the graph"
  [lines]
  (loop [v lines res {}]
    (if (seq v)
     (recur (rest v) (merge res (pars-key-g (get-in res [(first (pars-int (first v))) :neigh]) (first v)) (pars-key-d (get-in res [(second (pars-int (first v))) :neigh]) (first v))))
     res)))




(defn erdos-renyi-rnd
  "Returns a G_{n,p} random graph, also known as an Erdős-Rényi graph"
  [n,p]
  (gen-graph (rand-seq n p)))


(defn rand-seq
     [n,p]
  (loop [nseq (range n) res ()]
    (if (seq nseq)
      (recur (rest nseq) (concat res (link-by-node (first nseq) n p)))
     res)))

(defn link-by-node
      [node ,r ,p]
      (loop [x (range r) res ()]
        (if (seq x)
         (if (and (not= node (first x)) (< (rand) p))
            (recur (rest x) (conj res (str node" "(first x))))
           (recur (rest x) res))
         res)))



;;------------------------------------------------------Teste-----------------------------------------------------------
