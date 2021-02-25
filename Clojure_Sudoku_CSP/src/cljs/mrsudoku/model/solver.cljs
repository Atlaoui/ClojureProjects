(ns mrsudoku.model.solver
  (:require [mrsudoku.model.grid :as g]))


(declare posible-value)
(declare only-posValue)
(declare find-block)
(declare build_Graph)
(declare update-grid)
(declare isConistant?)
(declare build-row)
(declare update-case)
(declare getSmallest)
(declare solverP)
(declare solver)
(declare augmenter)
(declare max-matching)
(declare alldiff)
(declare block-doms)
(declare row-doms)
(declare col-doms)
(declare skip-val)
(declare all-unique?)
(declare solution)
(declare remove-dom)
;;choix et ancre pour la recursion
(defn solve
    "Solve the sudoku `grid` by returing a full solved grid,
   or `nil` if the solver fails."
  ([grid] (second (solverP grid)))
  ([grid ,choix]
   (cond
        (= choix 0) (second (solverP grid))
        (= choix 1) (second (solver grid))
        :else (let [newgrid (update-grid grid (build_Graph grid))]
                  (if (= newgrid grid)
                    nil
                   newgrid)))))

(defn solverP
  "Solve the sudoku using a csp algorithme and smallest domaine stratigie"
  ([grid] (solverP grid (build_Graph grid)))
  ([grid doms]
    ;;ici en choisi de resoudre en prenan la stratigie du plus petits ensemble
   (if (seq doms)
     (let [ cx (int (/ (key (getSmallest doms)) 10)) cy (mod (key (getSmallest doms)) 10) , dom (second (getSmallest doms))]
     ;;  (println "ICI : "cx cy grid doms dom)
       (loop [ens dom]
         (if (seq ens)
           (let [newgrid (update-case grid cx cy (first ens))
                 newgraph (build_Graph newgrid)]
             (if (isConistant? newgraph)
               (let [[resp ngrid] (solverP newgrid newgraph)]
                   (if resp
                     [true, ngrid]
                     (recur (rest ens))))
              (recur (rest ens))))
           [false , nil])))
     [true,grid])))




(defn getSmallest
  "get the next getSmallest domain to procede as a recursion point"
  [graph]
  (loop [s graph res (first graph)]
    ;;(println res)
    (if (seq s)
      (if (< (count (val (first s))) (count (val  res)))
        (recur (rest s)  (first s))
        (recur (rest s) res))
     res)))


(defn isConistant?
  "Tell us if there is still somme homme en this tree"
  [graph]
  (println graph)
  (loop [sol graph]
    (if (seq (first sol))
      ;;(do (println "ici "(val (first sol)))
        (if (and (coll? (val (first sol))) (= (count (val (first sol))) 0))
            false
         (recur (rest sol)))
     true)))


(defn build_Graph
  "build a graph  in function of grid"
  [grid]
  (loop [x 1 res {}]
    ;;(println x)
    (if (<= x 9)
      (recur (inc x) (conj res (build-row grid (g/row grid x) x)))
      res)))


(defn build-row
  "build the dom row by row "
  [grid row num]
  (loop [s row x 1 res {}]
    ;;(println s)
    (if (seq s)
      (if (:value (first s))
        (recur (rest s) (inc x) res)
       (recur (rest s) (inc x) (assoc res  (+ (* x 10) num) (only-posValue grid num x))))
      res)))


(defn update-case
  "change the value of one slot"
  [grid cx cy val]
 (g/change-cell grid cx cy (g/mk-cell :set val)))

(defn unpdate-case
  "unchange the value of one slot"
  [grid cx cy]
 (g/change-cell grid cx cy (g/mk-cell)))


(defn update-grid
  "update the grid with all singletons "
   [grid graphCont]
   (loop [s graphCont gr grid]
     (if (seq s)
         (if (and (coll? (val (first s))) (= (count (val (first s))) 1))
             (let [cx (int (/ (key (first s)) 10)) cy (int (mod (key (first s)) 10))]
              (recur (rest s) (g/change-cell gr cx cy (g/mk-cell :set (first (val (first s)))))))
           (recur (rest s) gr))
       gr)))



(defn find-block
  "return the position of the block with the pose x and y"
  ([b]
   (cond
     (= 1 b) [1 1]
     (= 2 b) [1 4]
     (= 3 b) [1 7]
     (= 4 b) [4 1]
     (= 5 b) [4 4]
     (= 6 b) [4 7]
     (= 7 b) [7 1]
     (= 8 b) [7 4]
     :else [7 7]))
  ([cx cy]
   (if (<= cx 3)
     (cond (<= cy 3) 1
           (<= cy 6) 2
          :else 3)
     (if (<= cx 6)
       (cond (<= cy 3) 4
             (<= cy 6) 5
             :else 6)
       (cond (<= cy 3) 7
             (<= cy 6) 8
             :else 9)))))


(defn  posible-value
  "recuper les valeur"
  [coll]
  (loop [c coll  res #{}]
    (if (seq c)
     (if (:value (first c))
       (recur (rest c) (conj res (:value (first c))))
       (recur (rest c) res))
     res)))

;;(clojure.set/difference #{1 2 3 4 5 6 7 8 9}
(defn only-posValue
  "give the only possible value for a given position"
  [grid cx cy]
  ;;(println cx cy  (clojure.set/difference #{1 2 3 4 5 6 7 8 9} (posible-value (g/row grid cx)) (posible-value (g/col grid cy)) (posible-value (g/block grid (find-block cx cy)))))
  (clojure.set/difference #{1 2 3 4 5 6 7 8 9} (posible-value (g/row grid cx)) (posible-value (g/col grid cy)) (posible-value (g/block grid (find-block cx cy)))))



;;-------------------------------------- SOLVEUR AVEC LE CODE VUE EN COUR -----------------------------------------
;;non fonctionnel
(defn solver
  "resous le sudoku avec les fonctions vue en cours"
  ;;([grid, cx ,cy] (solver gird cx cy (merge (build_Graph grid) (alldiff (row-doms grid cx)) (alldiff (col-doms grid cy)) (alldiff (block-doms grid (find-block cx cy))))))
  ([grid] (solver grid 1 1 (build_Graph grid)))
  ([grid cx cy doms]

   (if (seq doms)
    (if (and (< cx 10) (< cy 10))
     (if (= :init (:status (g/cell grid cx cy)))
         (if (= cy 9)
           (solver grid (inc cx) 1 doms)
           (solver grid cx (inc cy) doms))
         (let [pos (+ (* cy 10) cx) dom (get doms pos)]
          ;;(println "ici"  (row-doms grid cx) "qlskdsqdlkj" (alldiff (col-doms grid cy)) "qsdqsdsq"(alldiff (block-doms grid (find-block cx cy))))
          (loop [ens dom]
            (if (seq dom)
              (if (not (all-unique? doms))
                (let [newgrid (update-case grid cx cy (first ens))]
                  (println "ici" newgrid)
                  (if (= cy 9)
                   (let [[resp ngrid] (solver newgrid (inc cx) 1 (merge (build_Graph grid) (alldiff (row-doms grid cx)) (alldiff (col-doms grid cy)) (alldiff (block-doms grid (find-block cx cy)))))]
                     (if resp
                       [true , ngrid]
                       (recur (rest ens))))
                   (let [[resp ngrid] (solver newgrid  cx (inc cy) (merge (build_Graph grid) (alldiff (row-doms grid cx)) (alldiff (col-doms grid cy)) (alldiff (block-doms grid (find-block cx cy)))))]
                      (if resp
                        [true , ngrid]
                        (recur (rest ens))))))
                [true , (update-grid grid)])
              [false , nil]))))
     [true,grid])
    [true ,grid])))


(defn block-doms
  "retourne les valeur possible pour un block donner"
  [grid b]
  (let [[a bl] (find-block b)]
   (loop [lig a ,col bl, cpt 1 ,res {}]
    (if (< (count res) 9)
      (if (zero? (rem cpt 3))
        (recur (inc lig) bl (inc cpt) (assoc res (+ (* lig 10) col) (skip-val grid lig col)))
       (recur  lig (inc col) (inc cpt) (assoc res (+ (* lig 10) col) (skip-val grid lig col))))
      res))))

(defn row-doms
  "donne un domaine en ligne "
  [grid cx]
  (loop [cy 1 res {}]
    (if (< (count res) 9)
      (recur (inc cy) (assoc res (+ (* cx 10) cy) (skip-val grid cx cy)));;ptet j'ai inverser
     res)))

(defn col-doms
  [grid cy]
  (loop [cx 1 res {}]
    (if (< (count res) 9)
      (recur (inc cx) (assoc res (+ (* cx 10) cy) (skip-val grid cx cy)))
     res)))

(defn skip-val
  "juste pour esquiver les value pour la construction du dom pour le alldiff"
  [grid cx cy]
  (if-let [val (:value (g/cell grid cx cy))]
    #{val}
    (only-posValue grid cx cy)))



;;(apply dissoc t (keys (All-singletons (order-doms t))))
(defn All-singletons
  "take all singletons in a presorted array"
  [doms]
  (take-while (fn [x] (= (count (val x)) 1))  doms))

(defn isSolveAble?
    "tell us if the graph is solvable"
    [doms]
    (every? #(= (count %) 1)  doms))

(defn all-unique?
    "renvois true si il sont tousse des singletons"
    [& doms]
    (every? #(= (count %) 1)  doms))

(defn order-doms
  "order doms order the domaines from the smallest to the largest "
  [doms]
 (sort (fn [x y] (compare (count (second x)) (count (second y)))) doms))


(defn max-matching
  "give the graph with the maxmatching"
  [doms]
  (reduce (fn [m node] (nth (augmenter doms node #{} m) 2)) {} (keys doms)))

(defn augmenter [graph src visited match]
  (loop [dests (get graph src) visited visited]
    (if (seq dests)
     (if (visited (first dests))
         (recur (rest dests) visited)
       (if-let [old-src (get match (first dests))]
         (let [[found, visited', match'] (augmenter graph old-src (conj visited (first dests)) match)]
         ;; (println "visited : " visited ", dests : " dests ", match : " match)
          (if found
            [true, visited', (assoc match' (first dests) src)]
            (recur (rest dests) visited')))
         ;;  s'il n'y a pas de match : on a gagné !
         [true, (conj visited (first dests)), (assoc match (first dests) src)]))
     [false, visited, match])))

;;-----------------------------------DFS CODE ----------------------------------------------------------

(defn dfs-rec
  "Deep first search rec"
  ([graph vert f] (dfs-rec graph vert f #{} #{}))
  ([graph vert f init visited]
   (if (visited vert)
     [init visited]
     (loop [verts (get graph vert), res (f init vert), visited (conj visited vert)]
       (if (seq verts)
         (let [[res', visited'] (dfs-rec graph (first verts) f res visited)]
           (recur (rest verts) res' visited'))
         [res visited])))))

(defn dfs-post
  ([graph vert f] (dfs-post graph vert f #{} #{}))
  ([graph vert f init visited]
   (if (visited vert)
     [init visited]
     (loop [verts (get graph vert), res init,visited (conj visited vert)]
       (if (seq verts)
         (let [[res', visited'] (dfs-post graph (first verts) f res visited)]
           (recur (rest verts) res' visited'))
         [(f res vert) visited])))))


(defn dfs-stack
  "autre dfs"
  [graph]
  (loop [verts (keys graph),stack (),visited #{}]
    (if (seq verts)
      (let [[stack',visited'] (dfs-post graph (first verts) conj stack visited)]
        (recur (rest verts) stack' visited'))
      stack)))

(defn rev-graph-link;;transpose
  "Inverse la direction des arret d'un graph sans les noeud seul"
  [g]
  (loop [gr g resG {}]
    (if (seq gr)
     (recur (rest gr) (merge-with clojure.set/union (zipmap (val (first gr)) (repeat #{(key (first gr))})) resG))
     resG)))

(defn rev-graph-complet
  [graph]
  (loop [gr graph res (rev-graph-link graph)]
    (if (seq gr)
      (if (= (count (val (first gr))) 0)
        (recur (rest gr) (conj res (first gr)))
       (recur (rest gr) res))
     res)))

(defn compute-scc
  "composant fortement connex"
  [graph]
  (let [stack (dfs-stack graph), tgraph (rev-graph-complet graph)]
    (loop [s stack,visited #{},scc []]
      (if (seq s)
        (if (visited (first s))
          (recur (rest s) visited scc)
          (let [[comp visited'] (dfs-rec tgraph (first s) conj #{} visited)]
            (recur (rest s) visited' (conj scc comp))))
        scc))))

(defn add-vertex
  "add link"
  [graph vert]
  (if (contains? graph vert)
    vert
    (assoc graph vert #{})))

(defn remove-edge
 "Removes the a->b edge"
 [g a b]
 (if (= (count (get g a)) 1)
  (dissoc g a)
  (update g a #(disj % b))))

(defn add-edge
  "Add edge"
  [graph a b]
  (update graph a #(conj (or % #{}) b)))


(defn graph-with-matching
  [graph match]
  (reduce (fn [mgraph [src dest]]
            (-> mgraph
              (add-vertex src)
              (add-edge src dest)
              (remove-edge dest src)))
          graph match))

(defn complete-matching?
            [vars match]
  (= (count vars) (count match)))


(defn sinks
  "Rajoute dans le graphe les sommets qui n'ont pas de successeurs"
  [graph verts]
  (reduce (fn [ngraph vert]
            (if (contains? ngraph vert)
              ngraph
              (assoc ngraph vert #{})))
          graph
          verts))

(defn doms-from-sccomp [variables compp]
  (if (= (count compp) 1)
     (if (contains? variables (first compp))
      {(first compp) #{}}
      {})
     (let [vars (clojure.set/select #(contains? variables %) compp)
           values (clojure.set/difference compp vars)]
      (zipmap vars (repeat values)))))


(defn doms-from-scc
  [vars scc]
  (reduce (fn [res comp] (conj res (doms-from-sccomp vars comp))) {}  scc))


(defn value-known-by
  [doms value]
  (reduce (fn [res [v values]]
           (if (contains? values value)
             (conj res v)
             res)) #{} doms))

(defn isolated-values
  [variables scc]
  (into #{} (map first (filter #(and (= (count %) 1) (not (variables (first %)))) scc))))

(defn add-value
  [doms vers value]
  (into doms (map (fn [var] [var,(conj (get doms var) value)]) vers)))

(defn access
   [doms scc]
   (let [sccdoms (doms-from-scc (into #{} (keys doms)) scc)
         isolated (isolated-values (into #{} (keys doms)) scc)]
     (reduce (fn [doms' value] (add-value doms' (value-known-by doms value) value)) (sinks sccdoms (keys doms)) isolated)))


(defn alldiff
  "simplify the doms for the all-different constraint,return nil if not satisfiable"
  [doms]
  (let [match (max-matching doms)]
       (if (complete-matching? doms match)
         (let [scc (compute-scc (graph-with-matching doms match))]
           ;(println scc)
           (access doms scc))
          ;;incompler
         nil)))
