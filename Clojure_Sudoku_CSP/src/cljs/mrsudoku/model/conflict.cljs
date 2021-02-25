(ns mrsudoku.model.conflict
  (:require [clojure.set :as set]
            [mrsudoku.model.grid :as g]))


;; ========================================================================
;; Fichier à compléter : il faut utiliser les fonctions du namespace grid.
;; et regarder également les tests dans `test/cljs/mrsudoku/conflict_test.cljs`
;; et bien sûr `test/cljs/mrsudoku/grid_test.cljs`  (et les autres tests ...)
;; ========================================================================

;;normalement OP pour cells
(defn values
  "Return the set of values of a vector or grid `cells`."
  [cells]
  (into #{} (filter (fn [x] (not (nil? x))) (map :value cells))))


(defn values-except
  "Return the set of values of a vector of cells, except the `except`-th."
  [cells except]
  {:pre [(<= 1 except (count cells))]}
  (let [pos (dec except)]
    (values (filter (fn [x]  (not= (get cells pos) x)) cells))))


(defn mk-conflict [kind cx cy value]
  {:status :conflict
   :kind kind
   :value value})

;;plus que le merge with union
(defn merge-conflict-kind
  [kind1 kind2]
  (cond
    (and (coll? kind1) (coll? kind2)) (into #{} (distinct (concat kind1 kind2)))
    (coll? kind1) (merge kind1 kind2)
    (coll? kind2) (merge kind2 kind1)
    (= kind1 kind2) kind1
    :else #{kind1 kind2}))

(defn merge-conflict [conflict1 conflict2]
   (assoc conflict1 :kind (merge-conflict-kind (:kind conflict1) (:kind conflict2))))

(defn merge-conflicts [& conflicts]
  (apply (partial merge-with merge-conflict) conflicts))

(defn update-conflicts
  [conflict-kind cx cy value conflicts]
  (if-let [conflict (get conflicts [cx, cy])]
    (assoc conflicts [cx, cy] (mk-conflict (merge-conflict-kind conflict-kind (:kind conflict))
                                           cx cy value))
    (assoc conflicts [cx, cy] (mk-conflict conflict-kind cx cy value))))

(defn conflict-value [values except cell]
  (when-let [value (g/cell-value cell)]
    (when (and (not= (:status cell) :init)
               (contains? (values-except values except) value))
      value)))

(defn col-conflicts
  "Returns a map of conflicts in a `col`."
  [col cx]
  (loop [cy 1, cells (seq col), conflicts {}]
    ;(println conflicts)
    (if (seq cells)
      (let [cell (first cells)]
        (if-let [value (conflict-value col cy cell)]
          (recur (+ cy 1) (rest cells) (update-conflicts :col cx cy value conflicts))
          (recur (+ cy 1) (rest cells) conflicts)))
      ;; no more cells
      conflicts)))

(defn cols-conflicts
  [grid] (reduce merge-conflicts {}
                 (map (fn [c] (col-conflicts (g/col grid c) c)) (range 1 10))))



(defn row-conflicts
  "Returns a map of conflicts in a `row`."
  [row cy]
  (loop [cx 1, ro row, res {}]
    (if (seq ro)
      (let [r (first ro)]
        (if (and (= (get r :status) :set) (> (count (filter #(= (get % :value) (get r :value)) row)) 1))
          (recur (inc cx) (rest ro) (assoc res [cx cy] (mk-conflict :row cx cy (get r :value))))
          (recur (inc cx) (rest ro) res)))
      res)))


(defn rows-conflicts
  "Returns a map of conflicts in all rows of `grid`"
  [grid](reduce merge-conflicts {}
                 (map (fn [c] (row-conflicts (g/row grid c) c)) (range 1 10))))

(defn block-conflicts
  [block b]
  [block b]
  (let [bx (* (rem (dec b) 3) 3), by (* (quot (dec b) 3) 3)]
    (loop [i 0, blo block, res {}]
     (if (seq blo)
       (let [bl (first blo)]
         (if (and (= (get bl :status) :set) (> (count (filter #(= (get % :value) (get bl :value)) block)) 1))
           (recur (inc i) (rest blo) (assoc res [(+ bx 1 (rem i 3)) (+ by 1 (quot i 3))] (mk-conflict :block 0 0 (get bl :value))))
           (recur (inc i) (rest blo) res)))
      res))))

(defn blocks-conflicts
  [grid]
  (reduce merge-conflicts {}
          (map (fn [b] (block-conflicts (g/block grid b) b)) (range 1 10))))

(defn grid-conflicts
  "Compute all conflicts in the Sudoku grid."
  [grid]
  (println "compute conflicts")
  (println (g/grid->str grid))
  (merge-conflicts (rows-conflicts grid)
                   (cols-conflicts grid)
                   (blocks-conflicts grid)))





;;==============================================================================


;;(def t [{:status :init, :value 5} {:status :init, :value 6}
;;        {:status :empty} {:status :init, :value 8} {:status :init, :value 4}
;;        {:status :init, :value 7} {:status :empty}
;;         {:status :empty} {:status :empty})))
;;(values (filter (fn [x]  (not= (get (block sudoku-grid 1) 0) x)) (block sudoku-grid 1)))
;;(defn dfs-stack [sq])
;;(defn transpose [sq])
;;(defn dfs-pre [sq])




;;(defn graph-with matching [graph match]
;;  (reduce (fn [mgraph [src dest]]
;;            (-> mgraph
;;                (add-vertex src)
;;                (add-edge src dest)
;;                (remove-edge dest src))) graph match
