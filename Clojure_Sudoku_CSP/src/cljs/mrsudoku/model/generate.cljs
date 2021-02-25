(ns mrsudoku.model.generate
  (:require [mrsudoku.model.solver :as s]
            [mrsudoku.model.grid :as g]))

(declare generate-base-grid)
(declare init-cell)
(declare extract-value)
(declare extract-values)
(declare set-all-to-init)
(declare clear-cell)
(declare comparer-blocs)
(declare comparer-lignes)
(declare comparer-grilles)
(declare isUniqueSol)
(declare set-row)
(declare set-block)


(defn Generate
  "Generate a random sudoku grid"
  ([]
   (let [board (s/solve (g/change-cell (generate-base-grid) (+ (rand-int 9) 1) (+ (rand-int 9) 1)  (g/mk-cell :init (+ (rand-int 9) 1))))]
     (set-all-to-init (extract-value board))))
  ([n]
   (let [board (s/solve (g/change-cell (generate-base-grid) (+ (rand-int 9) 1) (+ (rand-int 9) 1)  (g/mk-cell :init (+ (rand-int 9) 1))))]
     (set-all-to-init (extract-values board)))))





(defn extract-values
  "extract randomly a value from the board"
  [board]
  (loop [b board res []]
    ;;(println res)
    (if (seq b)
      (recur (rest b) (conj res (into [] (map (fn [x] (into [] (map clear-cell x))) (first b)))))
     res)))

(defn extract-value
  "extract randomly a value from the board"
 ([grid] (extract-value grid grid 0))
 ([grid gridDep rec]
  (loop [g grid res [] i 0 num (int (rand 3))]
    (if (= rec 15)
      grid
      (if (= i 3)
        (if (first (isUniqueSol res gridDep))
          (extract-value res gridDep rec)
          (extract-value grid gridDep (inc rec)))
        (if (= i num)
          (recur (rest g) (conj res (set-row (first g) i grid)) (inc i) num)
          (recur (rest g) (conj res (first g)) (inc i) num)))))))

(defn set-row [row i grid]
  "Choisit au hasard un bloc de la grille pour lui retirer une valeur"
  (loop [r row res [] j 0 num (int (rand 3))]
    (if (= j 3)
      res
      (if (= j num)
        (recur (rest r) (conj res (set-block (g/block grid (inc (+ j (* 3 i)))))) (inc j) num)
        (recur (rest r) (conj res (first r)) (inc j) num)))))


(defn set-block [bloc]
  "Renvoie le bloc avec une valeur en moins"
 (loop [b bloc res [] i 0 num (int (rand 9))]
   (if (= i 9)
     res
     (if (= i num)
       (recur (rest b) (conj res (g/mk-cell)) (inc i) num)
       (recur (rest b) (conj res (first b)) (inc i) num)))))

(defn set-all-to-init
  "set all the value to init in a given board"
  [board]
  (loop [b board res []]
    (if (seq b)
      (recur (rest b) (conj res (into [] (map (fn [x] (into [](map init-cell x))) (first b)))))
     res)))

(defn init-cell
  "for a given cell well return a cell if there is a value in init state"
  [cell]
  (if (:value cell)
    (g/mk-cell (:value cell))
    cell))

(defn clear-cell
  "clear cell with a random chance test "
  ([cell]
   (if (and (:value cell) (< (rand) 0.8))
     (g/mk-cell)
     cell))
  ([cell rd]
   (if (and (:value cell) (< rd 0.5))
     (g/mk-cell)
     cell)))
(defn comparer-grilles [grid1 grid2]
  "Vérifie si les deux grilles de sudoku sont identiques"
 (loop [g1 grid1 g2 grid2]
   (if (seq g1)
     (if (comparer-lignes (first g1) (first g2))
       (recur (rest g1) (rest g2))
       false)
     true)))

(defn comparer-lignes [lig1 lig2]
 "Vérifie si deux lignes de blocs sont identiques"
 (loop [l1 lig1 l2 lig2]
   (if (seq l1)
     (if (comparer-blocs (first l1) (first l2))
       (recur (rest l1) (rest l2))
       false)
     true)))

(defn comparer-blocs [bloc1 bloc2]
 "Vérifie si deux blocs de la grille sont identiques"
 (loop [b1 bloc1 b2 bloc2]
   (if (seq b1)
     (if (= (:value (first b1)) (:value (first b2)))
       (recur (rest b1) (rest b2))
       false)
     true)))

(defn isUniqueSol
 "test if a sol is unique"
 ([grid gridDep] (isUniqueSol grid gridDep (s/build_Graph grid)))
 ([grid gridDep doms]
  (if (seq doms)
    (let [ cx (int (/ (key (s/getSmallest doms)) 10)) cy (mod (key (s/getSmallest doms)) 10) , dom (second (s/getSmallest doms))]
      (loop [ens dom]
        (if (seq ens)
          (let [newgrid (s/update-case grid cx cy (first ens))
                newgraph (s/build_Graph newgrid)]
            (if (s/isConistant? newgraph)
              (let [[resp ngrid] (isUniqueSol newgrid gridDep newgraph)]
                  (if (and resp (comparer-grilles ngrid gridDep))
                    (recur (rest ens))
                    [false, ngrid]))
             (recur (rest ens))))
          [true , nil])))
    [true,grid])))

(defn generate-base-grid
  "generate a white board"
  []
  [[;; row 1
    [(g/mk-cell) (g/mk-cell ) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]],
    [;; row 2
     [(g/mk-cell) (g/mk-cell) (g/mk-cell)
      (g/mk-cell) (g/mk-cell) (g/mk-cell)
      (g/mk-cell ) (g/mk-cell) (g/mk-cell)]
     [(g/mk-cell) (g/mk-cell ) (g/mk-cell)
      (g/mk-cell ) (g/mk-cell) (g/mk-cell)
      (g/mk-cell) (g/mk-cell ) (g/mk-cell)]
     [(g/mk-cell) (g/mk-cell) (g/mk-cell)
      (g/mk-cell) (g/mk-cell) (g/mk-cell)
      (g/mk-cell) (g/mk-cell) (g/mk-cell)]],
    [;; row 3
     [(g/mk-cell) (g/mk-cell ) (g/mk-cell)
       (g/mk-cell) (g/mk-cell) (g/mk-cell)
        (g/mk-cell) (g/mk-cell) (g/mk-cell)]
     [(g/mk-cell) (g/mk-cell) (g/mk-cell)
      (g/mk-cell ) (g/mk-cell ) (g/mk-cell)
      (g/mk-cell) (g/mk-cell ) (g/mk-cell)]
     [(g/mk-cell ) (g/mk-cell ) (g/mk-cell)
      (g/mk-cell) (g/mk-cell) (g/mk-cell)
      (g/mk-cell) (g/mk-cell ) (g/mk-cell)]]])
