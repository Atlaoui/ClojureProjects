(ns mrsudoku.solver-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mrsudoku.model.grid :as g]
            [mrsudoku.model.solver :refer [solve
                                           posible-value
                                           find-block
                                           only-posValue
                                           all-unique?
                                           build-row
                                           build_Graph
                                           augmenter
                                           update-grid
                                           isConistant?
                                           update-case
                                           unpdate-case
                                           max-matching
                                           dfs-rec
                                           dfs-post
                                           dfs-stack
                                           compute-scc
                                           rev-graph-link
                                           rev-graph-complet
                                           graph-with-matching
                                           alldiff
                                           value-known-by
                                           block-doms
                                           row-doms
                                           col-doms
                                           access]]))



(def ^:private sudoku-grid @#'g/sudoku-grid)
(deftest solve-test
  (is (= 1 1)))

(deftest posible-value-test
  (is (= (posible-value (g/block sudoku-grid 1)) #{5 3 6 9 8}))
  (is (= (posible-value (g/row sudoku-grid 1)) #{5 3 7}))
  (is (= (posible-value (g/col sudoku-grid 1)) #{5 6 8 4 7})))

(deftest find-block-test
  (is (= (find-block 1 1) 1))
  (is (= (find-block 1 4) 2))
  (is (= (find-block 1 7) 3))
  (is (= (find-block 4 3) 4))
  (is (= (find-block 1 3) 1)))


(deftest only-posValue-test
  (is (= (only-posValue sudoku-grid 1 3) #{1 4 2}))
  (is (= (only-posValue sudoku-grid 2 3) #{7 4 2}))
  (is (= (only-posValue sudoku-grid 3 1) #{1 2})))

(deftest all-unique-test
  (is (= (all-unique?  #{1}) true))
  (is (= (all-unique?  #{1} #{13} #{13}) true))
  (is (= (all-unique?  #{1} #{13 2} #{13}) false)))

(deftest build-row-test
  (let [ row (g/row sudoku-grid 1)]
   (is (= (build-row sudoku-grid row 1) {31 #{1 4 2}, 41 #{6 2}, 61 #{4 6 2 8}, 71 #{1 4 9 8}, 81 #{1 4 2 9}, 91 #{4 2 8}}))))

(deftest augmenter-test
  (is (= (augmenter {:x1 #{1 4 43 5} :x2 #{2}  :x3 #{5}} :x2 #{2 4} {1 :x1}) [false #{4 2} {1 :x1}]))
  (is (= (augmenter {:x1 #{1 4 55} :x2 #{1}  :x3 #{4}} :x2 #{1} {1 :x1}) [false #{1} {1 :x1}])))

(deftest isConistant-test
  (let [ens1 {74 #{}, 49 #{6 3 2 5}, 84 #{4 2 9 5}}
        ens2 {{56 #{2}} ,{33 #{2 3 4}}, {43 #{1}} ,{42 #{1}}}
        prms (build_Graph sudoku-grid)]
    (is (= (isConistant? prms) true))
    (is (= (isConistant? ens1) false))
    (is (= (isConistant? ens2)  true))))

(deftest build_Graph-test
  (let [test-grid [[;; row 1
                    [(g/mk-cell ) (g/mk-cell 3) (g/mk-cell 1)
                     (g/mk-cell 6) (g/mk-cell 4) (g/mk-cell)
                     (g/mk-cell 2) (g/mk-cell 9) (g/mk-cell 8)]
                    [(g/mk-cell) (g/mk-cell 7) (g/mk-cell)
                     (g/mk-cell 1) (g/mk-cell 9) (g/mk-cell 5)
                     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
                    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
                     (g/mk-cell) (g/mk-cell) (g/mk-cell)
                     (g/mk-cell) (g/mk-cell 6) (g/mk-cell)] ],
                   [;; row 2
                    [(g/mk-cell 8) (g/mk-cell) (g/mk-cell)
                     (g/mk-cell 4) (g/mk-cell) (g/mk-cell)
                     (g/mk-cell 7) (g/mk-cell) (g/mk-cell)]
                    [(g/mk-cell) (g/mk-cell 6) (g/mk-cell)
                     (g/mk-cell 8) (g/mk-cell) (g/mk-cell 3)
                     (g/mk-cell) (g/mk-cell 2) (g/mk-cell)]
                    [(g/mk-cell) (g/mk-cell) (g/mk-cell 3)
                     (g/mk-cell) (g/mk-cell) (g/mk-cell 1)
                     (g/mk-cell) (g/mk-cell) (g/mk-cell 6)]],
                   [;; row 3
                    [(g/mk-cell) (g/mk-cell 6) (g/mk-cell)
                      (g/mk-cell) (g/mk-cell) (g/mk-cell)
                     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
                    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
                     (g/mk-cell 4) (g/mk-cell 1) (g/mk-cell 9)
                     (g/mk-cell) (g/mk-cell 8) (g/mk-cell)]
                    [(g/mk-cell 2) (g/mk-cell 8) (g/mk-cell)
                     (g/mk-cell) (g/mk-cell) (g/mk-cell 5)
                     (g/mk-cell) (g/mk-cell 7) (g/mk-cell 9)]]]]
    (is (= (build_Graph test-grid)) {32 #{7 4 2}, 64 #{7 1 4}, 97 #{4}, 34 #{1 2 9 5}, 66 #{1 4}, 35 #{6 2 9 5}, 67 #{7}, 36 #{1 3 9 5}, 37 #{7 1 4 3 9 5}, 69 #{6 2}, 38 #{7 3 2}, 39 #{1 4 3 2 5}, 71 #{1 4 9 8}, 72 #{7 4 3 8}, 41 #{6 2}, 73 #{7 1 4 3 5}, 74 #{7 4 9 5}, 43 #{3 2}, 75 #{7 9 5}, 44 #{7 9 5}, 76 #{4 9 5 8}, 13 #{1 2}, 46 #{9 5}, 78 #{6 3}, 47 #{7 3 5}, 79 #{1 4 6 3}, 17 #{1 3 9}, 49 #{6 3 2 5}, 81 #{1 4 2 9}, 18 #{3 2}, 82 #{4 3 2}, 19 #{1 3 2}, 84 #{4 2 9 5}, 53 #{4 3}, 85 #{2 9 5}, 22 #{7 4 2}, 86 #{4 9 5}, 55 #{5}, 24 #{1 2 5}, 88 #{3}, 25 #{2 5}, 57 #{3 5}, 26 #{1 5}, 91 #{4 2 8}, 28 #{7 2 8}, 92 #{7 4 2 8}, 29 #{1 4 2 5}, 61 #{4 6 2 8}, 93 #{7 4 2}, 31 #{1 4 2}, 63 #{4 2}})))


(deftest update-unupdate-grid_test
  (let [grid sudoku-grid]
    grid (update-case grid 1 3 2)
    grid (unpdate-case grid 1 3)
    (is (= (g/cell sudoku-grid 1 3) (g/cell grid 1 3)))))

;;--------------------------------------FONCTION VUE EN COURS ---------------------------------------
(deftest max-matching-test
  (let [doms {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6}}
        doms2 {23 #{1 2 3} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}
        doms3 {23 #{} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}]
    (is (= (max-matching doms) {1 :v2, 3 :v1, 4 :v5, 6 :v4, 5 :v3}))
    (is (= (max-matching doms2) {1 34, 3 23, 4 54, 6 88, 5 44}))
    (is (= (max-matching doms3) {1 34, 4 54, 6 88, 5 44}))))

(deftest graph-with-matching-test
  (let [graph {:v1 #{1 2 3 4} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6}}]
      (is (= (graph-with-matching graph (max-matching graph)) {:v2 #{1 4 5}, 1 #{:v1}, 2 #{:v2}, 4 #{:v5}, 5 #{:v3}, :v5 #{6 5}, :v1 #{4 3 2}, 6 #{:v4}, :v4 #{4 5}, :v3 #{4 6}}))))


(deftest dfs-rec-test
  (let [g {:A #{:F :B :C}
               :B #{:C}
               :C #{:D}
               :D #{:E}
               :E #{:C}
               :F #{:H :G}
               :G #{:H :I}
               :H #{:F :I}
               :I #{}}]
    (is (= (dfs-rec g :A conj) [#{:I :A :F :D :B :C :E :G :H} #{:I :A :F :D :B :C :E :G :H}]))
    (is (= (dfs-rec g :A merge) [#{:I :A :F :D :B :C :E :G :H} #{:I :A :F :D :B :C :E :G :H}]))
    (is (= (dfs-rec g :I conj) [#{:I} #{:I}]))
    (is (= (dfs-rec g :C merge) [#{:C :D :E} #{:C :D :E}]))))

(deftest dfs-post-test
  (let [g {:A #{:F :B :C}
               :B #{:C}
               :C #{:D}
               :D #{:E}
               :E #{:C}
               :F #{:H :G}
               :G #{:H :I}
               :H #{:F :I}
               :I #{}}]
    (is (= (dfs-post g :A conj) [#{:I :A :F :D :B :C :E :G :H} #{:I :A :F :D :B :C :E :G :H}]))
    (is (= (dfs-post g :C conj) [#{:E :D :C} #{:C :D :E}]))
    (is (= (dfs-post g :I conj) [#{:I} #{:I}]))))

(deftest dfs-stack-test
  (let [doms {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6}}
        doms2 {23 #{1 2 3} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}
        doms3 {23 #{} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}]
    (is (= (dfs-stack doms) '(:v5 :v4 :v3 6 :v2 5 4 :v1 2 3 1)))
    (is (= (dfs-stack doms2) '(54 88 44 6 34 5 4 23 2 3 1)))
    (is (= (dfs-stack doms3) '(54 88 44 6 34 5 2 4 1 23)))))

(deftest rev-graph-test
  (let [doms {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6}}
        doms2 {23 #{1 2 3} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}
        doms3 {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{}}]
    (is (=  (rev-graph-link doms) {4 #{:v3 :v2 :v4 :v5}, 6 #{:v4 :v3 :v5}, 5 #{:v3 :v2 :v4 :v5}, 1 #{:v2 :v1}, 2 #{:v2 :v1}, 3 #{:v1}}))
    (is (= (rev-graph-complet doms) {4 #{:v3 :v2 :v4 :v5}, 6 #{:v4 :v3 :v5}, 5 #{:v3 :v2 :v4 :v5}, 1 #{:v2 :v1}, 2 #{:v2 :v1}, 3 #{:v1}}))
    (is (=  (rev-graph-link doms2) {4 #{44 34 88 54}, 6 #{88 44 54}, 5 #{44 34 88 54}, 1 #{34 23}, 2 #{34 23}, 3 #{23}}))
    (is (=  (rev-graph-link doms3) {4 #{:v3 :v2 :v4}, 6 #{:v4 :v3}, 5 #{:v3 :v2 :v4}, 1 #{:v2 :v1}, 2 #{:v2 :v1}, 3 #{:v1}}))
    (is (=  (rev-graph-complet doms3) {4 #{:v3 :v2 :v4}, 6 #{:v4 :v3}, 5 #{:v3 :v2 :v4}, 1 #{:v2 :v1}, 2 #{:v2 :v1}, 3 #{:v1}, :v5 #{}}))))



(deftest compute-scc-test
  (let [doms {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6}}
        doms2 {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6} :v6 #{2}}]
    (is (= (compute-scc (graph-with-matching doms (max-matching doms))) [#{3} #{:v1} #{1} #{:v2} #{2} #{4 :v4 6 :v3 5 :v5}]))
    (is (= (compute-scc (graph-with-matching doms2 (max-matching doms2))) [#{3} #{:v1} #{1} #{:v2} #{2} #{:v6} #{4 :v4 6 :v3 5 :v5}]))))

(deftest alldiff-test
 (let [doms {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6}}
       doms2 {23 #{1 2 3} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}
        doms3 {23 #{} 34 #{1 2 4 5} 44 #{4 5 6} 88 #{4 5 6} 54 #{4 5 6}}
        graph (build_Graph sudoku-grid)
        doms4 {32 #{7 4 2}, 64 #{7 1 4} ,34 #{1 2 9 5}, 66 #{1 4}, 35 #{6 2 9 5}, 67 #{7}, 36 #{1 3 9 5}, 37 #{7 1 4 3 9 5}}
        domsrow {32 #{1 2 3 4 5 6 7 8 9}, 64 #{}, 97 #{1 2 3 4 5 7 8 9}, 34 #{1 2 3 4 5 6  9}, 66 #{1 2 8 9}, 35 #{1 2 3 4 5 6 7 8 9}, 67 #{1 2 3 4 5 6 7 8 9}, 36 #{3}, 37 #{1}}]
  (is (= (alldiff doms) {:v1 #{3 1 2}, :v2 #{1 2}, :v4 #{4 6 5}, :v3 #{4 6 5}, :v5 #{4 6 5}}))
  (is (= (alldiff graph) nil))
  (is (= (alldiff doms2) {23 #{3 1 2}, 34 #{1 2}, 44 #{4 5 6}, 54 #{4 5 6}, 88 #{4 5 6}}))
  (is (= (alldiff doms3) nil))
  (is (= (alldiff doms4) {35 #{6 2}, 34 #{5 9 3 2}, 36 #{5 9 3}, 37 #{5 9 3 7}, 32 #{2 7}, 64 #{4 1 7}, 66 #{4 1}, 67 #{7}}))
  (is (= (alldiff domsrow) nil))))
 ;; (is (= (alldiff {13 #{1 4 2},17 #{1 4 9 8}, 18 #{1 4 2 9}, 19 #{4 2 8}})  1))))

(deftest value-known-by-test
  (let [doms {:v1 #{1 2 3} :v2 #{1 2 4 5} :v3 #{4 5 6} :v4 #{4 5 6} :v5 #{4 5 6} :v6 #{44}}]
    (is (= (value-known-by doms 5) #{:v2 :v3 :v4 :v5}))
    (is (= (value-known-by doms 1) #{:v1 :v2}))
    (is (= (value-known-by doms 44) #{:v6}))))

(deftest recup-val-alldif-test
  (let [dom1 (block-doms sudoku-grid 1)
        dom2 (row-doms sudoku-grid 1)
        dom3 (col-doms sudoku-grid 1)]
    (is (= dom1 {32 #{7 1 4 2}, 33 #{8}, 11 #{5}, 12 #{6}, 13 #{1 4 2}, 21 #{3}, 22 #{7 4 2}, 23 #{9}, 31 #{1 2}}))
    (is (= dom2 {11 #{5}, 12 #{6}, 13 #{1 4 2}, 14 #{8}, 15 #{4}, 16 #{7}, 17 #{1 4 9 8}, 18 #{1 4 2 9}, 19 #{4 2 8}}))
    (is (= dom3 {71 #{1 3 9}, 41 #{1 2 9}, 11 #{5}, 81 #{3 2}, 51 #{7}, 21 #{3}, 91 #{1 3 2}, 61 #{1 3 9}, 31 #{1 2}}))))
(deftest access-test
  (let [dom1 {:x1 #{1 4 5}
              :x2 #{1}
              :x3 #{4}}]
    (is (= (access dom1 (compute-scc (graph-with-matching dom1 (max-matching dom1)))) {:x1 #{5 4 1}, :x3 #{4}, :x2 #{1}}))))
