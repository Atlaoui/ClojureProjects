(ns mrsudoku.generate-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mrsudoku.model.grid :as g]
            [mrsudoku.model.generate :as gen :refer [set-all-to-init
                                                     init-cell
                                                     clear-cell
                                                     isUniqueSol
                                                     generate-base-grid]]))


(def ^:private sudoku-grid @#'g/sudoku-grid)

(deftest init-cell-test
  (is (= (init-cell {:value 7, :status :empty}) {:value 7, :status :init}))
  (is (= (init-cell {:value 7, :status :init}) {:value 7, :status :init})))

(deftest set-all-to-init-test
  (is (= (set-all-to-init sudoku-grid) sudoku-grid)))

;;(defn randfn
;;  ([] (randfn (java.util.Random.)))
 ;; ([r] #(.nextDouble r)))

;;(def ^:private source1 (randfn (java.util.Random. 37)))


;;(deftest clear-cell-test
;;  (is (= (clear-cell {:value 5, :status :init} (source1)) {:value 5, :status :init}))
;;  (is (= (clear-cell {:value 5, :status :init} (source1)) {:value 5, :status :init}))
 ;; (is (= (clear-cell {:value 5, :status :init} (source1)) {:value 5, :status :init}))
;;  (is (= (clear-cell {:value 5, :status :init} (source1)) {:status :empty})))
(deftest isUniqueSol-test
  (let [my-grille-res [[[{:value 4, :status :set}
                         {:value 7, :status :set}
                         {:value 5, :status :set}
                         {:value 6, :status :set}
                         {:value 8, :status :init}
                         {:value 9, :status :init}
                         {:value 1, :status :set}
                         {:value 3, :status :set}
                         {:value 2, :status :set}]
                        [{:value 1, :status :init}
                         {:value 3, :status :set}
                         {:value 6, :status :init}
                         {:value 5, :status :set}
                         {:value 2, :status :set}
                         {:value 7, :status :init}
                         {:value 4, :status :init}
                         {:value 9, :status :init}
                         {:value 8, :status :init}]
                        [{:value 8, :status :init}
                         {:value 9, :status :init}
                         {:value 2, :status :set}
                         {:value 1, :status :init}
                         {:value 3, :status :init}
                         {:value 4, :status :set}
                         {:value 5, :status :init}
                         {:value 7, :status :init}
                         {:value 6, :status :init}]]
                       [[{:value 2, :status :init}
                         {:value 1, :status :set}
                         {:value 4, :status :init}
                         {:value 7, :status :init}
                         {:value 5, :status :set}
                         {:value 8, :status :init}
                         {:value 9, :status :set}
                         {:value 6, :status :set}
                         {:value 3, :status :set}]
                        [{:value 6, :status :set}
                         {:value 5, :status :set}
                         {:value 3, :status :set}
                         {:value 9, :status :init}
                         {:value 4, :status :set}
                         {:value 1, :status :init}
                         {:value 7, :status :set}
                         {:value 8, :status :init}
                         {:value 2, :status :init}]
                        [{:value 7, :status :init}
                         {:value 8, :status :set}
                         {:value 9, :status :set}
                         {:value 2, :status :init}
                         {:value 6, :status :set}
                         {:value 3, :status :set}
                         {:value 4, :status :init}
                         {:value 1, :status :init}
                         {:value 5, :status :set}]]
                       [[{:value 8, :status :set}
                         {:value 9, :status :set}
                         {:value 1, :status :init}
                         {:value 5, :status :set}
                         {:value 4, :status :set}
                         {:value 7, :status :init}
                         {:value 3, :status :set}
                         {:value 2, :status :init}
                         {:value 6, :status :set}]
                        [{:value 2, :status :init}
                         {:value 6, :status :init}
                         {:value 5, :status :set}
                         {:value 3, :status :init}
                         {:value 1, :status :init}
                         {:value 9, :status :set}
                         {:value 8, :status :set}
                         {:value 7, :status :set}
                         {:value 4, :status :init}]
                        [{:value 3, :status :set}
                         {:value 4, :status :set}
                         {:value 7, :status :init}
                         {:value 6, :status :set}
                         {:value 2, :status :set}
                         {:value 8, :status :init}
                         {:value 9, :status :set}
                         {:value 5, :status :set}
                         {:value 1, :status :init}]]]
        my-grille [[[{:value 4, :status :set}
                     {:value 7, :status :set}
                     {:value 5, :status :set}
                     {:value 6, :status :set}
                     {:value 8, :status :init}
                     {:value 9, :status :init}
                     {:value 1, :status :set}
                     {:value 3, :status :set}
                     {:value 2, :status :set}]
                    [{:value 1, :status :init}
                     {:value 3, :status :set}
                     {:value 6, :status :init}
                     {:value 5, :status :set}
                     {:value 2, :status :set}
                     {:value 7, :status :init}
                     {:value 4, :status :init}
                     {:value 9, :status :init}
                     {:value 8, :status :init}]
                    [{:value 8, :status :init}
                     {:value 9, :status :init}
                     {:value 2, :status :set}
                     {:value 1, :status :init}
                     {:value 3, :status :init}
                     {:value 4, :status :set}
                     {:value 5, :status :init}
                     {:value 7, :status :init}
                     {:value 6, :status :init}]]
                   [[{:value 2, :status :init}
                     {:value 1, :status :set}
                     {:value 4, :status :init}
                     {:value 7, :status :init}
                     {:value 5, :status :set}
                     {:value 8, :status :init}
                     {:value 9, :status :set}
                     {:value 6, :status :set}
                     {:value 3, :status :set}]
                    [{:value 6, :status :set}
                     {:value 5, :status :set}
                     {:value 3, :status :set}
                     {:value 9, :status :init}
                     {:value 4, :status :set}
                     {:value 1, :status :init}
                     {:value 7, :status :set}
                     {:value 8, :status :init}
                     {:value 2, :status :init}]
                    [{:value 7, :status :init}
                     {:value 8, :status :set}
                     {:value 9, :status :set}
                     {:value 2, :status :init}
                     {:value 6, :status :set}
                     {:value 3, :status :set}
                     {:value 4, :status :init}
                     {:value 1, :status :init}
                     {:value 5, :status :set}]]
                   [[{:value 8, :status :set}
                     {:value 9, :status :set}
                     {:status :empty}
                     {:value 5, :status :set}
                     {:value 4, :status :set}
                     {:value 7, :status :init}
                     {:value 3, :status :set}
                     {:value 2, :status :init}
                     {:value 6, :status :set}]
                    [{:value 2, :status :init}
                     {:value 6, :status :init}
                     {:value 5, :status :set}
                     {:status :empty}
                     {:value 1, :status :init}
                     {:value 9, :status :set}
                     {:value 8, :status :set}
                     {:value 7, :status :set}
                     {:value 4, :status :init}]
                    [{:value 3, :status :set}
                     {:value 4, :status :set}
                     {:value 7, :status :init}
                     {:value 6, :status :set}
                     {:status :empty}
                     {:status :empty}
                     {:value 9, :status :set}
                     {:value 5, :status :set}
                     {:value 1, :status :init}]]]]
   (is (= (first (isUniqueSol (generate-base-grid) my-grille-res)) false))
   (is (= (first (isUniqueSol my-grille my-grille-res)) true))))
