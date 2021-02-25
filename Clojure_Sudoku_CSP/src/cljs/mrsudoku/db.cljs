(ns mrsudoku.db
  (:require [mrsudoku.model.grid :as grid]))

(def default-db
  {:name "MrSudoku"
   :grid grid/sudoku-grid
   :conflicts {}})


