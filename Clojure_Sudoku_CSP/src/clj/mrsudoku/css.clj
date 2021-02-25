(ns mrsudoku.css
  (:require [garden.def :refer [defstyles]]))

(def block-clr1 "#E2E2F6")
(def block-clr2 "#F6E2E2")


(defstyles screen
  [:body {:color "black"}]

  [:.sudokuview {:min-width "200px" :width "320px" :height "200px"}]
  [:table.gridview {:border "1px solid lightgray" :padding "4pt" :min-width "30%" :max-width "80%" :min-height "30%"}]

  [:.init-cell {:color "blue" :font-weight "700"}]
  [:td.rowview {:border "1px solid blue"}]

  [:.blockview {:display "grid"
                :padding "5pt"
                :margin "4px "
                :grid-template-columns "1fr 1fr 1fr"
                :border "1px solid gray"}]

  [:.gridview {:display "grid"
               :grid-template-columns "1fr 1fr 1fr"}]

  [:.block-1 {:background block-clr1}]
  [:.block-2 {:background block-clr2}]
  [:.block-3 {:background block-clr1}]
  [:.block-4 {:background block-clr2}]
  [:.block-5 {:background block-clr1}]
  [:.block-6 {:background block-clr2}]
  [:.block-7 {:background block-clr1}]
  [:.block-8 {:background block-clr2}]
  [:.block-9 {:background block-clr1}]

  [:.cellview {:text-align "center" :border "1px solid lightgray" :margin "3px"}]
  [:.cellview [:input {:width "30px"}]]
  [:.conflict-cell [:input {:color "red" :border-color "red"}]]


)
