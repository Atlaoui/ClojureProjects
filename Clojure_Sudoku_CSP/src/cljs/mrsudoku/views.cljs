(ns mrsudoku.views
  (:require
   [re-frame.core :as re-frame]
   [mrsudoku.subs :as subs]
   [mrsudoku.events :as events]
   [mrsudoku.model.grid :as grid]))

(defn cellv-value [cv]
  (-> cv .-target .-value))

(defn cell-view [cell block-index cell-col cell-row cell-conflicts]
  (let [classes (-> ["cellview" (str "block-" block-index) (str "col-" cell-col) (str "row-" cell-row)]
                    (conj (case (:status cell)
                            :init "init-cell"
                            :empty "empty-cell"
                            :set "set-cell"
                            "unknown-cell")))
        classes' (if (nil? cell-conflicts)
                   classes
                   (conj classes "conflict-cell"))
        cell-classes {:class (clojure.string/join " " classes')}]
    (case (:status cell)
      :init [:div cell-classes (str (:value cell))]
      (:empty :set :conflict) [:div cell-classes [:input {:type "number" :min "1" :max "9" :value (str (:value cell))
                                                          :on-change #(re-frame/dispatch [:cell-value-changed [(cellv-value %) cell-col cell-row]])}]]
      [:div cell-classes (str "<<Unknown:" cell ">>")])))

(defn block-view [grid conflicts block-index]
  (apply vector :div {:class (str "blockview" " block-" block-index)}
         (grid/reduce-block (fn [cviews _ cx cy cell]
                              (conj cviews (cell-view cell block-index cx cy (get conflicts [cx cy]))))
                            [] (grid/block grid block-index) block-index)))

(defn grid-view []
  (let [model (re-frame/subscribe [::subs/grid-changed])]
    (println "[view]" (str "grid=\n" (grid/grid->str (:grid @model))))
    (println "[view] conflicts=" (:conflicts @model))
    (apply vector :div {:class "gridview"}
           (loop [b 1, bviews []]
             (if (<= b 9)
               (let [mmodel @model
                     grid (:grid mmodel)
                     conflicts (:conflicts mmodel)]
                 (recur (inc b) (conj bviews (block-view grid conflicts b))))
               bviews)))))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1 @name]
     [:div {:class "control-panel"}
      [:button {:type "Button" :on-click #(re-frame/dispatch [:initialize])} "Reinitialize"]
      [:button {:type "Button" :on-click #(re-frame/dispatch [:solve-button-clicked []])} "Solve"]
      [:button {:type "Button" :on-click #(re-frame/dispatch [:solve-button-clicked-4 []])} "Generate"]
      [:button {:type "Button" :on-click #(re-frame/dispatch [:solve-button-clicked-2 []])} "Gen UniqSol"]
      [:button {:type "Button" :on-click #(re-frame/dispatch [:solve-button-clicked-3 []])} "Help?"]]
     [:div {:class "sudokuview"} [grid-view]]]))
