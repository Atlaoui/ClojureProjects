(ns mrsudoku.subs
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (str (:name db))))


(re-frame/reg-sub
 ::grid-changed
 (fn [db]
   (console.log (str "grid:" (:name db)))
   {:grid (:grid db)
    :conflicts (:conflicts db)}))



