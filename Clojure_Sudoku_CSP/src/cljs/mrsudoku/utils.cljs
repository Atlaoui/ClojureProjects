(ns mrsudoku.utils)

(defn concatv
  "Concatenate vectors."
  ([] [])
  ([v] v)
  ([v1 v2] (into v1 v2))
  ([v1 v2 & more]
   (into v1 (apply concat (cons v2 more)))))
