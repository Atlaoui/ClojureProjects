(ns graphclj.core
  (:require [graphclj.graph :as graph]
            [graphclj.tools :as tools]
            [graphclj.centrality :as central])
  (:gen-class))

(declare menu)
(declare pars-args)

(defn pars-args
   [arg]
  (loop [va arg res ()]
    (if (seq va)
      (recur (rest va) (conj res (str (ffirst va)" "(second (first va)))))
     res)))

(defn do-it-all
  "applique toutes les fonction au graph d'un fichier puis retourn le .dot en fonction de :close"
  [sef]
  (let [g (central/degrees (central/closeness-all (graph/gen-graph (tools/readfile sef))))]
    (println g)
    (println "le rank a partire de :close puis de degree")
    (println (tools/rank-nodes g :close))
    (println (tools/rank-nodes g :degree))
    (println " ")
    (println " ")
    (println "Entrer le noeud dont vous vouler connaitre la distance")
    (println (central/distance g (read-string (read-line))))
    (println (tools/to-dot (tools/rank-nodes g :close)))
    (tools/to-dot (tools/rank-nodes g :close))))

(defn dist-teste
  []
  (let[ g  (graph/erdos-renyi-rnd 5 0.5)]
    (println g)
    (println "entrer une valeur :")
    (println (central/distance g (read-string (read-line))))))

(defn menu
  []
  (println "---------------------GraphProject------------------------")
  (println "pour crer votre graph metre des argument dans le lein run sous le format 1 2 2 4 ....")
  (println "1 - Pour generer un raphe donner un nombre de noeud et une proba")
  (println "2 - Pour generer un .dot d'un graph generer aléatoirement")
  (println "3 - Pour avoir la distance d'un graph aleatoire")
  (println "4 - Pour generer un graph depuis un fichier")
  (println "5 - Pour appliquer toutes les fonction a un  fichier passer en parametre")
  (println "6 - Pour quiter")
  (let [val (read-line)]
     (cond
      (= val "1") (do (println (graph/erdos-renyi-rnd 5 0.5)) (menu))
      (= val "2") (do (println (tools/to-dot (tools/rank-nodes (central/closeness-all (graph/erdos-renyi-rnd 5 0.5)) :close ))) (menu))
      (= val "3") (do (dist-teste) (menu))
      (= val "4") (do (println "entrer le nom d(un fichier :")(graph/gen-graph (tools/readfile (read-line))) (menu))
      (= val "5") (do (println "entrer le nom d'un fichier :") (do-it-all (read-line)))
      (= val "6") (println  "bye bye")
      :else (menu))))



(defn -main
  [& args]

  (if (empty? args)
    (menu)
   (println (graph/gen-graph (pars-args (partition 2  args))))))
