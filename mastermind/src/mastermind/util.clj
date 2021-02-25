(ns mastermind.util
 (:require [clojure.string :as str]))

(declare code-secret)
(declare frequences)
(declare indications)
(declare freqs-dispo)
(declare filtre-indications)
(declare win?)
(declare menu)
(declare jouer)
(declare dum-solver)
(declare parse-read)
(declare parse-rep)
(declare restart)
(declare restart)
(declare build-answer)

;; creer un code secret
(defn code-secret
    [n]
 (let [couleurs [:rouge :bleau :vert :jaune :noir :blanc]]
    (into [](repeatedly n #(rand-nth couleurs)))))

(defn indications
    [vecChoix vecRep]
    (loop [index 0 ,taille (count vecChoix) ,vecRes []]
        (if (< index taille)
            (recur (inc index) taille  (conj vecRes (cond
                                                     (= (get vecChoix index) (get vecRep index)) :good
                                                     (some #(= (get vecRep index) %) vecChoix) :color
                                                      :else :bad)))
         vecRes)))


(defn frequences
    [vect]
 (frequencies vect))



(defn freqs-dispo
    [v1 v2]
    (loop [pos 0 , taille (count v1) , res (frequences v1)]
         (if (< pos taille)
            (recur (inc pos) taille (if (= (get v2 pos) :good)
                                     (update res (get v1 pos) dec)
                                     res))
          res)))



(defn filtre-indications
  [vTester reponse resultat]
  (loop [pos 0
         ,res []
         ,freq (freqs-dispo vTester resultat)]
    (if (< pos (count vTester))
      (if (= (compare (get resultat pos) :color) 0)
        (if (= 0 (get freq (get reponse pos)))
          (recur (inc pos) (assoc res pos :bad) freq)
          (recur (inc pos) (conj res (get resultat pos)) (assoc freq (get reponse pos) (dec (get freq (get reponse pos))))))
        (recur (inc pos) (conj res (get resultat pos)) freq))
      res)))





;;lance touts le jeux et est utiliser pour touts relancez a chaque partie
(defn menu
  []
  (println "Jouant")
  (println "Mastermind")
  (println "1-Jouer a mastermind")
  (println "2- Lancez le solver Dum-dum")
  (println "3-bye bye")
  (let [val (read-line)]
    (cond
     (= val "1") (jouer)
     (= val "2") (dum-solver)
     :else nil)))

;;voir si avec le temps en peux moduler la taille d code secret
(defn jouer
  []
  (println "*************Mastermind****************")
  (println "en combien de coups voulez voous jouez")
  (println "Tapez :")
  (println "1 -Mastermind niveau exper 4 coups")
  (println "2 -Mastermind niveau B2 8 coups")
  (println "3 -Mastermind niveau debutant 12 coups")
  (println "4 -Mastermind Je veux gangner 14 coups")
  (let [nb_coup (parse-read (read-line))]
    (let [secret_vector (code-secret 4)]
      (println "Le maitre a choisie 4 couleur parmis [rouge bleau vert jaune noir blanc]")
      (println "Le format des réponse doit etre jaune rouge vert bleu......")
      (println "A vous de jouer")
      (loop [coup_num 0]
        (println "coup num "coup_num "sur "nb_coup)
        (if (< coup_num nb_coup)
          (if (win? (parse-rep (read-line)) secret_vector)
            (do (println "YOUHOU JE MARCHE ET TU GG")
                (restart))
            (recur (inc coup_num)))
         (restart))))))


;;solver brut-force qui commence par generer une séquence aléatoirment et la teste puis passe par toutes les couleur une a une et remplace toutes les couleur qui ne sont pas
;;:good et ainsi de suite jusque a ce qu'il trouve ou qu'il n'est plus de coups possible
(defn dum-solver
 []
 (println "*************Mastermind****************")
 (println "Choisir un code secret de 4 parmis rouge bleau vert jaune noir blanc")
 (let [secret_vector (parse-rep (read-line))]
   (println "Me donner un nombre de tour")
   (println "1 -6 coups")
   (println "2 -8 coups")
   (println "3 -12 coups")
   (println "4 -14 coups")
   (println secret_vector)
              ;;ici en commence par génerer aléatoirment un vecteur
   (loop [cpt 0 , nb_coup (parse-read (read-line)) ,response_vector (code-secret 4) ,color_vector [:rouge :bleau :vert :jaune :noir :blanc]]
     (println cpt " " nb_coup " " response_vector " " color_vector)
     (if (< cpt nb_coup)
         (if (win? secret_vector response_vector)
             (do (println "MOUAHAHAHAHAHAHAAHAHHAAH JE GG")
                (restart))
          (recur (inc cpt) nb_coup (build-answer 4 (filtre-indications secret_vector response_vector (indications response_vector secret_vector)) response_vector (first color_vector)) (rest color_vector)))
       (println "J'AI PERDU NOOOOOONNNNNN")))))

;;(build-answer 4 (filtre-indications secret_vector response_vector (indications response_vector secret_vector) response_vector (first color_vector))) (rest color_vector)))))))



;;ont donne la couleur le vecteur d'indications ainsi que un vecteur de reponse et la taille du code a craquer
(defn build-answer
 [taille_code indicat_vector rep_vector next_color]
 (loop [i 0 , res []]
  (if (< i taille_code)
    (recur (inc i) (conj res (cond
                              (= (get indicat_vector i) :good) (get rep_vector i)
                              :else next_color)))
    res)))


;;(build-answer 4 [:good :color :bad :bad] [:rouge :jaune :vert :bleu] :blanc)


;;*********************************Fonction utilitaire ********************************************************

;;parse la réponse de l'utilisateur a la question nombre de coups
(defn parse-read
  [n]
  (cond
    (= n "1") 4
    (= n "2") 8
    (= n "3") 12
    (= n "4") 14
    :else (do (println "Un des numero afficher svp") (parse-read (read-line)))))

;;Fonction permetant de passer du format "x y z" a[:x :y :z]
(defn parse-rep
  [v]
  (into [](map keyword (str/split v #"\s+"))))




;;changer le som
(defn win? [s r]
  (if (= (count s) (count r))
    (let [ret_indic (filtre-indications s r (indications r s))]
       (do (println ret_indic))
       (if (= (get (frequencies ret_indic) :good) 4)
          true
        false))))

;;permet de relancer le jeux une fois terminer
(defn restart
  []
  (println "0 - Ont s'arret ici héhé")
  (println "1 - Ont retante")
  (let [res (read-line)]
    (if (= res "1")
      (menu)
     (println "A++"))))




;;(defn update-by
     ;;  [toU val f]
     ;;  (update toU val f)
     ;;  val)

     ;;(defn filtre-indications
     ;;    [v1,v2,v3]
     ;;    (loop [v v3 ,f1 (freqs-dispo v1 v3) ,res []]
     ;;          (if (seq v)
     ;;              (recur (rest v) f1 (conj res (cond
     ;;                                            (or (= (first v) :bad) (= (first v) :good)) (first v)
     ;;                                            (= (get f1 (first v)) 0)  :bad
     ;;                                            :else (update-by f1 (first v) dec)
     ;;            res)))
