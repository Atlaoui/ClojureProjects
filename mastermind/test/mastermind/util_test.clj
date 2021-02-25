(ns mastermind.util-test
  (:require [midje.sweet :refer :all]
            [mastermind.util :refer :all]))





(fact "le solveur Dum-dum est fonctionnel"
      (build-answer 4 [:good :color :bad :bad] [:rouge :jaune :vert :bleu] :blanc)
      => [:rouge :blanc :blanc :blanc]
      (build-answer 4 [:good :color :bad :bad] [:rouge :jaune :vert :bleu] nil)
      =>[:rouge nil nil nil]
      (build-answer 4 [:good :color :bad :good] [:rouge :jaune :vert :bleu] :noir)
      =>[:rouge :noir :noir :bleu]
      (build-answer 4 [:good :good :good :good] [:rouge :jaune :vert :bleu] :noir)
      =>[:rouge :jaune :vert :bleu]
      (build-answer 4 [:bad :bad :bad :bad] [:rouge :jaune :vert :bleu] :noir)
      =>[:noir :noir :noir :noir])

(fact "le parsage est ok"
      (parse-rep "good good rouge blanc")
      => [:good :rouge :rouge :blanc]
      (parse-read "1")
      => 1)

(fact "La victoire ou la défaite"
      (win? [:vert :rouge :blanc :violet] [:vert :rouge :blanc :violet])
      => true
      (win? [:blanc :rouge :blanc :violet] [:vert :rouge :blanc :violet])
      => false)



(fact "Le bon format"
     (= (freqs-dispo [:vert :rouge :blanc :violet] (parse-rep "good good rouge blanc")
          (freqs-dispo [:vert :rouge :blanc :violet] [:good :rouge :rouge :blanc])))
    => true)



(fact "Le `code-secret` a l'air aléatoire."
    (> (count (filter true? (map not=
                                (repeatedly 20 #(code-secret 4))
                                (repeatedly 20 #(code-secret 4)))))
      0)
 => true)


(fact "`indications` sont les bonnes."
    (indications [:rouge :rouge :vert :bleu]
      [:vert :rouge :bleu :jaune])
    => [:color :good :color :bad]

     (indications [:rouge :rouge :vert :bleu]
                [:bleu :rouge :vert :jaune])
    => [:color :good :good :bad]

    (indications [:rouge :rouge :vert :bleu]
                [:rouge :rouge :vert :bleu])
    => [:good :good :good :good]

    (indications [:rouge :rouge :vert :vert]
        [:vert :bleu :rouge :jaune])
    => [:color :bad :color :bad])



(fact "les `frequences` suivantes sont correctes."
    (frequences [:rouge :rouge :vert :bleu :vert :rouge])
    => {:rouge 3 :vert 2 :bleu 1}

    (frequences [:rouge :vert :bleu])
    => {:rouge 1 :vert 1 :bleu 1}

  (frequences [1 2 3 2 1 4]) => {1 2, 2 2, 3 1, 4 1})




(fact "Les fréquences disponibles de `freqs-dispo` sont correctes."
    (freqs-dispo [:rouge :rouge :bleu :vert :rouge]
                [:good :color :bad :good :color]
      => {:bleu 1, :rouge 2, :vert 0}))



  ;;Pour la fonction filtre-indications
(fact "Le `filtre-indications` fonctionne bien."
        (filtre-indications [:rouge :rouge :vert :bleu]
                            [:vert :rouge :bleu :jaune]
                            [:color :good :color :bad])
        => [:color :good :color :bad]

        (filtre-indications [:rouge :vert :rouge :bleu]
                            [:rouge :rouge :bleu :rouge]
                            [:good :color :color :color])
  => [:good :color :color :bad])
