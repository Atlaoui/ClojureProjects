(ns graphclj.centrality-test
  (:require [clojure.test :refer :all]
            [graphclj.centrality :refer :all]))


(deftest degree-teste
  (testing "teste du calcule de de degree"
    (let [g {1 {:neigh #{0 4 3}},
             0 {:neigh #{1 3}},
             3 {:neigh #{0 1 2}},
             4 {:neigh #{1}},
             2 {:neigh #{3}}} g2 {0 {:neigh #{1 2}}, 1 {:neigh #{0}}, 2 {:neigh #{0 4 3 5}}, 3 {:neigh #{4 2}}, 4 {:neigh #{3 2}}, 5 {:neigh #{2}}}]

         (is (degrees g) {1 {:neigh #{0 4 3}, :degree 3}, 0 {:neigh #{1 3}, :degree 2}, 3 {:neigh #{0 1 2}, :degree 3}, 4 {:neigh #{1}, :degree 1}, 2 {:neigh #{3}, :degree 1}})
         (is (degrees g2) {0 {:neigh #{1 2}, :degree 2}, 1 {:neigh #{0}, :degree 1}, 2 {:neigh #{0 4 3 5}, :degree 4}, 3 {:neigh #{4 2}, :degree 2}, 4 {:neigh #{3 2}, :degree 2}, 5 {:neigh #{2}, :degree 1}}))))


(deftest centr1-test
    (testing "teste la clossnes clossnes all"
      (let [g {1 {:neigh #{0 4 3}},
               0 {:neigh #{1 3}},
               3 {:neigh #{0 1 2}},
               4 {:neigh #{1}},
               2 {:neigh #{3}}}]
         (is (= (closeness g 1) 3.5))
         (is (= (closeness g 0) 3.0))
         (is (= (closeness g 2) 2.333333333333333))
         (is (= (closeness g 3) 3.5))
         (is (= (closeness g 4) 2.333333333333333))
         (is (= (closeness-all g) {1 {:neigh #{0 4 3}, :close 3.5}, 0 {:neigh #{1 3}, :close 3.0}, 3 {:neigh #{0 1 2}, :close 3.5}, 4 {:neigh #{1}, :close 2.333333333333333}, 2 {:neigh #{3}, :close 2.333333333333333}})))))

(deftest dist-test
    (testing "teste du calcule de distance"
        (let [g {1 {:neigh #{0 4 3}},
                 0 {:neigh #{1 3}},
                 3 {:neigh #{0 1 2}},
                 4 {:neigh #{1}},
                 2 {:neigh #{3}}}
              g2 {0 {:neigh #{1 3 2}},
                  3 {:neigh #{0 1 2}},
                  2 {:neigh #{0 1 3}},
                  1 {:neigh #{0 3 2}}}
              g3 {0 {:neigh #{1 3 2}},
                  3 {:neigh #{0 1 2}},
                  2 {:neigh #{0 1 3}},
                  1 {:neigh #{0 3 2}}}
              g4 {0 {:neigh #{7 1 4 6 3 2 9 5 8}},
                  7 {:neigh #{0 1 4 6 3 2 9 5 8}},
                  1 {:neigh #{0 7 4 6 3 2 9 5 8}},
                  4 {:neigh #{0 7 1 6 3 2 9 5 8}},
                  6 {:neigh #{0 7 1 4 3 2 9 5 8}},
                  3 {:neigh #{0 7 1 4 6 2 9 5 8}},
                  2 {:neigh #{0 7 1 4 6 3 9 5 8}},
                  9 {:neigh #{0 7 1 4 6 3 2 5 8}},
                  5 {:neigh #{0 7 1 4 6 3 2 9 8}},
                  8 {:neigh #{0 7 1 4 6 3 2 9 5}}}
              g5 {0 {:neigh #{7 6 3 2 9 5 8}},
                  7 {:neigh #{0 4 5 8}},
                  1 {:neigh #{3}},
                  4 {:neigh #{7 6 2 9 5}},
                  6 {:neigh #{0 4 3 2 9 5 8}},
                  3 {:neigh #{0 1 6 2 9 8}},
                  2 {:neigh #{0 4 6 3 9}},
                  9 {:neigh #{0 4 6 3 2 5}},
                  5 {:neigh #{0 7 4 6 9}},
                  8 {:neigh #{0 7 6 3}}}]
          (is (= (distance g 0) {2 2.0, 4 2.0, 3 1.0, 1 1.0, 0 0.0}))
          (is (= (distance g 1) {2 2.0, 4 1.0, 3 1.0, 0 1.0, 1 0.0}))
          (is (= (distance g 2) {4 3.0, 1 2.0, 0 2.0, 3 1.0, 2 0.0}))
          (is (= (distance g 3) {4 2.0, 2 1.0, 1 1.0, 0 1.0, 3 0.0}))
          (is (= (distance g 4) {2 3.0, 3 2.0, 0 2.0, 1 1.0, 4 0.0}))
          (is (= (distance g2 0) {3 1.0, 2 1.0, 1 1.0, 0 0.0}))
          (is (= (distance g2 1) {3 1.0, 2 1.0, 0 1.0, 1 0.0}))
          (is (= (distance g2 2) {3 1.0, 1 1.0, 0 1.0, 2 0.0}))
          (is (= (distance g2 3) {2 1.0, 1 1.0, 0 1.0, 3 0.0}))
          (is (= (distance g3 0) {3 1.0, 2 1.0, 1 1.0, 0 0.0}))
          (is (= (distance g3 1) {3 1.0, 2 1.0, 0 1.0, 1 0.0}))
          (is (= (distance g3 2) {3 1.0, 1 1.0, 0 1.0, 2 0.0}))
          (is (= (distance g3 3) {2 1.0, 1 1.0, 0 1.0, 3 0.0}))
          (is (= (distance g4 0) {0 0.0, 7 1.0, 1 1.0, 4 1.0, 6 1.0, 3 1.0, 2 1.0, 9 1.0, 5 1.0, 8 1.0}))
          (is (= (distance g4 1) {0 1.0, 7 1.0, 1 0.0, 4 1.0, 6 1.0, 3 1.0, 2 1.0, 9 1.0, 5 1.0, 8 1.0}))
          (is (= (distance g4 2) {0 1.0, 7 1.0, 1 1.0, 4 1.0, 6 1.0, 3 1.0, 2 0.0, 9 1.0, 5 1.0, 8 1.0}))
          (is (= (distance g4 3) {0 1.0, 7 1.0, 1 1.0, 4 1.0, 6 1.0, 3 0.0, 2 1.0, 9 1.0, 5 1.0, 8 1.0}))
          (is (= (distance g5 0) {0 0.0, 7 1.0, 1 3.0, 4 2.0, 6 1.0, 3 1.0, 2 1.0, 9 1.0, 5 1.0, 8 1.0}))
          (is (= (distance g5 1) {0 2.0, 7 4.0, 1 0.0, 4 5.0, 6 2.0, 3 1.0, 2 2.0, 9 2.0, 5 3.0, 8 2.0}))
          (is (= (distance g5 2) {0 1.0, 7 3.0, 1 5.0, 4 1.0, 6 1.0, 3 1.0, 2 0.0, 9 1.0, 5 2.0, 8 4.0}))
          (is (= (distance g5 3) {0 1.0, 7 3.0, 1 1.0, 4 4.0, 6 1.0, 3 0.0, 2 1.0, 9 1.0, 5 2.0, 8 1.0}))
          (is (= (distance g5 4) {0 2.0, 7 1.0, 1 6.0, 4 0.0, 6 1.0, 3 2.0, 2 1.0, 9 1.0, 5 1.0, 8 3.0}))
          (is (= (distance g5 5) {0 1.0, 7 1.0, 1 6.0, 4 1.0, 6 1.0, 3 2.0, 2 2.0, 9 1.0, 5 0.0, 8 3.0}))
          (is (= (distance g5 6) {0 1.0, 7 2.0, 1 4.0, 4 1.0, 6 0.0, 3 1.0, 2 1.0, 9 1.0, 5 1.0, 8 1.0}))
          (is (= (distance g5 7) {0 1.0, 7 0.0, 1 7.0, 4 1.0, 6 2.0, 3 2.0, 2 2.0, 9 3.0, 5 1.0, 8 1.0}))
          (is (= (distance g5 8) {0 1.0, 7 1.0, 1 4.0, 4 4.0, 6 1.0, 3 1.0, 2 2.0, 9 3.0, 5 2.0, 8 0.0})))))
