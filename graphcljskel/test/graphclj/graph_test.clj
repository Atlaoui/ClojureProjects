(ns graphclj.graph-test
  (:require [clojure.test :refer :all]
            [graphclj.graph :refer :all]))


(deftest g-test
  (testing "Teston la generation de graph"
    (is (= (gen-graph '("0 1" "2 3" "0 2")) {0 {:neigh #{1 2}}, 1 {:neigh #{0}}, 2 {:neigh #{0 3}}, 3 {:neigh #{2}}}))
    (is (= (gen-graph '("0 1" "2 3" "0 2" "4 2" "3 4" "2 5")) {0 {:neigh #{1 2}}, 1 {:neigh #{0}}, 2 {:neigh #{0 4 3 5}}, 3 {:neigh #{4 2}}, 4 {:neigh #{3 2}}, 5 {:neigh #{2}}}))
    (is (= (gen-graph '()) {}))
    (is (= (gen-graph '("1 2")) {1 {:neigh #{2}}, 2 {:neigh #{1}}}))))

(deftest r-test
  (testing "Teston la generation aléatoir sur les cas sur"
    (is (= (erdos-renyi-rnd 10 0.0) {}))
    (is (= (erdos-renyi-rnd 2 0.0) {}))))
