(ns graphclj.tools-test
  (:require [clojure.test :refer :all]
            [graphclj.tools :refer :all]))


(deftest rank-test
  (testing "teste la clossnes clossnes all"
             (let [g {1 {:neigh #{0 4 3}, :degree 3, :close 3.5},
                       0 {:neigh #{1 3}, :degree 2, :close 3.0},
                       3 {:neigh #{0 1 2}, :degree 3, :close 3.5},
                       4 {:neigh #{1}, :degree 1, :close 2.333333333333333},
                       2 {:neigh #{3}, :degree 1, :close 2.333333333333333}}]
               (is (= (rank-nodes g :close) {0 {:neigh #{1 3}, :degree 2, :close 3.0, :rank 2}, 1 {:neigh #{0 4 3}, :degree 3, :close 3.5, :rank 3}, 2 {:neigh #{3}, :degree 1, :close 2.333333333333333, :rank 0}, 3 {:neigh #{0 1 2}, :degree 3, :close 3.5, :rank 3}, 4 {:neigh #{1}, :degree 1, :close 2.333333333333333, :rank 0}}))
               (is (= (rank-nodes g :degree) {0 {:neigh #{1 3}, :degree 2, :close 3.0, :rank 2}, 1 {:neigh #{0 4 3}, :degree 3, :close 3.5, :rank 3}, 2 {:neigh #{3}, :degree 1, :close 2.333333333333333, :rank 0}, 3 {:neigh #{0 1 2}, :degree 3, :close 3.5, :rank 3}, 4 {:neigh #{1}, :degree 1, :close 2.333333333333333, :rank 0}})))))

(deftest to-dot-test
  (testing "teste de la conversion en .dot"
   (let [g {1 {:neigh #{0 4 3}, :degree 3, :close 3.5},
             0 {:neigh #{1 3}, :degree 2, :close 3.0},
             3 {:neigh #{0 1 2}, :degree 3, :close 3.5},
             4 {:neigh #{1}, :degree 1, :close 2.333333333333333},
             2 {:neigh #{3}, :degree 1, :close 2.333333333333333}}]
        (is (= (to-dot (rank-nodes g :close)) "graph g{ 0 [style=filled color= 0.11764705882352941 0.7450980392156863 0.596078431372549] 1 [style=filled color= 0.1568627450980392 0.7843137254901961 0.6352941176470588] 2 [style=filled color= 0.0392156862745098 0.6666666666666666 0.5176470588235295] 3 [style=filled color= 0.1568627450980392 0.7843137254901961 0.6352941176470588] 4 [style=filled color= 0.0392156862745098 0.6666666666666666 0.5176470588235295]  0--3 0--1 1--4 1--0 3--2 }"))
        (is (= (to-dot (rank-nodes g :degree)) "graph g{ 0 [style=filled color= 0.11764705882352941 0.7450980392156863 0.596078431372549] 1 [style=filled color= 0.1568627450980392 0.7843137254901961 0.6352941176470588] 2 [style=filled color= 0.0392156862745098 0.6666666666666666 0.5176470588235295] 3 [style=filled color= 0.1568627450980392 0.7843137254901961 0.6352941176470588] 4 [style=filled color= 0.0392156862745098 0.6666666666666666 0.5176470588235295]  0--3 0--1 1--4 1--0 3--2 }")))))
