(ns clojure-life.test.core
  (:use [clojure-life.core])
  (:use [clojure.test]))

(deftest test-next-cell
  (are [c n e] (= e (next-cell c n))
       0 0 0
       0 1 0
       0 2 1
       0 3 0
       0 4 0
       0 5 0
       1 0 0
       1 1 0
       1 2 1
       1 3 1
       1 4 0
       1 5 0))

(deftest test-tor
  (are [x n e] (= e (tor x n))
       -1 3 2
       0 3 0
       1 3 1
       2 3 2
       3 3 0))

(deftest test-get-cell
  (let [w [[1 2 3]
           [4 5 6]
           [7 8 9]]]
    (are [r c e] (= e (get-cell r c w))
         0 0 1
         0 1 2
         0 2 3
         1 0 4
         1 1 5
         1 2 6
         2 0 7
         2 1 8
         2 2 9
         -1 -1 9
         -1 0 7
         -1 3 7
         3 3 1
         3 -1 3)))

(deftest test-neighbors
  (let [w [[0 0 0 1 1]
           [0 0 0 1 1]
           [0 0 0 1 0]
           [1 1 0 0 1]
           [1 0 0 1 0]]]
    
    (is (= [[3 1 3 4 5]
            [2 0 3 4 4]
            [4 2 3 3 5]
            [3 2 3 3 4]
            [4 3 3 3 6]]
             (for [r (range (count w))]
               (for [c (range (count (first w)))]
                 (neighbors r c w)))))))

(deftest test-update-cell
  (let [w [[0 0 0 1 1]
           [0 0 0 1 1]
           [0 0 0 1 0]
           [1 1 0 0 1]
           [1 0 0 1 0]]]

    (is (= '((0 0 0 0 0) (1 0 0 0 0) (0 1 0 1 0) (1 1 0 0 0) (0 0 0 1 0))
             (update-world w)))))
