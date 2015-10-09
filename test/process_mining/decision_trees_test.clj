(ns process-mining.decision-trees-test
  (:require [process-mining.decision-trees :refer :all]
            [clojure.test :refer :all]))

(deftest test-entropy
  (testing "test entropy of 160/100 is 0.9544"
    (is (==
         (with-precision 4
           (* 1 (bigdec (entropy [100 160]))))
         0.9544)))
  (testing "test entropy of 70/80 is 0.5436"
    (is (==
         (with-precision 4
           (* 1 (bigdec (entropy [70 80]))))
         0.5436))))

(deftest test-weighted-entropy
  (testing "test weighted entropy of population 0f 160 and entropies [30 80]
     and [70 80] is 0.749"
    (is (==
         (with-precision 3
           (* 1 (bigdec (weighted-entropy [[30 80] [70 80]]))))
         0.749))))

(deftest test-information-guide
  (testing "test info gain 10 to 4 is 6"
    (is (= (information-gain 10 4) 6)))
  (testing "test info gain 10 to 4 is 6"
    (is (= (information-gain 4 10) -6))))
