(ns erv.constant-structures.brute-force-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.constant-structures.brute-force :refer [cs-subsets quick-cs-subsets]]))

(deftest quick-cs-subsets-test
  (testing "Can find all the CS of Centaura. This test assumes `cs-subsets` works."
    (let [scale (mapv (fn [r] {:bounded-ratio r :bounding-period 2}) [33/32 9/8 7/6 5/4 4/3 11/8 3/2 14/9 5/3 7/4 15/8 2/1])
          min-scale-size 2]
      (is (= (map #(map :bounded-ratio %) (cs-subsets min-scale-size scale))
             (map #(map :bounded-ratio %) (quick-cs-subsets (range min-scale-size (count scale)) scale))))))
  (testing "Can find subsets of specific sizes"
    (let [scale (mapv (fn [r] {:bounded-ratio r :bounding-period 2}) [33/32 9/8 7/6 5/4 4/3 11/8 3/2 14/9 5/3 7/4 15/8 2/1])
          min-scale-size 2]
      (is (= (->> (cs-subsets min-scale-size scale)
                  (map #(map :bounded-ratio %))
                  (filter (fn [subset] (#{6 7} (count subset)))))
             (map #(map :bounded-ratio %) (quick-cs-subsets [6 7] scale)))))))
