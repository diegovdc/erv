(ns erv.lattice.v2-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.lattice.v2 :refer [get-point-data-difference maybe-make-d1-connection]]))

(deftest maybe-make-d1-connection-test
  (is (= #{#{"15/14" "10/7"}}
         (maybe-make-d1-connection
          #{}
          2
          {:ratio "15/14"
           :numerator 15
           :denominator 14
           :numer-factors [3 3]
           :denom-factors []
           :coords {:x 27, :y 29}}
          {:ratio "10/7"
           :numerator 10
           :denominator 7
           :numer-factors [3]
           :denom-factors []
           :coords {:x -13, :y 29}})))
  (testing "Shouldn't make a connection because it is at adistance of 2 3's"
    (is (= #{}
           (maybe-make-d1-connection
            #{}
            2
            {:ratio "15/14"
             :numerator 15
             :denominator 14
             :numer-factors [3 3 3]
             :denom-factors []
             :coords {:x 27, :y 29}}
            {:ratio "10/7"
             :numerator 10
             :denominator 7
             :numer-factors [3]
             :denom-factors []
             :coords {:x -13, :y 29}})))))

(deftest get-point-data-difference-test
  (is (= {3 2, 5 0}
         (get-point-data-difference 2
                                    {:numer-factors [2 3 3 3 5]
                                     :denom-factors []}
                                    {:numer-factors [2 3 5]
                                     :denom-factors []}
                                    :numer-factors)))
  (is (= {3 0, 5 1}
         (get-point-data-difference 2
                                    {:numer-factors [3 5]
                                     :denom-factors []}
                                    {:numer-factors [3]
                                     :denom-factors []}
                                    :numer-factors)))
  (is (= {3 1}
         (get-point-data-difference 2
                                    {:ratio 3/2
                                     :numerator 3
                                     :denominator 2
                                     :numer-factors [3]
                                     :denom-factors [2]
                                     :coords {:x 40, :y 0}}
                                    {:ratio 9/8
                                     :numerator 9
                                     :denominator 8
                                     :numer-factors [3 3]
                                     :denom-factors [2 2 2]
                                     :coords {:x 80, :y 0}}
                                    :numer-factors))))
