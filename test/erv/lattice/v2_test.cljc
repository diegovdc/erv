(ns erv.lattice.v2-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.lattice.v2 :refer [base-coords combine-nodes connect-nodes
                           get-point-data-difference make-connection
                           ratio->lattice-point ratios->lattice-data ref-ratio-in-ratio-edges?]]))

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

(deftest ratios->lattice-data-test
  (is (= {:period 2
          :min-x 0
          :max-x 80
          :min-y 0
          :max-y 0
          :data [{:ratio 3/2
                  :numerator 3
                  :denominator 2
                  :numer-factors [3]
                  :denom-factors [2]
                  :coords {:x 40 :y 0}}
                 {:ratio 9/8
                  :numerator 9
                  :denominator 8
                  :numer-factors [3 3]
                  :denom-factors [2 2 2]
                  :coords {:x 80 :y 0}}
                 {:ratio 2
                  :numerator 2
                  :denominator 1
                  :numer-factors [2]
                  :denom-factors []
                  :coords {:x 0 :y 0}}]
          :edges [[{:x 40 :y 0} {:x 80 :y 0}]
                  [{:x 40 :y 0} {:x 0 :y 0}]]}
         (ratios->lattice-data base-coords  [3/2 9/8 2/1])))
  (testing "Will add metadata to edges"
    (is (= [{:diff 1
             :single-factor-diff? true
             :num-diff {5 1}
             :denom-diff {}}
            {:diff 2
             :single-factor-diff? false
             :num-diff {3 2}
             :denom-diff {}}]
           (map meta (:edges (ratios->lattice-data base-coords  [5/4 9/8 2/1])))))))

(deftest combine-nodes-test
  (is (= {{:ratio 3/2
           :numerator 3
           :denominator 2
           :numer-factors [3]
           :denom-factors [2]
           :coords {:x 40 :y 0}} [{:ratio 2
                                   :numerator 2
                                   :denominator 1
                                   :numer-factors [2]
                                   :denom-factors []
                                   :coords {:x 0 :y 0}}]
          {:ratio 2
           :numerator 2
           :denominator 1
           :numer-factors [2]
           :denom-factors []
           :coords {:x 0 :y 0}} [{:ratio 3/2
                                  :numerator 3
                                  :denominator 2
                                  :numer-factors [3]
                                  :denom-factors [2]
                                  :coords {:x 40 :y 0}}]}
         (combine-nodes
          {3/2 {:ratio 3/2 :numerator 3 :denominator 2 :numer-factors [3] :denom-factors [2] :coords {:x 40 :y 0}}
           2 {:ratio 2 :numerator 2 :denominator 1 :numer-factors [2] :denom-factors [] :coords {:x 0 :y 0}}}))))

(deftest connect-nodes-test
  (is (= #{#{3/2 9/8} #{3/2 2}}
         (connect-nodes
          2
          (combine-nodes
           {3/2 {:ratio 3/2 :numerator 3 :denominator 2 :numer-factors [3] :denom-factors [2] :coords {:x 40 :y 0}}
            9/8 {:ratio 9/8 :numerator 9 :denominator 8 :numer-factors [3 3] :denom-factors [2 2 2] :coords {:x 80 :y 0}}
            2 {:ratio 2 :numerator 2 :denominator 1 :numer-factors [2] :denom-factors [] :coords {:x 0 :y 0}}}))))
  (testing "Can connect nodes which are not single-factor-difference if that doesn't exist"
    (is (= #{#{2 9/8}}
           (connect-nodes
            2
            (combine-nodes
             (->> [2 9/8]
                  (map #(ratio->lattice-point % base-coords))
                  (into {}))))))
    (is (= #{#{2 25/16} #{9/8 2}}
           (connect-nodes
            2
            (combine-nodes
             (->> [2 9/8 25/16]
                  (map #(ratio->lattice-point % base-coords))
                  (into {}))))))
    (is (= #{#{2 5/4} #{5/4 25/16} #{9/8 2}}
           (connect-nodes
            2
            (combine-nodes
             (->> [2 9/8 5/4 25/16]
                  (map #(ratio->lattice-point % base-coords))
                  (into {}))))))))

(deftest make-connection-test
  (testing "Can connect nodes at a factor difference greater than 1"
    (is (= #{#{9/8 2}}
           (make-connection #{2}
                            #{}
                            2
                            {:ratio 2,
                             :numerator 2,
                             :denominator 1,
                             :numer-factors [2],
                             :denom-factors [],
                             :coords {:x 0, :y 0}}
                            {:ratio 9/8
                             :numerator 9
                             :denominator 8
                             :numer-factors [3 3]
                             :denom-factors [2 2 2]
                             :coords {:x 80, :y 0}}))))
  (testing "Will not connect nodes if factor difference is greater than stated"
    (is (= #{}
           (make-connection #{0 1}
                            #{}
                            2
                            {:ratio 2,
                             :numerator 2,
                             :denominator 1,
                             :numer-factors [2],
                             :denom-factors [],
                             :coords {:x 0, :y 0}}
                            {:ratio 25/4,
                             :numerator 25,
                             :denominator 4,
                             :numer-factors [5 5],
                             :denom-factors [2 2],
                             :coords {:x 0, :y -80}})))))

(deftest ref-ratio-in-ratio-edges?-test
  (is (true? (ref-ratio-in-ratio-edges? 3/2 #{#{3/2 9/8} #{3/2 2}})))
  (is (false? (ref-ratio-in-ratio-edges? 5/4 #{#{3/2 9/8} #{3/2 2}}))))
