(ns erv.cps.core-test
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest testing is]]
   [erv.cps.core :as cps :refer [->cps
                                 maps->data
                                 filter-scale
                                 set->maps
                                 bound-ratio
                                 find-subcps
                                 get-cps-description
                                 subcps-sets->map
                                 filter-subcps-map
                                 cps-intervals
                                 cps-intervals-as-ratios
                                 cps-intervals-by-denominator*
                                 cps-intervals-by-denominator
                                 set-chord]]))

(deftest ->cps-test
  (testing "Creates all possible combinations of the given generators"
    (is (= #{#{1 3} #{1 2} #{3 2}}
           (->cps 2 [1 2 3])))
    (is (= #{#{1 2 3}}
           (->cps 3 [1 2 3])))
    (is (= #{#{3 2 5} #{1 3 5} #{1 2 5} #{1 3 2}}
           (->cps 3 [1 2 3 5]))))
  (testing "Can take a set as an input"
    (is (= #{#{1} #{2} #{3}}
           (->cps 1 #{1 2 3})))
    (is (= #{#{1 3} #{1 2} #{3 2}}
           (->cps 2 #{1 2 3}))))
  (testing "If `size` is 0, it returns #{#{}}"
    (is (= #{#{}}
           (->cps 0 #{1 2 3}))))
  (testing "If `size` is > than `(count generators)`, it returns `#{#{}}` "
    (is (= #{#{}}
           (->cps 4 #{1 2 3})))))

(deftest filter-scale-test
  (let [hex (->> [1 3 5 7] (->cps 2) set->maps (bound-ratio 2)
                 (maps->data :bounded-ratio) :scale) ]
    (is (= #{#{7 1} #{1 5} #{1 3}}
           (->> (filter-scale hex #{1}) (map :set) set)))

    (is (= #{#{7 1} #{1 5} #{1 3} #{3 5} #{3 7}}
           (->> (filter-scale hex #{1 3}) (map :set) set)))))

(deftest find-subcps-test
  (testing "It returns a `::sub-cps-set` (set of CPSs), therefore guaranteeing uniqueness"
    (is (s/valid? ::cps/sub-cps-set (find-subcps [1 2 3 4 5 6] 3 2 4)))
    (is (s/valid? ::cps/sub-cps-set (find-subcps [1 2 3 4 5 6 7 8] 3 2 4))))

  (testing "There are 30 hexanies in an eikosany"
    (is (= 30 (count (find-subcps [1 2 3 4 5 6] 3 2 4)))))

  (testing "There are 30 tetrads (major and minor) in an eikosay"
    (is (= 30 (count (set/union (find-subcps [1 2 3 4 5 6] 3 1 4)
                                (find-subcps [1 2 3 4 5 6] 3 3 4)))))))

(deftest get-cps-description-test
  (testing "Creates a string representation of a CPS"
    (is (= "1.2.3.4" (get-cps-description (->cps 2 [1 2 3 4])))))
  (testing "Deals with fractions as generatos"
    (is (= "1/4.2.3.4" (get-cps-description (->cps 2 [1/4 2 3 4])))))
  (testing "Can represent common generators shared across a whole set (helpful for subcps sets)"
    (is (= "9-1.5.7.11" (get-cps-description
                         #{#{7 9 5} #{11 9 5} #{1 11 9}
                           #{7 11 9} #{1 9 5} #{7 1 9}})))
    (is (= "7.13-1.9.11.15" (get-cps-description
                             #{#{7 15 13 11} #{7 15 13 9} #{7 1 13 11}
                               #{7 1 15 13} #{7 1 13 9} #{7 13 11 9}})))
    (is (= "1.3.5.7" (get-cps-description #{#{7 1 3 5}}))))
  (testing "An empty cps returns an empty string"
    (is (= "" (get-cps-description #{#{}})))))

(deftest subcps-sets->map-test
  (testing "Converts a set of subcps sets into a map where keys are the descriptions of the subcps sets"
    (is (= (subcps-sets->map (find-subcps [1 3 5 7] 2 1 3))
           {"5-1.3.7" #{#{7 5} #{3 5} #{1 5}},
            "3-1.5.7" #{#{3 5} #{7 3} #{1 3}},
            "7-1.3.5" #{#{7 1} #{7 5} #{7 3}},
            "1-3.5.7" #{#{7 1} #{1 5} #{1 3}}})))
  (testing "It does not lose any subcps from the list"
    (is (= 30 (count (subcps-sets->map (find-subcps [1 3 5 7 9 11] 3 2 4)))))
    (is (= 30 (count (subcps-sets->map (set/union (find-subcps [1 2 3 4 5 6] 3 1 4)
                                                  (find-subcps [1 2 3 4 5 6] 3 3 4))))))
    (is (= 420 (count (subcps-sets->map (find-subcps [1 3 5 7 9 11 13 15] 4 2 4)))))))

(deftest filter-subcps-map-test
  (testing "Will get a submap that only has sets that include *any* of the given generators"
    (is (= {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
            "1.3.7" #{#{7 1} #{7 3} #{1 3}},
            "1.3.5" #{#{3 5} #{1 5} #{1 3}}}
           (filter-subcps-map {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
                               "1.3.7" #{#{7 1} #{7 3} #{1 3}},
                               "1.3.5" #{#{3 5} #{1 5} #{1 3}},
                               "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
                              #{3}))))
  (is (= {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
          "1.3.7" #{#{7 1} #{7 3} #{1 3}},
          "1.3.5" #{#{3 5} #{1 5} #{1 3}},
          "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
         (filter-subcps-map {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
                             "1.3.7" #{#{7 1} #{7 3} #{1 3}},
                             "1.3.5" #{#{3 5} #{1 5} #{1 3}},
                             "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
                            #{3 5})))
  (testing "Optional param `match-all?`"
    (testing "It defaults to false"
      (is (= {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
              "1.3.7" #{#{7 1} #{7 3} #{1 3}},
              "1.3.5" #{#{3 5} #{1 5} #{1 3}},
              "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
             (filter-subcps-map {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
                                 "1.3.7" #{#{7 1} #{7 3} #{1 3}},
                                 "1.3.5" #{#{3 5} #{1 5} #{1 3}},
                                 "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
                                #{3 5} false))))

    (testing "`match-all?` requires that the selected cps sets include *all* the given generators"
      (is (= {"3.5.7" #{#{7 5} #{3 5} #{7 3}}, "1.3.5" #{#{3 5} #{1 5} #{1 3}}}
             (filter-subcps-map {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
                                 "1.3.7" #{#{7 1} #{7 3} #{1 3}},
                                 "1.3.5" #{#{3 5} #{1 5} #{1 3}},
                                 "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
                                #{3 5} true)))))
  (testing "An empty generators set will return the given subcps-map"
    (is (= {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
            "1.3.7" #{#{7 1} #{7 3} #{1 3}},
            "1.3.5" #{#{3 5} #{1 5} #{1 3}},
            "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
           (filter-subcps-map {"3.5.7" #{#{7 5} #{3 5} #{7 3}},
                               "1.3.7" #{#{7 1} #{7 3} #{1 3}},
                               "1.3.5" #{#{3 5} #{1 5} #{1 3}},
                               "1.5.7" #{#{7 1} #{7 5} #{1 5}}}
                              #{})))))

(deftest cps-intervals-test
  (testing "Calculates all intervals between a set and any other set in a cps"
      (= (cps-intervals (->cps 2 [1 3 5 7]))
         {#{7 1} {#{7 5} 5, #{3 5} 15/7, #{7 3} 3, #{1 5} 5/7, #{1 3} 3/7},
          #{7 5} {#{7 1} 1/5, #{3 5} 3/7, #{7 3} 3/5, #{1 5} 1/7, #{1 3} 3/35},
          #{3 5} {#{7 1} 7/15, #{7 5} 7/3, #{7 3} 7/5, #{1 5} 1/3, #{1 3} 1/5},
          #{7 3} {#{7 1} 1/3, #{7 5} 5/3, #{3 5} 5/7, #{1 5} 5/21, #{1 3} 1/7},
          #{1 5} {#{7 1} 7/5, #{7 5} 7, #{3 5} 3, #{7 3} 21/5, #{1 3} 3/5},
          #{1 3} {#{7 1} 7/3, #{7 5} 35/3, #{3 5} 5, #{7 3} 7, #{1 5} 5/3}}))
  (testing "Calculates all intervals between a set and any other *related* set"
    (= (cps-intervals (->cps 2 [1 3 5 7]) true)
       {#{7 1} {#{7 5} 5, #{7 3} 3, #{1 5} 5/7, #{1 3} 3/7},
        #{7 5} {#{7 1} 1/5, #{3 5} 3/7, #{7 3} 3/5, #{1 5} 1/7},
        #{7 3} {#{7 1} 1/3, #{7 5} 5/3, #{3 5} 5/7, #{1 3} 1/7},
        #{1 5} {#{7 1} 7/5, #{7 5} 7, #{3 5} 3, #{1 3} 3/5},
        #{1 3} {#{7 1} 7/3, #{3 5} 5, #{7 3} 7, #{1 5} 5/3},
        #{3 5} {#{7 5} 7/3, #{7 3} 7/5, #{1 5} 1/3, #{1 3} 1/5}})))

(deftest cps-intervals-as-ratios-test
  (testing "Calculates all intervals between a set and any other set in a cps"
    (= (cps-intervals-as-ratios (->cps 2 [1 3 5 7]))
       {7 {35 5, 15 15/7, 21 3, 5 5/7, 3 3/7},
        35 {7 1/5, 15 3/7, 21 3/5, 5 1/7, 3 3/35},
        15 {7 7/15, 35 7/3, 21 7/5, 5 1/3, 3 1/5},
        21 {7 1/3, 35 5/3, 15 5/7, 5 5/21, 3 1/7},
        5 {7 7/5, 35 7, 15 3, 21 21/5, 3 3/5},
        3 {7 7/3, 35 35/3, 15 5, 21 7, 5 5/3}}))
  (testing "Calculates all intervals between a set and any other *related* set"
    (= (cps-intervals-as-ratios (->cps 2 [1 3 5 7]) true)
       {7 {35 5, 21 3, 5 5/7, 3 3/7},
        35 {7 1/5, 15 3/7, 21 3/5, 5 1/7},
        21 {7 1/3, 35 5/3, 15 5/7, 3 1/7},
        5 {7 7/5, 35 7, 15 3, 3 3/5},
        3 {7 7/3, 15 5, 21 7, 5 5/3},
        15 {35 7/3, 21 7/5, 5 1/3, 3 1/5}})))

(deftest cps-intervals-by-denominator*-test
  (testing "Takes the result of `cps-intervals` and groups each interval (of a set) by its denominator"
    (is (= (cps-intervals-by-denominator*
            (cps-intervals (->cps 2 [1 3 5 7]) true))
           {#{7 1} {1 {#{7 5} 5, #{7 3} 3}, 7 {#{1 5} 5/7, #{1 3} 3/7}},
            #{7 5} {5 {#{7 1} 1/5, #{7 3} 3/5}, 7 {#{3 5} 3/7, #{1 5} 1/7}},
            #{7 3} {3 {#{7 1} 1/3, #{7 5} 5/3}, 7 {#{3 5} 5/7, #{1 3} 1/7}},
            #{1 5} {5 {#{7 1} 7/5, #{1 3} 3/5}, 1 {#{7 5} 7, #{3 5} 3}},
            #{1 3} {3 {#{7 1} 7/3, #{1 5} 5/3}, 1 {#{3 5} 5, #{7 3} 7}},
            #{3 5} {3 {#{7 5} 7/3, #{1 5} 1/3}, 5 {#{7 3} 7/5, #{1 3} 1/5}}}))))

(deftest cps-intervals-by-denominator-test
  (testing "Takes and cps and returns a map of its ratios (as sets) where the values is a map that groups each interval by its denominator"
    (is (= (cps-intervals-by-denominator (->cps 2 [1 3 5 7]))
           {#{7 1} {1 {#{7 5} 5, #{7 3} 3}, 7 {#{3 5} 15/7, #{1 5} 5/7, #{1 3} 3/7}},
            #{7 5}
            {5 {#{7 1} 1/5, #{7 3} 3/5}, 7 {#{3 5} 3/7, #{1 5} 1/7}, 35 {#{1 3} 3/35}},
            #{3 5}
            {15 {#{7 1} 7/15}, 3 {#{7 5} 7/3, #{1 5} 1/3}, 5 {#{7 3} 7/5, #{1 3} 1/5}},
            #{7 3}
            {3 {#{7 1} 1/3, #{7 5} 5/3}, 7 {#{3 5} 5/7, #{1 3} 1/7}, 21 {#{1 5} 5/21}},
            #{1 5} {5 {#{7 1} 7/5, #{7 3} 21/5, #{1 3} 3/5}, 1 {#{7 5} 7, #{3 5} 3}},
            #{1 3} {3 {#{7 1} 7/3, #{7 5} 35/3, #{1 5} 5/3}, 1 {#{3 5} 5, #{7 3} 7}}}))
    (testing "Uses `only-direct?` to only show ratios that are directly related to each other"
      (is (= (cps-intervals-by-denominator (->cps 2 [1 3 5 7]) true)
             {#{7 1} {1 {#{7 5} 5, #{7 3} 3}, 7 {#{1 5} 5/7, #{1 3} 3/7}},
              #{7 5} {5 {#{7 1} 1/5, #{7 3} 3/5}, 7 {#{3 5} 3/7, #{1 5} 1/7}},
              #{7 3} {3 {#{7 1} 1/3, #{7 5} 5/3}, 7 {#{3 5} 5/7, #{1 3} 1/7}},
              #{1 5} {5 {#{7 1} 7/5, #{1 3} 3/5}, 1 {#{7 5} 7, #{3 5} 3}},
              #{1 3} {3 {#{7 1} 7/3, #{1 5} 5/3}, 1 {#{3 5} 5, #{7 3} 7}},
              #{3 5} {3 {#{7 5} 7/3, #{1 5} 1/3}, 5 {#{7 3} 7/5, #{1 3} 1/5}}})))))

(deftest set-chord-test
  (testing "Gets a map of  ratio (as set) and interval"
    (is (= {#{3 5} 5, #{7 3} 7, #{1 3} 1}
           (set-chord
            (cps-intervals-by-denominator* (cps-intervals (->cps 2 [1 3 5 7]) true))
            #{1 3}
            1)))
    (is (= {#{7 1} 7/3, #{1 5} 5/3, #{1 3} 3}
           (set-chord
            (cps-intervals-by-denominator* (cps-intervals (->cps 2 [1 3 5 7]) true))
            #{1 3}
            3))))
  (testing "No `interval` should return all the intervals for the given set "
    (is (= {#{7 1} 7/3, #{1 5} 5/3, #{3 5} 5, #{7 3} 7, #{1 3} 1}
           (set-chord
            (cps-intervals-by-denominator* (cps-intervals (->cps 2 [1 3 5 7]) true))
            #{1 3})))
    (testing "CPS with undirect intervals"
      (is (= {#{7 1} 7/3, #{7 5} 35/3, #{1 5} 5/3, #{3 5} 5, #{7 3} 7, #{1 3} 1}
             (set-chord
              (cps-intervals-by-denominator* (cps-intervals (->cps 2 [1 3 5 7])))
              #{1 3}))))))
