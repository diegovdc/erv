(ns erv.cps.core-test
  (:require
   [cljs.test :refer [deftest is]]
   [erv.cps.core :as cps]
   [erv.utils.exact :as exact.utils]))

(deftest make-test
  (is (= '{:meta
           {:scale :cps,
            :period 2,
            :size 3,
            :cps/size 2,
            :cps/factors (1 3 5),
            :cps/normalized-by 1,
            :cps/type "2)3"},
           :scale
           ({:set #{1 5},
             :archi-set #{:c :a},
             :ratio "5",
             :bounded-ratio "5/4",
             :bounding-period 2,
             :degree 0}
            {:set #{1 3},
             :archi-set #{:b :a},
             :ratio "3",
             :bounded-ratio "3/2",
             :bounding-period 2,
             :degree 1}
            {:set #{3 5},
             :archi-set #{:c :b},
             :ratio "15",
             :bounded-ratio "15/8",
             :bounding-period 2,
             :degree 2}),
           :nodes
           ({:set #{1 3},
             :archi-set #{:b :a},
             :ratio "3",
             :bounded-ratio "3/2",
             :bounding-period 2}
            {:set #{1 5},
             :archi-set #{:c :a},
             :ratio "5",
             :bounded-ratio "5/4",
             :bounding-period 2}
            {:set #{3 5},
             :archi-set #{:c :b},
             :ratio "15",
             :bounded-ratio "15/8",
             :bounding-period 2}),
           :graphs
           {:full
            {{:set #{1 3},
              :archi-set #{:b :a},
              :ratio "3",
              :bounded-ratio "3/2",
              :bounding-period 2}
             #{{:set #{3 5},
                :archi-set #{:c :b},
                :ratio "15",
                :bounded-ratio "15/8",
                :bounding-period 2}
               {:set #{1 5},
                :archi-set #{:c :a},
                :ratio "5",
                :bounded-ratio "5/4",
                :bounding-period 2}},
             {:set #{1 5},
              :archi-set #{:c :a},
              :ratio "5",
              :bounded-ratio "5/4",
              :bounding-period 2}
             #{{:set #{3 5},
                :archi-set #{:c :b},
                :ratio "15",
                :bounded-ratio "15/8",
                :bounding-period 2}
               {:set #{1 3},
                :archi-set #{:b :a},
                :ratio "3",
                :bounded-ratio "3/2",
                :bounding-period 2}},
             {:set #{3 5},
              :archi-set #{:c :b},
              :ratio "15",
              :bounded-ratio "15/8",
              :bounding-period 2}
             #{{:set #{1 3},
                :archi-set #{:b :a},
                :ratio "3",
                :bounded-ratio "3/2",
                :bounding-period 2}
               {:set #{1 5},
                :archi-set #{:c :a},
                :ratio "5",
                :bounded-ratio "5/4",
                :bounding-period 2}}},
            :simple
            {#{1 3} #{#{3 5} #{1 5}}, #{1 5} #{#{3 5} #{1 3}}, #{3 5} #{#{1 5} #{1 3}}}},
           :subcps
           {"1)1 of 2)3 3.5"
            {:meta
             {:scale :cps,
              :period 2,
              :size 1,
              :cps/size 2,
              :cps/factors (3 5),
              :cps/normalized-by 1,
              :cps/type "1)1 of 2)3"},
             :scale
             ({:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :nodes
             ({:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :graphs {:full {}, :simple {}}},
            "1)1 of 2)3 1.5"
            {:meta
             {:scale :cps,
              :period 2,
              :size 1,
              :cps/size 2,
              :cps/factors (1 5),
              :cps/normalized-by 1,
              :cps/type "1)1 of 2)3"},
             :scale
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}),
             :nodes
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}),
             :graphs {:full {}, :simple {}}},
            "2)2 of 2)3 1.3"
            {:meta
             {:scale :cps,
              :period 2,
              :size 1,
              :cps/size 2,
              :cps/factors (1 3),
              :cps/normalized-by 1,
              :cps/type "2)2 of 2)3"},
             :scale
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}),
             :nodes
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}),
             :graphs {:full {}, :simple {}}},
            "1)2 of 2)3 5-1.3"
            {:meta
             {:scale :cps,
              :period 2,
              :size 2,
              :cps/size 2,
              :cps/factors (1 3 5),
              :cps/normalized-by 1,
              :cps/type "1)2 of 2)3"},
             :scale
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}
              {:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :nodes
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}
              {:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :graphs
             {:full
              {{:set #{1 5},
                :archi-set #{nil},
                :ratio "5",
                :bounded-ratio "5/4",
                :bounding-period 2}
               #{{:set #{3 5},
                  :archi-set #{nil},
                  :ratio "15",
                  :bounded-ratio "15/8",
                  :bounding-period 2}},
               {:set #{3 5},
                :archi-set #{nil},
                :ratio "15",
                :bounded-ratio "15/8",
                :bounding-period 2}
               #{{:set #{1 5},
                  :archi-set #{nil},
                  :ratio "5",
                  :bounded-ratio "5/4",
                  :bounding-period 2}}},
              :simple {#{1 5} #{#{3 5}}, #{3 5} #{#{1 5}}}}},
            "1)2 of 2)3 3-1.5"
            {:meta
             {:scale :cps,
              :period 2,
              :size 2,
              :cps/size 2,
              :cps/factors (1 3 5),
              :cps/normalized-by 1,
              :cps/type "1)2 of 2)3"},
             :scale
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}
              {:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :nodes
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}
              {:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :graphs
             {:full
              {{:set #{1 3},
                :archi-set #{nil},
                :ratio "3",
                :bounded-ratio "3/2",
                :bounding-period 2}
               #{{:set #{3 5},
                  :archi-set #{nil},
                  :ratio "15",
                  :bounded-ratio "15/8",
                  :bounding-period 2}},
               {:set #{3 5},
                :archi-set #{nil},
                :ratio "15",
                :bounded-ratio "15/8",
                :bounding-period 2}
               #{{:set #{1 3},
                  :archi-set #{nil},
                  :ratio "3",
                  :bounded-ratio "3/2",
                  :bounding-period 2}}},
              :simple {#{1 3} #{#{3 5}}, #{3 5} #{#{1 3}}}}},
            "2)2 of 2)3 1.5"
            {:meta
             {:scale :cps,
              :period 2,
              :size 1,
              :cps/size 2,
              :cps/factors (1 5),
              :cps/normalized-by 1,
              :cps/type "2)2 of 2)3"},
             :scale
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}),
             :nodes
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}),
             :graphs {:full {}, :simple {}}},
            "1)2 of 2)3 1-3.5"
            {:meta
             {:scale :cps,
              :period 2,
              :size 2,
              :cps/size 2,
              :cps/factors (1 3 5),
              :cps/normalized-by 1,
              :cps/type "1)2 of 2)3"},
             :scale
             ({:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}
              {:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}),
             :nodes
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}
              {:set #{1 5},
               :archi-set #{nil},
               :ratio "5",
               :bounded-ratio "5/4",
               :bounding-period 2}),
             :graphs
             {:full
              {{:set #{1 3},
                :archi-set #{nil},
                :ratio "3",
                :bounded-ratio "3/2",
                :bounding-period 2}
               #{{:set #{1 5},
                  :archi-set #{nil},
                  :ratio "5",
                  :bounded-ratio "5/4",
                  :bounding-period 2}},
               {:set #{1 5},
                :archi-set #{nil},
                :ratio "5",
                :bounded-ratio "5/4",
                :bounding-period 2}
               #{{:set #{1 3},
                  :archi-set #{nil},
                  :ratio "3",
                  :bounded-ratio "3/2",
                  :bounding-period 2}}},
              :simple {#{1 3} #{#{1 5}}, #{1 5} #{#{1 3}}}}},
            "2)2 of 2)3 3.5"
            {:meta
             {:scale :cps,
              :period 2,
              :size 1,
              :cps/size 2,
              :cps/factors (3 5),
              :cps/normalized-by 1,
              :cps/type "2)2 of 2)3"},
             :scale
             ({:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :nodes
             ({:set #{3 5},
               :archi-set #{nil},
               :ratio "15",
               :bounded-ratio "15/8",
               :bounding-period 2}),
             :graphs {:full {}, :simple {}}},
            "1)1 of 2)3 1.3"
            {:meta
             {:scale :cps,
              :period 2,
              :size 1,
              :cps/size 2,
              :cps/factors (1 3),
              :cps/normalized-by 1,
              :cps/type "1)1 of 2)3"},
             :scale
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}),
             :nodes
             ({:set #{1 3},
               :archi-set #{nil},
               :ratio "3",
               :bounded-ratio "3/2",
               :bounding-period 2}),
             :graphs {:full {}, :simple {}}}}}
         (->> (cps/make 2 [1 3 5])
              (cps/+all-subcps)
              exact.utils/make-readable))))

(->> (cps/make 2 [1 3 5])
     (cps/+all-subcps)
     exact.utils/make-readable)
