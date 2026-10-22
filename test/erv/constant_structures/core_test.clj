(ns erv.constant-structures.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.constant-structures.core :refer [analyze]]
   [erv.cps.core :as cps]))

(deftest analyze-test
  (is (= '{:constant-structure? true,
           :interval-data
           ([143/140
             {:intervals ({:interval (35/32 143/128), :steps 1}), :steps #{1}}]
            [14/13
             {:intervals
              ({:interval (65/64 35/32), :steps 1}
               {:interval (143/128 77/64), :steps 1}),
              :steps #{1}}]
            [11/10
             {:intervals
              ({:interval (65/64 143/128), :steps 2}
               {:interval (35/32 77/64), :steps 2}),
              :steps #{2}}]
            [13/11
             {:intervals
              ({:interval (55/32 65/64), :steps -5}
               {:interval (77/64 91/64), :steps 1}),
              :steps #{1}}]
            [77/65
             {:intervals ({:interval (65/64 77/64), :steps 3}), :steps #{3}}]
            [110/91
             {:intervals ({:interval (91/64 55/32), :steps 1}), :steps #{1}}]
            [14/11
             {:intervals
              ({:interval (55/32 35/32), :steps -4}
               {:interval (143/128 91/64), :steps 2}),
              :steps #{2}}]
            [13/10
             {:intervals
              ({:interval (35/32 91/64), :steps 3}
               {:interval (55/32 143/128), :steps -3}),
              :steps #{3}}]
            [7/5
             {:intervals
              ({:interval (65/64 91/64), :steps 4}
               {:interval (55/32 77/64), :steps -2}),
              :steps #{4}}]
            [10/7
             {:intervals
              ({:interval (91/64 65/64), :steps -4}
               {:interval (77/64 55/32), :steps 2}),
              :steps #{2}}]
            [20/13
             {:intervals
              ({:interval (91/64 35/32), :steps -3}
               {:interval (143/128 55/32), :steps 3}),
              :steps #{3}}]
            [11/7
             {:intervals
              ({:interval (35/32 55/32), :steps 4}
               {:interval (91/64 143/128), :steps -2}),
              :steps #{4}}]
            [91/55
             {:intervals ({:interval (55/32 91/64), :steps -1}), :steps #{5}}]
            [130/77
             {:intervals ({:interval (77/64 65/64), :steps -3}), :steps #{3}}]
            [22/13
             {:intervals
              ({:interval (65/64 55/32), :steps 5}
               {:interval (91/64 77/64), :steps -1}),
              :steps #{5}}]
            [20/11
             {:intervals
              ({:interval (143/128 65/64), :steps -2}
               {:interval (77/64 35/32), :steps -2}),
              :steps #{4}}]
            [13/7
             {:intervals
              ({:interval (35/32 65/64), :steps -1}
               {:interval (77/64 143/128), :steps -1}),
              :steps #{5}}]
            [280/143
             {:intervals ({:interval (143/128 35/32), :steps -1}),
              :steps #{5}}]),
           :non-cs-intervals {:intervals (), :total 0}}
         (analyze (:scale (cps/make 2 [11 13 5 7]))))))
