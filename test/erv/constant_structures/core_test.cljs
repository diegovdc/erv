(ns erv.constant-structures.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.constant-structures.core :refer [analyze]]
   [erv.cps.core :as cps]
   [erv.utils.exact :as exact.utils]))

(deftest analyze-test
  (is (= '{:interval-data
           (["143/140"
             {:steps #{1}, :intervals ({:steps 1, :interval ("35/32" "143/128")})}]
            ["14/13"
             {:steps #{1},
              :intervals
              ({:steps 1, :interval ("65/64" "35/32")}
               {:steps 1, :interval ("143/128" "77/64")})}]
            ["11/10"
             {:steps #{2},
              :intervals
              ({:steps 2, :interval ("65/64" "143/128")}
               {:steps 2, :interval ("35/32" "77/64")})}]
            ["13/11"
             {:steps #{1},
              :intervals
              ({:steps -5, :interval ("55/32" "65/64")}
               {:steps 1, :interval ("77/64" "91/64")})}]
            ["77/65" {:steps #{3}, :intervals ({:steps 3, :interval ("65/64" "77/64")})}]
            ["110/91"
             {:steps #{1}, :intervals ({:steps 1, :interval ("91/64" "55/32")})}]
            ["14/11"
             {:steps #{2},
              :intervals
              ({:steps -4, :interval ("55/32" "35/32")}
               {:steps 2, :interval ("143/128" "91/64")})}]
            ["13/10"
             {:steps #{3},
              :intervals
              ({:steps 3, :interval ("35/32" "91/64")}
               {:steps -3, :interval ("55/32" "143/128")})}]
            ["7/5"
             {:steps #{4},
              :intervals
              ({:steps 4, :interval ("65/64" "91/64")}
               {:steps -2, :interval ("55/32" "77/64")})}]
            ["10/7"
             {:steps #{2},
              :intervals
              ({:steps -4, :interval ("91/64" "65/64")}
               {:steps 2, :interval ("77/64" "55/32")})}]
            ["20/13"
             {:steps #{3},
              :intervals
              ({:steps -3, :interval ("91/64" "35/32")}
               {:steps 3, :interval ("143/128" "55/32")})}]
            ["11/7"
             {:steps #{4},
              :intervals
              ({:steps 4, :interval ("35/32" "55/32")}
               {:steps -2, :interval ("91/64" "143/128")})}]
            ["91/55"
             {:steps #{5}, :intervals ({:steps -1, :interval ("55/32" "91/64")})}]
            ["130/77"
             {:steps #{3}, :intervals ({:steps -3, :interval ("77/64" "65/64")})}]
            ["22/13"
             {:steps #{5},
              :intervals
              ({:steps 5, :interval ("65/64" "55/32")}
               {:steps -1, :interval ("91/64" "77/64")})}]
            ["20/11"
             {:steps #{4},
              :intervals
              ({:steps -2, :interval ("143/128" "65/64")}
               {:steps -2, :interval ("77/64" "35/32")})}]
            ["13/7"
             {:steps #{5},
              :intervals
              ({:steps -1, :interval ("35/32" "65/64")}
               {:steps -1, :interval ("77/64" "143/128")})}]
            ["280/143"
             {:steps #{5}, :intervals ({:steps -1, :interval ("143/128" "35/32")})}]),
           :non-cs-intervals {:total 0, :intervals ()},
           :constant-structure? true}
         (->> (analyze
               (:scale (cps/make 2 [11 13 5 7])))
              (exact.utils/make-readable)))))
