(ns erv.mos.v2.submos-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.mos.v2.submos :as subject]))

(deftest make-all-submos-test
  (is (= '({:pattern [3 4],
            :period 7,
            :submos
            ({:degree 0,
              :mos [5 7],
              :mos-degrees [0 3],
              :rotation {:at-zero? true, :degree 0, :mos [5 7]}}
             {:degree 1,
              :mos [5 7],
              :mos-degrees [1 4],
              :rotation {:at-zero? false, :degree 1, :mos [5 7]}}
             {:degree 2,
              :mos [5 7],
              :mos-degrees [2 5],
              :rotation {:at-zero? false, :degree 2, :mos [5 7]}}
             {:degree 3,
              :mos [5 7],
              :mos-degrees [3 6],
              :rotation {:at-zero? false, :degree 3, :mos [5 7]}}
             {:degree 4,
              :mos [6 6],
              :mos-degrees [4 0],
              :rotation {:at-zero? true, :degree 0, :mos [6 6]}}
             {:degree 5,
              :mos [5 7],
              :mos-degrees [5 1],
              :rotation {:at-zero? false, :degree 1, :mos [7 5]}}
             {:degree 6,
              :mos [5 7],
              :mos-degrees [6 2],
              :rotation {:at-zero? false, :degree 2, :mos [7 5]}}),
            :true-submos? true}
           {:pattern [3 3 1],
            :period 7,
            :submos
            ({:degree 0,
              :mos [5 5 2],
              :mos-degrees [0 3 6],
              :rotation {:at-zero? true, :degree 0, :mos [5 5 2]}}
             {:degree 1,
              :mos [5 6 1],
              :mos-degrees [1 4 0],
              :rotation {:at-zero? true, :degree 0, :mos [1 5 6]}}
             {:degree 2,
              :mos [5 5 2],
              :mos-degrees [2 5 1],
              :rotation {:at-zero? false, :degree 1, :mos [2 5 5]}}
             {:degree 3,
              :mos [5 5 2],
              :mos-degrees [3 6 2],
              :rotation {:at-zero? false, :degree 2, :mos [2 5 5]}}
             {:degree 4,
              :mos [6 5 1],
              :mos-degrees [4 0 3],
              :rotation {:at-zero? true, :degree 0, :mos [5 1 6]}}
             {:degree 5,
              :mos [5 5 2],
              :mos-degrees [5 1 4],
              :rotation {:at-zero? false, :degree 1, :mos [5 2 5]}}
             {:degree 6,
              :mos [5 5 2],
              :mos-degrees [6 2 5],
              :rotation {:at-zero? false, :degree 2, :mos [5 2 5]}}),
            :true-submos? true}
           {:pattern [2 1 2 1 1],
            :period 7,
            :submos
            ({:degree 0,
              :mos [3 2 3 2 2],
              :mos-degrees [0 2 3 5 6],
              :rotation {:at-zero? true, :degree 0, :mos [3 2 3 2 2]}}
             {:degree 1,
              :mos [4 1 4 2 1],
              :mos-degrees [1 3 4 6 0],
              :rotation {:at-zero? true, :degree 0, :mos [1 4 1 4 2]}}
             {:degree 2,
              :mos [3 2 4 1 2],
              :mos-degrees [2 4 5 0 1],
              :rotation {:at-zero? true, :degree 0, :mos [1 2 3 2 4]}}
             {:degree 3,
              :mos [3 2 3 2 2],
              :mos-degrees [3 5 6 1 2],
              :rotation {:at-zero? false, :degree 1, :mos [2 2 3 2 3]}}
             {:degree 4,
              :mos [4 2 3 2 1],
              :mos-degrees [4 6 0 2 3],
              :rotation {:at-zero? true, :degree 0, :mos [3 2 1 4 2]}}
             {:degree 5,
              :mos [4 1 4 1 2],
              :mos-degrees [5 0 1 3 4],
              :rotation {:at-zero? true, :degree 0, :mos [1 4 1 2 4]}}
             {:degree 6,
              :mos [3 2 3 2 2],
              :mos-degrees [6 1 2 4 5],
              :rotation {:at-zero? false, :degree 1, :mos [2 3 2 2 3]}}),
            :true-submos? true})
         (subject/make-all-submos 5 [1 2 2 1 2 2 2]))))
