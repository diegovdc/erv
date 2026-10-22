(ns erv.mos.v2.submos-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.mos.v2.submos :as subject]))

(deftest make-all-submos-test
  (is (= '({:pattern [3 4],
            :period 7,
            :true-submos? true,
            :submos
            ({:mos [5 7],
              :degree 0,
              :mos-degrees [0 3],
              :chromatic-degrees [0 5],
              :rotation {:mos [5 7], :degree 0, :at-zero? true}}
             {:mos [5 7],
              :degree 1,
              :mos-degrees [1 4],
              :chromatic-degrees [1 6],
              :rotation {:mos [5 7], :degree 1, :at-zero? false}}
             {:mos [5 7],
              :degree 2,
              :mos-degrees [2 5],
              :chromatic-degrees [3 8],
              :rotation {:mos [5 7], :degree 2, :at-zero? false}}
             {:mos [5 7],
              :degree 3,
              :mos-degrees [3 6],
              :chromatic-degrees [5 10],
              :rotation {:mos [5 7], :degree 3, :at-zero? false}}
             {:mos [6 6],
              :degree 4,
              :mos-degrees [4 0],
              :chromatic-degrees [6 0],
              :rotation {:mos [6 6], :degree 0, :at-zero? true}}
             {:mos [5 7],
              :degree 5,
              :mos-degrees [5 1],
              :chromatic-degrees [8 1],
              :rotation {:mos [7 5], :degree 1, :at-zero? false}}
             {:mos [5 7],
              :degree 6,
              :mos-degrees [6 2],
              :chromatic-degrees [10 3],
              :rotation {:mos [7 5], :degree 2, :at-zero? false}})}
           {:pattern [3 3 1],
            :period 7,
            :true-submos? true,
            :submos
            ({:mos [5 5 2],
              :degree 0,
              :mos-degrees [0 3 6],
              :chromatic-degrees [0 5 10],
              :rotation {:mos [5 5 2], :degree 0, :at-zero? true}}
             {:mos [5 6 1],
              :degree 1,
              :mos-degrees [1 4 0],
              :chromatic-degrees [1 6 0],
              :rotation {:mos [1 5 6], :degree 0, :at-zero? true}}
             {:mos [5 5 2],
              :degree 2,
              :mos-degrees [2 5 1],
              :chromatic-degrees [3 8 1],
              :rotation {:mos [2 5 5], :degree 1, :at-zero? false}}
             {:mos [5 5 2],
              :degree 3,
              :mos-degrees [3 6 2],
              :chromatic-degrees [5 10 3],
              :rotation {:mos [2 5 5], :degree 2, :at-zero? false}}
             {:mos [6 5 1],
              :degree 4,
              :mos-degrees [4 0 3],
              :chromatic-degrees [6 0 5],
              :rotation {:mos [5 1 6], :degree 0, :at-zero? true}}
             {:mos [5 5 2],
              :degree 5,
              :mos-degrees [5 1 4],
              :chromatic-degrees [8 1 6],
              :rotation {:mos [5 2 5], :degree 1, :at-zero? false}}
             {:mos [5 5 2],
              :degree 6,
              :mos-degrees [6 2 5],
              :chromatic-degrees [10 3 8],
              :rotation {:mos [5 2 5], :degree 2, :at-zero? false}})}
           {:pattern [2 1 2 1 1],
            :period 7,
            :true-submos? true,
            :submos
            ({:mos [3 2 3 2 2],
              :degree 0,
              :mos-degrees [0 2 3 5 6],
              :chromatic-degrees [0 3 5 8 10],
              :rotation {:mos [3 2 3 2 2], :degree 0, :at-zero? true}}
             {:mos [4 1 4 2 1],
              :degree 1,
              :mos-degrees [1 3 4 6 0],
              :chromatic-degrees [1 5 6 10 0],
              :rotation {:mos [1 4 1 4 2], :degree 0, :at-zero? true}}
             {:mos [3 2 4 1 2],
              :degree 2,
              :mos-degrees [2 4 5 0 1],
              :chromatic-degrees [3 6 8 0 1],
              :rotation {:mos [1 2 3 2 4], :degree 0, :at-zero? true}}
             {:mos [3 2 3 2 2],
              :degree 3,
              :mos-degrees [3 5 6 1 2],
              :chromatic-degrees [5 8 10 1 3],
              :rotation {:mos [2 2 3 2 3], :degree 1, :at-zero? false}}
             {:mos [4 2 3 2 1],
              :degree 4,
              :mos-degrees [4 6 0 2 3],
              :chromatic-degrees [6 10 0 3 5],
              :rotation {:mos [3 2 1 4 2], :degree 0, :at-zero? true}}
             {:mos [4 1 4 1 2],
              :degree 5,
              :mos-degrees [5 0 1 3 4],
              :chromatic-degrees [8 0 1 5 6],
              :rotation {:mos [1 4 1 2 4], :degree 0, :at-zero? true}}
             {:mos [3 2 3 2 2],
              :degree 6,
              :mos-degrees [6 1 2 4 5],
              :chromatic-degrees [10 1 3 6 8],
              :rotation {:mos [2 3 2 2 3], :degree 1, :at-zero? false}})})
         (subject/make-all-submos 5 [1 2 2 1 2 2 2]))))
