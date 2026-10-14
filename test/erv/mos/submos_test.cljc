(ns erv.mos.submos-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.mos.submos :as subject]))

(deftest make-all-submos-test
  (let [all-submos '({:generator 1,
                      :pattern [1 6],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [1 11],
                        :mos-degrees [0 1],
                        :rotation {:at-zero? true, :degree 0, :mos [1 11]}}
                       {:degree 1,
                        :mos [2 10],
                        :mos-degrees [1 2],
                        :rotation {:at-zero? false, :degree 1, :mos [2 10]}}
                       {:degree 2,
                        :mos [2 10],
                        :mos-degrees [2 3],
                        :rotation {:at-zero? false, :degree 2, :mos [2 10]}}
                       {:degree 3,
                        :mos [1 11],
                        :mos-degrees [3 4],
                        :rotation {:at-zero? false, :degree 3, :mos [1 11]}}
                       {:degree 4,
                        :mos [2 10],
                        :mos-degrees [4 5],
                        :rotation {:at-zero? false, :degree 4, :mos [2 10]}}
                       {:degree 5,
                        :mos [2 10],
                        :mos-degrees [5 6],
                        :rotation {:at-zero? false, :degree 5, :mos [2 10]}}
                       {:degree 6,
                        :mos [2 10],
                        :mos-degrees [6 0],
                        :rotation {:at-zero? true, :degree 0, :mos [10 2]}}),
                      :submos-by-mos
                      {[1 11]
                       [{:degree 0,
                         :mos [1 11],
                         :mos-degrees [0 1],
                         :rotation {:at-zero? true, :degree 0, :mos [1 11]}}
                        {:degree 3,
                         :mos [1 11],
                         :mos-degrees [3 4],
                         :rotation {:at-zero? false, :degree 3, :mos [1 11]}}],
                       [2 10]
                       [{:degree 1,
                         :mos [2 10],
                         :mos-degrees [1 2],
                         :rotation {:at-zero? false, :degree 1, :mos [2 10]}}
                        {:degree 2,
                         :mos [2 10],
                         :mos-degrees [2 3],
                         :rotation {:at-zero? false, :degree 2, :mos [2 10]}}
                        {:degree 4,
                         :mos [2 10],
                         :mos-degrees [4 5],
                         :rotation {:at-zero? false, :degree 4, :mos [2 10]}}
                        {:degree 5,
                         :mos [2 10],
                         :mos-degrees [5 6],
                         :rotation {:at-zero? false, :degree 5, :mos [2 10]}}
                        {:degree 6,
                         :mos [2 10],
                         :mos-degrees [6 0],
                         :rotation {:at-zero? true, :degree 0, :mos [10 2]}}]},
                      :true-submos? false}
                     {:generator 1,
                      :pattern [1 1 5],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [1 2 9],
                        :mos-degrees [0 1 2],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 9]}}
                       {:degree 1,
                        :mos [2 2 8],
                        :mos-degrees [1 2 3],
                        :rotation {:at-zero? false, :degree 1, :mos [2 2 8]}}
                       {:degree 2,
                        :mos [2 1 9],
                        :mos-degrees [2 3 4],
                        :rotation {:at-zero? false, :degree 2, :mos [2 1 9]}}
                       {:degree 3,
                        :mos [1 2 9],
                        :mos-degrees [3 4 5],
                        :rotation {:at-zero? false, :degree 3, :mos [1 2 9]}}
                       {:degree 4,
                        :mos [2 2 8],
                        :mos-degrees [4 5 6],
                        :rotation {:at-zero? false, :degree 4, :mos [2 2 8]}}
                       {:degree 5,
                        :mos [2 2 8],
                        :mos-degrees [5 6 0],
                        :rotation {:at-zero? true, :degree 0, :mos [8 2 2]}}
                       {:degree 6,
                        :mos [2 1 9],
                        :mos-degrees [6 0 1],
                        :rotation {:at-zero? true, :degree 0, :mos [1 9 2]}}),
                      :submos-by-mos
                      {[1 2 9]
                       [{:degree 0,
                         :mos [1 2 9],
                         :mos-degrees [0 1 2],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 9]}}
                        {:degree 3,
                         :mos [1 2 9],
                         :mos-degrees [3 4 5],
                         :rotation {:at-zero? false, :degree 3, :mos [1 2 9]}}],
                       [2 1 9]
                       [{:degree 2,
                         :mos [2 1 9],
                         :mos-degrees [2 3 4],
                         :rotation {:at-zero? false, :degree 2, :mos [2 1 9]}}
                        {:degree 6,
                         :mos [2 1 9],
                         :mos-degrees [6 0 1],
                         :rotation {:at-zero? true, :degree 0, :mos [1 9 2]}}],
                       [2 2 8]
                       [{:degree 1,
                         :mos [2 2 8],
                         :mos-degrees [1 2 3],
                         :rotation {:at-zero? false, :degree 1, :mos [2 2 8]}}
                        {:degree 4,
                         :mos [2 2 8],
                         :mos-degrees [4 5 6],
                         :rotation {:at-zero? false, :degree 4, :mos [2 2 8]}}
                        {:degree 5,
                         :mos [2 2 8],
                         :mos-degrees [5 6 0],
                         :rotation {:at-zero? true, :degree 0, :mos [8 2 2]}}]},
                      :true-submos? false}
                     {:generator 1,
                      :pattern [1 1 1 4],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [1 2 2 7],
                        :mos-degrees [0 1 2 3],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 2 7]}}
                       {:degree 1,
                        :mos [2 2 1 7],
                        :mos-degrees [1 2 3 4],
                        :rotation {:at-zero? false, :degree 1, :mos [2 2 1 7]}}
                       {:degree 2,
                        :mos [2 1 2 7],
                        :mos-degrees [2 3 4 5],
                        :rotation {:at-zero? false, :degree 2, :mos [2 1 2 7]}}
                       {:degree 3,
                        :mos [1 2 2 7],
                        :mos-degrees [3 4 5 6],
                        :rotation {:at-zero? false, :degree 3, :mos [1 2 2 7]}}
                       {:degree 4,
                        :mos [2 2 2 6],
                        :mos-degrees [4 5 6 0],
                        :rotation {:at-zero? true, :degree 0, :mos [6 2 2 2]}}
                       {:degree 5,
                        :mos [2 2 1 7],
                        :mos-degrees [5 6 0 1],
                        :rotation {:at-zero? true, :degree 0, :mos [1 7 2 2]}}
                       {:degree 6,
                        :mos [2 1 2 7],
                        :mos-degrees [6 0 1 2],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 7 2]}}),
                      :submos-by-mos
                      {[1 2 2 7]
                       [{:degree 0,
                         :mos [1 2 2 7],
                         :mos-degrees [0 1 2 3],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 2 7]}}
                        {:degree 3,
                         :mos [1 2 2 7],
                         :mos-degrees [3 4 5 6],
                         :rotation {:at-zero? false, :degree 3, :mos [1 2 2 7]}}],
                       [2 1 2 7]
                       [{:degree 2,
                         :mos [2 1 2 7],
                         :mos-degrees [2 3 4 5],
                         :rotation {:at-zero? false, :degree 2, :mos [2 1 2 7]}}
                        {:degree 6,
                         :mos [2 1 2 7],
                         :mos-degrees [6 0 1 2],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 7 2]}}],
                       [2 2 1 7]
                       [{:degree 1,
                         :mos [2 2 1 7],
                         :mos-degrees [1 2 3 4],
                         :rotation {:at-zero? false, :degree 1, :mos [2 2 1 7]}}
                        {:degree 5,
                         :mos [2 2 1 7],
                         :mos-degrees [5 6 0 1],
                         :rotation {:at-zero? true, :degree 0, :mos [1 7 2 2]}}],
                       [2 2 2 6]
                       [{:degree 4,
                         :mos [2 2 2 6],
                         :mos-degrees [4 5 6 0],
                         :rotation {:at-zero? true, :degree 0, :mos [6 2 2 2]}}]},
                      :true-submos? false}
                     {:generator 1,
                      :pattern [1 1 1 1 3],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [1 2 2 1 6],
                        :mos-degrees [0 1 2 3 4],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 2 1 6]}}
                       {:degree 1,
                        :mos [2 2 1 2 5],
                        :mos-degrees [1 2 3 4 5],
                        :rotation {:at-zero? false, :degree 1, :mos [2 2 1 2 5]}}
                       {:degree 2,
                        :mos [2 1 2 2 5],
                        :mos-degrees [2 3 4 5 6],
                        :rotation {:at-zero? false, :degree 2, :mos [2 1 2 2 5]}}
                       {:degree 3,
                        :mos [1 2 2 2 5],
                        :mos-degrees [3 4 5 6 0],
                        :rotation {:at-zero? true, :degree 0, :mos [5 1 2 2 2]}}
                       {:degree 4,
                        :mos [2 2 2 1 5],
                        :mos-degrees [4 5 6 0 1],
                        :rotation {:at-zero? true, :degree 0, :mos [1 5 2 2 2]}}
                       {:degree 5,
                        :mos [2 2 1 2 5],
                        :mos-degrees [5 6 0 1 2],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 5 2 2]}}
                       {:degree 6,
                        :mos [2 1 2 2 5],
                        :mos-degrees [6 0 1 2 3],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 2 5 2]}}),
                      :submos-by-mos
                      {[1 2 2 1 6]
                       [{:degree 0,
                         :mos [1 2 2 1 6],
                         :mos-degrees [0 1 2 3 4],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 2 1 6]}}],
                       [1 2 2 2 5]
                       [{:degree 3,
                         :mos [1 2 2 2 5],
                         :mos-degrees [3 4 5 6 0],
                         :rotation {:at-zero? true, :degree 0, :mos [5 1 2 2 2]}}],
                       [2 1 2 2 5]
                       [{:degree 2,
                         :mos [2 1 2 2 5],
                         :mos-degrees [2 3 4 5 6],
                         :rotation {:at-zero? false, :degree 2, :mos [2 1 2 2 5]}}
                        {:degree 6,
                         :mos [2 1 2 2 5],
                         :mos-degrees [6 0 1 2 3],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 2 5 2]}}],
                       [2 2 1 2 5]
                       [{:degree 1,
                         :mos [2 2 1 2 5],
                         :mos-degrees [1 2 3 4 5],
                         :rotation {:at-zero? false, :degree 1, :mos [2 2 1 2 5]}}
                        {:degree 5,
                         :mos [2 2 1 2 5],
                         :mos-degrees [5 6 0 1 2],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 5 2 2]}}],
                       [2 2 2 1 5]
                       [{:degree 4,
                         :mos [2 2 2 1 5],
                         :mos-degrees [4 5 6 0 1],
                         :rotation {:at-zero? true, :degree 0, :mos [1 5 2 2 2]}}]},
                      :true-submos? false}
                     {:generator 1,
                      :pattern [1 1 1 1 1 2],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [1 2 2 1 2 4],
                        :mos-degrees [0 1 2 3 4 5],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 2 1 2 4]}}
                       {:degree 1,
                        :mos [2 2 1 2 2 3],
                        :mos-degrees [1 2 3 4 5 6],
                        :rotation {:at-zero? false, :degree 1, :mos [2 2 1 2 2 3]}}
                       {:degree 2,
                        :mos [2 1 2 2 2 3],
                        :mos-degrees [2 3 4 5 6 0],
                        :rotation {:at-zero? true, :degree 0, :mos [3 2 1 2 2 2]}}
                       {:degree 3,
                        :mos [1 2 2 2 1 4],
                        :mos-degrees [3 4 5 6 0 1],
                        :rotation {:at-zero? true, :degree 0, :mos [1 4 1 2 2 2]}}
                       {:degree 4,
                        :mos [2 2 2 1 2 3],
                        :mos-degrees [4 5 6 0 1 2],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 3 2 2 2]}}
                       {:degree 5,
                        :mos [2 2 1 2 2 3],
                        :mos-degrees [5 6 0 1 2 3],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 2 3 2 2]}}
                       {:degree 6,
                        :mos [2 1 2 2 1 4],
                        :mos-degrees [6 0 1 2 3 4],
                        :rotation {:at-zero? true, :degree 0, :mos [1 2 2 1 4 2]}}),
                      :submos-by-mos
                      {[1 2 2 1 2 4]
                       [{:degree 0,
                         :mos [1 2 2 1 2 4],
                         :mos-degrees [0 1 2 3 4 5],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 2 1 2 4]}}],
                       [1 2 2 2 1 4]
                       [{:degree 3,
                         :mos [1 2 2 2 1 4],
                         :mos-degrees [3 4 5 6 0 1],
                         :rotation {:at-zero? true, :degree 0, :mos [1 4 1 2 2 2]}}],
                       [2 1 2 2 1 4]
                       [{:degree 6,
                         :mos [2 1 2 2 1 4],
                         :mos-degrees [6 0 1 2 3 4],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 2 1 4 2]}}],
                       [2 1 2 2 2 3]
                       [{:degree 2,
                         :mos [2 1 2 2 2 3],
                         :mos-degrees [2 3 4 5 6 0],
                         :rotation {:at-zero? true, :degree 0, :mos [3 2 1 2 2 2]}}],
                       [2 2 1 2 2 3]
                       [{:degree 1,
                         :mos [2 2 1 2 2 3],
                         :mos-degrees [1 2 3 4 5 6],
                         :rotation {:at-zero? false, :degree 1, :mos [2 2 1 2 2 3]}}
                        {:degree 5,
                         :mos [2 2 1 2 2 3],
                         :mos-degrees [5 6 0 1 2 3],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 2 3 2 2]}}],
                       [2 2 2 1 2 3]
                       [{:degree 4,
                         :mos [2 2 2 1 2 3],
                         :mos-degrees [4 5 6 0 1 2],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 3 2 2 2]}}]},
                      :true-submos? false}
                     {:generator 2,
                      :pattern [2 5],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [3 9],
                        :mos-degrees [0 2],
                        :rotation {:at-zero? true, :degree 0, :mos [3 9]}}
                       {:degree 1,
                        :mos [4 8],
                        :mos-degrees [1 3],
                        :rotation {:at-zero? false, :degree 1, :mos [4 8]}}
                       {:degree 2,
                        :mos [3 9],
                        :mos-degrees [2 4],
                        :rotation {:at-zero? false, :degree 2, :mos [3 9]}}
                       {:degree 3,
                        :mos [3 9],
                        :mos-degrees [3 5],
                        :rotation {:at-zero? false, :degree 3, :mos [3 9]}}
                       {:degree 4,
                        :mos [4 8],
                        :mos-degrees [4 6],
                        :rotation {:at-zero? false, :degree 4, :mos [4 8]}}
                       {:degree 5,
                        :mos [4 8],
                        :mos-degrees [5 0],
                        :rotation {:at-zero? true, :degree 0, :mos [8 4]}}
                       {:degree 6,
                        :mos [3 9],
                        :mos-degrees [6 1],
                        :rotation {:at-zero? false, :degree 1, :mos [9 3]}}),
                      :submos-by-mos
                      {[3 9]
                       [{:degree 0,
                         :mos [3 9],
                         :mos-degrees [0 2],
                         :rotation {:at-zero? true, :degree 0, :mos [3 9]}}
                        {:degree 2,
                         :mos [3 9],
                         :mos-degrees [2 4],
                         :rotation {:at-zero? false, :degree 2, :mos [3 9]}}
                        {:degree 3,
                         :mos [3 9],
                         :mos-degrees [3 5],
                         :rotation {:at-zero? false, :degree 3, :mos [3 9]}}
                        {:degree 6,
                         :mos [3 9],
                         :mos-degrees [6 1],
                         :rotation {:at-zero? false, :degree 1, :mos [9 3]}}],
                       [4 8]
                       [{:degree 1,
                         :mos [4 8],
                         :mos-degrees [1 3],
                         :rotation {:at-zero? false, :degree 1, :mos [4 8]}}
                        {:degree 4,
                         :mos [4 8],
                         :mos-degrees [4 6],
                         :rotation {:at-zero? false, :degree 4, :mos [4 8]}}
                        {:degree 5,
                         :mos [4 8],
                         :mos-degrees [5 0],
                         :rotation {:at-zero? true, :degree 0, :mos [8 4]}}]},
                      :true-submos? false}
                     {:generator 2,
                      :pattern [2 2 3],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [3 3 6],
                        :mos-degrees [0 2 4],
                        :rotation {:at-zero? true, :degree 0, :mos [3 3 6]}}
                       {:degree 1,
                        :mos [4 3 5],
                        :mos-degrees [1 3 5],
                        :rotation {:at-zero? false, :degree 1, :mos [4 3 5]}}
                       {:degree 2,
                        :mos [3 4 5],
                        :mos-degrees [2 4 6],
                        :rotation {:at-zero? false, :degree 2, :mos [3 4 5]}}
                       {:degree 3,
                        :mos [3 4 5],
                        :mos-degrees [3 5 0],
                        :rotation {:at-zero? true, :degree 0, :mos [5 3 4]}}
                       {:degree 4,
                        :mos [4 3 5],
                        :mos-degrees [4 6 1],
                        :rotation {:at-zero? false, :degree 1, :mos [5 4 3]}}
                       {:degree 5,
                        :mos [4 3 5],
                        :mos-degrees [5 0 2],
                        :rotation {:at-zero? true, :degree 0, :mos [3 5 4]}}
                       {:degree 6,
                        :mos [3 4 5],
                        :mos-degrees [6 1 3],
                        :rotation {:at-zero? false, :degree 1, :mos [4 5 3]}}),
                      :submos-by-mos
                      {[3 3 6]
                       [{:degree 0,
                         :mos [3 3 6],
                         :mos-degrees [0 2 4],
                         :rotation {:at-zero? true, :degree 0, :mos [3 3 6]}}],
                       [3 4 5]
                       [{:degree 2,
                         :mos [3 4 5],
                         :mos-degrees [2 4 6],
                         :rotation {:at-zero? false, :degree 2, :mos [3 4 5]}}
                        {:degree 3,
                         :mos [3 4 5],
                         :mos-degrees [3 5 0],
                         :rotation {:at-zero? true, :degree 0, :mos [5 3 4]}}
                        {:degree 6,
                         :mos [3 4 5],
                         :mos-degrees [6 1 3],
                         :rotation {:at-zero? false, :degree 1, :mos [4 5 3]}}],
                       [4 3 5]
                       [{:degree 1,
                         :mos [4 3 5],
                         :mos-degrees [1 3 5],
                         :rotation {:at-zero? false, :degree 1, :mos [4 3 5]}}
                        {:degree 4,
                         :mos [4 3 5],
                         :mos-degrees [4 6 1],
                         :rotation {:at-zero? false, :degree 1, :mos [5 4 3]}}
                        {:degree 5,
                         :mos [4 3 5],
                         :mos-degrees [5 0 2],
                         :rotation {:at-zero? true, :degree 0, :mos [3 5 4]}}]},
                      :true-submos? false}
                     {:generator 2,
                      :pattern [2 2 2 1],
                      :period 7,
                      :submos
                      ({:degree 0,
                        :mos [3 3 4 2],
                        :mos-degrees [0 2 4 6],
                        :rotation {:at-zero? true, :degree 0, :mos [3 3 4 2]}}
                       {:degree 1,
                        :mos [4 3 4 1],
                        :mos-degrees [1 3 5 0],
                        :rotation {:at-zero? true, :degree 0, :mos [1 4 3 4]}}
                       {:degree 2,
                        :mos [3 4 3 2],
                        :mos-degrees [2 4 6 1],
                        :rotation {:at-zero? false, :degree 1, :mos [2 3 4 3]}}
                       {:degree 3,
                        :mos [3 4 3 2],
                        :mos-degrees [3 5 0 2],
                        :rotation {:at-zero? true, :degree 0, :mos [3 2 3 4]}}
                       {:degree 4,
                        :mos [4 3 4 1],
                        :mos-degrees [4 6 1 3],
                        :rotation {:at-zero? false, :degree 1, :mos [4 1 4 3]}}
                       {:degree 5,
                        :mos [4 3 3 2],
                        :mos-degrees [5 0 2 4],
                        :rotation {:at-zero? true, :degree 0, :mos [3 3 2 4]}}
                       {:degree 6,
                        :mos [3 4 3 2],
                        :mos-degrees [6 1 3 5],
                        :rotation {:at-zero? false, :degree 1, :mos [4 3 2 3]}}),
                      :submos-by-mos
                      {[3 3 4 2]
                       [{:degree 0,
                         :mos [3 3 4 2],
                         :mos-degrees [0 2 4 6],
                         :rotation {:at-zero? true, :degree 0, :mos [3 3 4 2]}}],
                       [3 4 3 2]
                       [{:degree 2,
                         :mos [3 4 3 2],
                         :mos-degrees [2 4 6 1],
                         :rotation {:at-zero? false, :degree 1, :mos [2 3 4 3]}}
                        {:degree 3,
                         :mos [3 4 3 2],
                         :mos-degrees [3 5 0 2],
                         :rotation {:at-zero? true, :degree 0, :mos [3 2 3 4]}}
                        {:degree 6,
                         :mos [3 4 3 2],
                         :mos-degrees [6 1 3 5],
                         :rotation {:at-zero? false, :degree 1, :mos [4 3 2 3]}}],
                       [4 3 3 2]
                       [{:degree 5,
                         :mos [4 3 3 2],
                         :mos-degrees [5 0 2 4],
                         :rotation {:at-zero? true, :degree 0, :mos [3 3 2 4]}}],
                       [4 3 4 1]
                       [{:degree 1,
                         :mos [4 3 4 1],
                         :mos-degrees [1 3 5 0],
                         :rotation {:at-zero? true, :degree 0, :mos [1 4 3 4]}}
                        {:degree 4,
                         :mos [4 3 4 1],
                         :mos-degrees [4 6 1 3],
                         :rotation {:at-zero? false, :degree 1, :mos [4 1 4 3]}}]},
                      :true-submos? false}
                     {:generator 3,
                      :pattern [3 4],
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
                      :submos-by-mos
                      {[5 7]
                       [{:degree 0,
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
                        {:degree 5,
                         :mos [5 7],
                         :mos-degrees [5 1],
                         :rotation {:at-zero? false, :degree 1, :mos [7 5]}}
                        {:degree 6,
                         :mos [5 7],
                         :mos-degrees [6 2],
                         :rotation {:at-zero? false, :degree 2, :mos [7 5]}}],
                       [6 6]
                       [{:degree 4,
                         :mos [6 6],
                         :mos-degrees [4 0],
                         :rotation {:at-zero? true, :degree 0, :mos [6 6]}}]},
                      :true-submos? true}
                     {:generator 3,
                      :pattern [3 3 1],
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
                      :submos-by-mos
                      {[5 5 2]
                       [{:degree 0,
                         :mos [5 5 2],
                         :mos-degrees [0 3 6],
                         :rotation {:at-zero? true, :degree 0, :mos [5 5 2]}}
                        {:degree 2,
                         :mos [5 5 2],
                         :mos-degrees [2 5 1],
                         :rotation {:at-zero? false, :degree 1, :mos [2 5 5]}}
                        {:degree 3,
                         :mos [5 5 2],
                         :mos-degrees [3 6 2],
                         :rotation {:at-zero? false, :degree 2, :mos [2 5 5]}}
                        {:degree 5,
                         :mos [5 5 2],
                         :mos-degrees [5 1 4],
                         :rotation {:at-zero? false, :degree 1, :mos [5 2 5]}}
                        {:degree 6,
                         :mos [5 5 2],
                         :mos-degrees [6 2 5],
                         :rotation {:at-zero? false, :degree 2, :mos [5 2 5]}}],
                       [5 6 1]
                       [{:degree 1,
                         :mos [5 6 1],
                         :mos-degrees [1 4 0],
                         :rotation {:at-zero? true, :degree 0, :mos [1 5 6]}}],
                       [6 5 1]
                       [{:degree 4,
                         :mos [6 5 1],
                         :mos-degrees [4 0 3],
                         :rotation {:at-zero? true, :degree 0, :mos [5 1 6]}}]},
                      :true-submos? true}
                     {:generator 3,
                      :pattern [2 1 2 1 1],
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
                      :submos-by-mos
                      {[3 2 3 2 2]
                       [{:degree 0,
                         :mos [3 2 3 2 2],
                         :mos-degrees [0 2 3 5 6],
                         :rotation {:at-zero? true, :degree 0, :mos [3 2 3 2 2]}}
                        {:degree 3,
                         :mos [3 2 3 2 2],
                         :mos-degrees [3 5 6 1 2],
                         :rotation {:at-zero? false, :degree 1, :mos [2 2 3 2 3]}}
                        {:degree 6,
                         :mos [3 2 3 2 2],
                         :mos-degrees [6 1 2 4 5],
                         :rotation {:at-zero? false, :degree 1, :mos [2 3 2 2 3]}}],
                       [3 2 4 1 2]
                       [{:degree 2,
                         :mos [3 2 4 1 2],
                         :mos-degrees [2 4 5 0 1],
                         :rotation {:at-zero? true, :degree 0, :mos [1 2 3 2 4]}}],
                       [4 1 4 1 2]
                       [{:degree 5,
                         :mos [4 1 4 1 2],
                         :mos-degrees [5 0 1 3 4],
                         :rotation {:at-zero? true, :degree 0, :mos [1 4 1 2 4]}}],
                       [4 1 4 2 1]
                       [{:degree 1,
                         :mos [4 1 4 2 1],
                         :mos-degrees [1 3 4 6 0],
                         :rotation {:at-zero? true, :degree 0, :mos [1 4 1 4 2]}}],
                       [4 2 3 2 1]
                       [{:degree 4,
                         :mos [4 2 3 2 1],
                         :mos-degrees [4 6 0 2 3],
                         :rotation {:at-zero? true, :degree 0, :mos [3 2 1 4 2]}}]},
                      :true-submos? true})]
    (is (= all-submos
           (subject/make-all-submos [1 2 2 1 2 2 2] 5)))))
