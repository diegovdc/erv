(ns erv.scale.scl-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.scale.scl :refer [make-scl-file]]))

(deftest make-scl-file-test
  (testing "Creates the data necessary to create an scl file. Only data necessary is a map with `:scale` and the scale data containing `:bounded-ratio` and `:bounding-period` keys."
    (is (= {:filename "unkown.scl",
            :content
            "! unkown.scl
!

 6
!
15/14
5/4
10/7
3/2
12/7
2/1

! Made with the \"erv\" library: https://github.com/diegovdc/erv"}
           (make-scl-file
            (-> {:scale [{:bounded-ratio 1, :bounding-period 2}
                         {:bounded-ratio 15/14, :bounding-period 2}
                         {:bounded-ratio 5/4, :bounding-period 2}
                         {:bounded-ratio 10/7, :bounding-period 2}
                         {:bounded-ratio 3/2, :bounding-period 2}
                         {:bounded-ratio 12/7, :bounding-period 2}]})))))
  (testing "Can automatically name and describe cps scales if they have their metadata"
    (is (= {:filename "cps-2_4-1_3_5_7_p2.scl",
            :content
            "! cps-2_4-1_3_5_7_p2.scl
!
A 2)4 CPS scale with factors {1, 3, 5, 7} and period 2.
 6
!
15/14
5/4
10/7
3/2
12/7
2/1

! Made with the \"erv\" library: https://github.com/diegovdc/erv"}
           (make-scl-file
            (-> {:meta
                 {:scale :cps,
                  :period 2,
                  :size 6,
                  :cps/size 2,
                  :cps/factors [1 3 5 7],
                  :cps/normalized-by 7,
                  :cps/type "2)4"}
                 :scale [{:bounded-ratio 1, :bounding-period 2}
                         {:bounded-ratio 15/14, :bounding-period 2}
                         {:bounded-ratio 5/4, :bounding-period 2}
                         {:bounded-ratio 10/7, :bounding-period 2}
                         {:bounded-ratio 3/2, :bounding-period 2}
                         {:bounded-ratio 12/7, :bounding-period 2}]})))))
  (testing "The metadata key can be used to add name, description and footer and can override the automatic generation of these fields."
    (is (= {:filename "my-cps.scl",
            :content
            "! my-cps.scl
!
scale description
 6
!
15/14
5/4
10/7
3/2
12/7
2/1

! footer note
! Made with the \"erv\" library: https://github.com/diegovdc/erv"}
           (make-scl-file
            (-> {:meta
                 {:scl/name "my-cps.scl"
                  :scl/description "scale description"
                  :scl/footer "footer note"
                  :scale :cps,
                  :period 2,
                  :size 6,
                  :cps/size 2,
                  :cps/factors [1 3 5 7],
                  :cps/normalized-by 7,
                  :cps/type "2)4"}
                 :scale [{:bounded-ratio 1, :bounding-period 2}
                         {:bounded-ratio 15/14, :bounding-period 2}
                         {:bounded-ratio 5/4, :bounding-period 2}
                         {:bounded-ratio 10/7, :bounding-period 2}
                         {:bounded-ratio 3/2, :bounding-period 2}
                         {:bounded-ratio 12/7, :bounding-period 2}]}))))))
