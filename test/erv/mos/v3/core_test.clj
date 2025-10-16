(ns erv.mos.v3.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.mos.v3.core :as subject]))

(deftest gen->mos-ratios-test
  (is (= [{:meta
           {:period 2,
            :scale :mos,
            :size 2,
            :intervals/cents '(701.9550008653874 498.04499913461217),
            :intervals/ratios '(3/2 4/3),
            :mos/L 3/2,
            :mos/L.cents 701.9550008653874,
            :mos/generator 3/2,
            :mos/normalized-by 1,
            :mos/pattern "Ls",
            :mos/pattern.name "1s1L",
            :mos/s 4/3,
            :mos/s.cents 498.04499913461217,
            :mos/sL-ratio 9/8,
            :mos/sL-ratio.cents 594.1229411833639,
            :mos/sL-ratio.float (float 1.4094208),
            :mos/type :ratio},
           :scale
           '({:bounded-ratio 1, :bounding-period 2, :ratio 1}
             {:bounded-ratio 3/2, :bounding-period 2, :ratio 3/2})}
          {:meta
           {:period 2,
            :scale :mos,
            :size 3,
            :intervals/cents
            '(203.91000173077484 498.04499913461217 498.04499913461217),
            :intervals/ratios '(9/8 4/3 4/3),
            :mos/L 4/3,
            :mos/L.cents 498.04499913461217,
            :mos/generator 3/2,
            :mos/normalized-by 1,
            :mos/pattern "sLL",
            :mos/pattern.name "1s2L",
            :mos/s 9/8,
            :mos/s.cents 203.91000173077484,
            :mos/sL-ratio 32/27,
            :mos/sL-ratio.cents 1546.0122684152425,
            :mos/sL-ratio.float (float 2.4424746),
            :mos/type :ratio},
           :scale
           '({:bounded-ratio 1, :bounding-period 2, :ratio 1}
             {:bounded-ratio 9/8, :bounding-period 2, :ratio 9/8}
             {:bounded-ratio 3/2, :bounding-period 2, :ratio 3/2})}
          {:meta
           {:period 2,
            :scale :mos,
            :size 5,
            :intervals/cents
            '(203.91000173077484
              203.91000173077484
              294.1349974038373
              203.91000173077484
              294.1349974038373),
            :intervals/ratios '(9/8 9/8 32/27 9/8 32/27),
            :mos/L 32/27,
            :mos/L.cents 294.1349974038373,
            :mos/generator 3/2,
            :mos/normalized-by 1,
            :mos/pattern "ssLsL",
            :mos/pattern.name "3s2L",
            :mos/s 9/8,
            :mos/s.cents 203.91000173077484,
            :mos/sL-ratio 256/243,
            :mos/sL-ratio.cents 634.2550936716368,
            :mos/sL-ratio.float (float 1.4424746),
            :mos/type :ratio},
           :scale
           '({:bounded-ratio 1, :bounding-period 2, :ratio 1}
             {:bounded-ratio 9/8, :bounding-period 2, :ratio 9/8}
             {:bounded-ratio 81/64, :bounding-period 2, :ratio 81/64}
             {:bounded-ratio 3/2, :bounding-period 2, :ratio 3/2}
             {:bounded-ratio 27/16, :bounding-period 2, :ratio 27/16})}
          {:meta
           {:period 2,
            :scale :mos,
            :size 7,
            :intervals/cents
            '(203.91000173077484
              203.91000173077484
              203.91000173077484
              90.22499567306232
              203.91000173077484
              203.91000173077484
              90.22499567306232),
            :intervals/ratios '(9/8 9/8 9/8 256/243 9/8 9/8 256/243),
            :mos/L 9/8,
            :mos/L.cents 203.91000173077484,
            :mos/generator 3/2,
            :mos/normalized-by 1,
            :mos/pattern "LLLsLLs",
            :mos/pattern.name "2s5L",
            :mos/s 256/243,
            :mos/s.cents 90.22499567306232,
            :mos/sL-ratio 2187/2048,
            :mos/sL-ratio.cents 1411.6001602157576,
            :mos/sL-ratio.float (float 2.2600167),
            :mos/type :ratio},
           :scale
           '({:bounded-ratio 1, :bounding-period 2, :ratio 1}
             {:bounded-ratio 9/8, :bounding-period 2, :ratio 9/8}
             {:bounded-ratio 81/64, :bounding-period 2, :ratio 81/64}
             {:bounded-ratio 729/512, :bounding-period 2, :ratio 729/512}
             {:bounded-ratio 3/2, :bounding-period 2, :ratio 3/2}
             {:bounded-ratio 27/16, :bounding-period 2, :ratio 27/16}
             {:bounded-ratio 243/128, :bounding-period 2, :ratio 243/128})}]
         (subject/gen->mos-ratios 3/2 2 7))))
