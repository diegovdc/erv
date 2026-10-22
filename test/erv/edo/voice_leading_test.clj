(ns erv.edo.voice-leading-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.edo.voice-leading :as vl]))

(deftest chord-progression-test
  (is (= '[[{:chord [0 4 7], :degree 0} {:chord (-1 4 7), :degree 4, :mvt -1}]
           [{:chord [0 4 7], :degree 0} {:chord (0 4 9), :degree 9, :mvt 2}]
           [{:chord [0 4 7], :degree 0} {:chord (2 5 9), :degree 2, :mvt 5}]
           [{:chord [0 4 7], :degree 0} {:chord (-3 2 5), :degree 2, :mvt -7}]
           [{:chord [0 4 7], :degree 0} {:chord (0 4 7), :degree 0, :mvt 0}]
           [{:chord [0 4 7], :degree 0} {:chord (-1 2 7), :degree 7, :mvt -3}]
           [{:chord [0 4 7], :degree 0} {:chord (0 5 9), :degree 5, :mvt 3}]]
         (-> (vl/chord-progression {:scale-steps [2 2 1 2 2 2 1]
                                    :chords-steps #{[4 3 5] [3 4 5]}
                                    :max-len 2
                                    :max-movement 7
                                    :initial-chord-degs [0 4 7]})
             :promise/progressions
             deref)))
  (is (= [[{:chord [0 4 7], :degree 0} {:chord [-1 4 7], :mvt -1, :degree 2}]
          [{:chord [0 4 7], :degree 0} {:chord [0 4 9], :mvt 2, :degree 5}]
          [{:chord [0 4 7], :degree 0} {:chord [0 4 7], :mvt 0, :degree 0}]
          [{:chord [0 4 7], :degree 0} {:chord [-1 2 7], :mvt -3, :degree 4}]
          [{:chord [0 4 7], :degree 0} {:chord [0 5 9], :mvt 3, :degree 3}]]
         (-> (vl/chord-progression (merge {:max-len 2
                                           :max-movement 3}
                                          (vl/submos-chord-progression-config 0 [2 2 1 2 2 2 1] [2 2 3])))
             :promise/progressions
             deref)))
  (testing "Ensure chord progressions always increase in size on every loop, except when they exceed the maximum number of degrees in the scale"
    (let [config {:scale-size         12
                  :scale-steps        [2 2 1 2 2 2 1]
                  :chords-steps       #{[4 3 5] [3 4 5]}
                  :chord->degree      (#'vl/default-chord->degree
                                       [2 2 1 2 2 2 1]
                                       #{[4 3 5] [3 4 5]})
                  :max-movement       7
                  :initial-chord-degs [0 4 7]}
          get-prog-count (fn [max-len] (-> (vl/chord-progression (assoc config :max-len max-len))
                                           :promise/progressions
                                           deref
                                           count))]
      (is (<= (get-prog-count 5)
              (get-prog-count 6)
              (get-prog-count 7)
              (get-prog-count 8))))))

(deftest min-note-diff-filter-test
  (is (= '([{:chord (2 5 9), :degree 2, :mvt 3}
            {:chord (-1 2 7), :degree 7, :mvt -6}
            {:chord (0 4 7), :degree 0, :mvt 3}])
         (vl/min-note-diff-filter
          2
          '[[{:chord [0 4 7], :degree 0}
             {:chord (-1 4 7), :mvt -1, :degree 4}
             {:chord (0 4 9), :mvt 3, :degree 9}]
            [{:chord (2 5 9), :mvt 3, :degree 2}
             {:chord (-1 2 7), :mvt -6, :degree 7}
             {:chord (0 4 7), :mvt 3, :degree 0}]]))))

(deftest default-chord->degree-test
  (is (= {#{0 7 4} 0
          #{2 9 5} 2
          #{7 4 11} 4
          #{0 9 5} 5
          #{7 2 11} 7
          #{0 4 9} 9}
         (#'vl/default-chord->degree [2 2 1 2 2 2 1]
                                     #{[4 3 5] [3 4 5]}))))
