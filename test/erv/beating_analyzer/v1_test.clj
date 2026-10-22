(ns erv.beating-analyzer.v1-test
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]
   [erv.beating-analyzer.v1 :as subject]
   [erv.utils.exact :as exact.utils]))

(deftest get-beat-data-test
  (let [data (subject/get-beat-data 2 1 (range 1 6)
                                    [1 5/4 3/2])]
    #_(is (= [{:root-hz 64,
               :degree-1 0,
               :ratio-2-partial "1",
               :degree-2 2,
               :beat-freq.ratio "32",
               :beat-freq.factors {:numer ["2" "2" "2" "2" "2"], :denom []},
               :root "B0+62",
               :ratio-1 "1",
               :ratio-1-partial "1",
               :period 1,
               :ratio-2 "3/2",
               :beat-freq.hz 32}]

             (->> data
                  (drop 99)
                  (take 1)
                  (exact.utils/make-readable))))
    (is (= 30 (count data)))))

(subject/get-beat-data 2 1 (range 1 6) [1 5/4 3/2])

(comment

  (def clj (->>
            (subject/get-beat-data 2 1 (range 1 6) [1 5/4 3/2])
            (exact.utils/make-readable)
            (into #{})))
  (-> clj count)
  (->>
   (subject/get-beat-data 2 1 (range 1 6) [1 5/4 3/2])
   (exact.utils/make-readable)
   frequencies
   vals
   (sort)
   reverse)

  (->> (edn/read-string (slurp "test_data.edn"))
       (map #(-> % (update :beat-freq.hz float)))
       (exact.utils/make-readable)
       frequencies
       vals
       count)

  (def cljs (->> (edn/read-string (slurp "test_data.edn"))
                 (map #(-> % (update :beat-freq.hz float)))
                 (exact.utils/make-readable)
                 (into #{})))
  (->> clj (map #(:beat-freq.hz %)))
  (->> clj (filter #(zero? (:beat-freq.hz %))))
  (->> clj (filter
            (fn [%]
              (and
               (= (% :degree-1) "0") ,
               (= (% :degree-2) "2") ,
               (= (% :ratio-2-partial) "2") ,
               #_(= (% :ratio-1) "1") ,
               (= (% :ratio-1-partial) "3") ,
               (= (% :period) "1") ,
               #_(= (% :ratio-2) "3/2")))))
  (-> clj first)
  (-> cljs first)
  (set/difference cljs clj)

  (-> 90/72))
