(ns erv.cps.cycles
  (:require [clojure.set :as set]
            [erv.utils.core :as utils]))



(defn get-next-nodes [graph cycle]
  (->> (graph (-> cycle :seq first))
       (reduce
        (fn [acc next-node]
          (cond
            ;; a cycle that goes inmmediately back to the prev node
            (= (nth (:seq cycle) 1 nil) next-node) acc

            ;; if the cycle does not start and end with the same node then
            ;; we dismiss it because there will be another cycle that will be
            ;; a subset of this one only containing the desired range of nodes
            (and ((:set cycle) next-node)
                 (not= (last (:seq cycle)) next-node))
            acc

            ;; a correctly closing cycle
            ((:set cycle) next-node)
            (conj acc
                  (assoc cycle
                         :seq (conj (:seq cycle) next-node)
                         :status :closed))

            :else
            (conj acc
                  {:seq (conj (:seq cycle) next-node)
                   :set (conj (:set cycle) next-node)
                   :status :open})))
        ())))


(defn update-finder-state [previous-state new-interation-result]
  (let [{:keys [open closed]} (group-by :status new-interation-result)]
    {:open open
     :closed (into (:closed previous-state) closed)}))


(defn init-state [graph]
  {:open (mapv (fn [node]
                 {:seq (list node)
                  :set #{node}
                  :status :open})
               (keys graph))
   :closed []})

(def find-cycles
  (memoize
   (fn [graph]
     (loop [state (init-state graph)
            count* 0]
       (let [iteration-results (flatten (map (partial get-next-nodes graph) (:open state)))
             new-state (update-finder-state state iteration-results)]
         (println count* (count (:open new-state)) (count (:closed new-state))
                  (+ (count (:open new-state)) (count (:closed new-state))))
         (if (seq (:open new-state))
           (recur new-state (inc count*))
           (->> new-state
                :closed
                (map :seq)
                (filter #(> (count %) 3)))))))))

(def get-unique-cycles
  (memoize
   (fn [cycles]
     (:cycles
      (reduce (fn [acc cycle]
                (let [cycle* (drop 1 cycle)]
                  (if (-> acc :set (contains? cycle*))
                    acc
                    {:set (apply conj (:set acc) (utils/get-all-rotations cycle*))
                     :cycles (conj (:cycles acc) cycle*)}
                    ))
                )
              {:set #{} :cycles []}
              cycles)))))
(comment
  (def hexany-graph {#{7 1} #{#{7 5} #{7 3} #{1 5} #{1 3}},
                     #{7 5} #{#{7 1} #{3 5} #{7 3} #{1 5}},
                     #{7 3} #{#{7 1} #{7 5} #{3 5} #{1 3}},
                     #{1 5} #{#{7 1} #{7 5} #{3 5} #{1 3}},
                     #{1 3} #{#{7 1} #{3 5} #{7 3} #{1 5}},
                     #{3 5} #{#{7 5} #{7 3} #{1 5} #{1 3}}})
  (def hcycles (find-cycles hexany-graph))
  (= 600 (-> hcycles count))
  (get-unique-cycles hcycles)
  (count (get-unique-cycles hcycles))

  (def dekany-graph {#{7 1} #{#{7 5} #{7 3} #{1 5} #{7 9} #{1 3} #{1 9}},
                     #{7 5} #{#{7 1} #{3 5} #{7 3} #{9 5} #{1 5} #{7 9}},
                     #{3 5} #{#{7 5} #{7 3} #{9 5} #{1 5} #{1 3} #{3 9}},
                     #{7 3} #{#{7 1} #{7 5} #{3 5} #{7 9} #{1 3} #{3 9}},
                     #{9 5} #{#{7 5} #{3 5} #{1 5} #{7 9} #{3 9} #{1 9}},
                     #{1 5} #{#{7 1} #{7 5} #{3 5} #{9 5} #{1 3} #{1 9}},
                     #{7 9} #{#{7 1} #{7 5} #{7 3} #{9 5} #{3 9} #{1 9}},
                     #{1 3} #{#{7 1} #{3 5} #{7 3} #{1 5} #{3 9} #{1 9}},
                     #{3 9} #{#{3 5} #{7 3} #{9 5} #{7 9} #{1 3} #{1 9}},
                     #{1 9} #{#{7 1} #{9 5} #{1 5} #{7 9} #{1 3} #{3 9}}})

  (def dcycles (find-cycles dekany-graph))
  (-> dcycles count)
  (get-unique-cycles dcycles)
  (count (get-unique-cycles dcycles))

  ;; TODO can't do a full run of the eikosany because of the sheer number of cycles
  ;; Maybe try a Depth First approach instead
  (def eikosany-graph
    {#{3 11} #{#{11 5} #{3 5} #{7 3} #{11 9} #{1 11} #{7 11} #{1 3} #{3 9}},
     #{7 1} #{#{7 5} #{7 3} #{1 11} #{1 5} #{7 11} #{7 9} #{1 3} #{1 9}},
     #{7 5} #{#{7 1} #{11 5} #{3 5} #{7 3} #{9 5} #{1 5} #{7 11} #{7 9}},
     #{11 5} #{#{3 11} #{7 5} #{3 5} #{11 9} #{1 11} #{9 5} #{1 5} #{7 11}},
     #{3 5} #{#{3 11} #{7 5} #{11 5} #{7 3} #{9 5} #{1 5} #{1 3} #{3 9}},
     #{7 3} #{#{3 11} #{7 1} #{7 5} #{3 5} #{7 11} #{7 9} #{1 3} #{3 9}},
     #{11 9} #{#{3 11} #{11 5} #{1 11} #{9 5} #{7 11} #{7 9} #{3 9} #{1 9}},
     #{1 11} #{#{3 11} #{7 1} #{11 5} #{11 9} #{1 5} #{7 11} #{1 3} #{1 9}},
     #{9 5} #{#{7 5} #{11 5} #{3 5} #{11 9} #{1 5} #{7 9} #{3 9} #{1 9}},
     #{1 5} #{#{7 1} #{7 5} #{11 5} #{3 5} #{1 11} #{9 5} #{1 3} #{1 9}},
     #{7 11} #{#{3 11} #{7 1} #{7 5} #{11 5} #{7 3} #{11 9} #{1 11} #{7 9}},
     #{7 9} #{#{7 1} #{7 5} #{7 3} #{11 9} #{9 5} #{7 11} #{3 9} #{1 9}},
     #{1 3} #{#{3 11} #{7 1} #{3 5} #{7 3} #{1 11} #{1 5} #{3 9} #{1 9}},
     #{3 9} #{#{3 11} #{3 5} #{7 3} #{11 9} #{9 5} #{7 9} #{1 3} #{1 9}},
     #{1 9} #{#{7 1} #{11 9} #{1 11} #{9 5} #{1 5} #{7 9} #{1 3} #{3 9}}})

  (def ecycles (find-cycles eikosany-graph))
  (-> ecycles)
  (get-unique-cycles ecycles)
  (count (get-unique-cycles ecycles))

  ;; `get next-nodes`
  (->> [{:seq '(#{7 1}) :set #{#{7 1}} :status :open}]
       (mapcat (partial get-next-nodes hexany-graph))
       (mapcat (partial get-next-nodes hexany-graph)))

  (update-finder-state
   {:open [] :closed []}
   '({:seq (#{7 5} #{7 1}), :set #{#{7 1} #{7 5}}, :status :closed}
     {:seq (#{7 3} #{7 5} #{7 1}), :set #{#{7 1} #{7 5} #{7 3}}, :status :open}
     {:seq (#{1 5} #{7 5} #{7 1}), :set #{#{7 1} #{7 5} #{1 5}}, :status :open}
     {:seq (#{1 3} #{7 5} #{7 1}), :set #{#{7 1} #{7 5} #{1 3}}, :status :open}
     {:seq (#{7 5} #{7 3} #{7 1}), :set #{#{7 1} #{7 5} #{7 3}}, :status :open}
     {:seq (#{7 3} #{7 1}), :set #{#{7 1} #{7 3}}, :status :closed}
     {:seq (#{1 5} #{7 3} #{7 1}), :set #{#{7 1} #{7 3} #{1 5}}, :status :open}
     {:seq (#{1 3} #{7 3} #{7 1}), :set #{#{7 1} #{7 3} #{1 3}}, :status :open}
     {:seq (#{7 5} #{1 5} #{7 1}), :set #{#{7 1} #{7 5} #{1 5}}, :status :open}
     {:seq (#{7 3} #{1 5} #{7 1}), :set #{#{7 1} #{7 3} #{1 5}}, :status :open}
     {:seq (#{1 5} #{7 1}), :set #{#{7 1} #{1 5}}, :status :closed}
     {:seq (#{1 3} #{1 5} #{7 1}), :set #{#{7 1} #{1 5} #{1 3}}, :status :open}
     {:seq (#{7 5} #{1 3} #{7 1}), :set #{#{7 1} #{7 5} #{1 3}}, :status :open}
     {:seq (#{7 3} #{1 3} #{7 1}), :set #{#{7 1} #{7 3} #{1 3}}, :status :open}
     {:seq (#{1 5} #{1 3} #{7 1}), :set #{#{7 1} #{1 5} #{1 3}}, :status :open}
     {:seq (#{1 3} #{7 1}), :set #{#{7 1} #{1 3}}, :status :closed}))

  )
