(ns erv.mos.core
  (:require [erv.mos.mos :as mos]))

;; cf. pg 3 of  http://anaphoria.com/xen9mar.pdf

;;                  S M N G D R P S
(def kalyan-fourths [5 4 4 4 4 4 4])
(def kalayan [2 2 2 1 2 2 1])
;;S R G M P D N S    índices
,,[2 2 2 1 2 2 1]
;;S R G m P D N S -- 2->3
,,[2 2 1 2 2 2 1]
;;S R G M P D n S -- 6->7
,,[2 2 2 1 2 1 2]
;;S R G m P D n S -- 2->3 y 5->6
,,[2 2 1 2 2 1 2]
;;S R g M P D N S -- 1->2
,,[2 1 3 1 2 2 1]
;;S R g m P D n S -- 1->2, 2->3
,,[2 1 2 2 2 2 1]
;;S R g m P D n S -- 1->2, 2->3 y 5->6
,,[2 1 2 2 2 1 2]

(update-in [1 2 3] [2] dec)

(mos/get-mos-points (count kalyan) 3) ;; the fourths sequence [0 3 6 2 5 1 4 7]
(do
  (defn marwa-seq []
    (loop [s [3 6 2 5 1 4 #_0]
           m-perms []]
      (let [current-el (first s)
            next-s (rest s)]
        (cond (not current-el) m-perms
              (not (seq m-perms)) (recur next-s (conj m-perms [current-el]))
              :else (recur next-s
                           (concat m-perms
                                   (loop [m-perms* m-perms
                                          sub-perms [[current-el]]]
                                     (let [current-perm-el (first m-perms*)
                                           next-perms (rest m-perms*)]
                                       (cond
                                         (not current-perm-el) sub-perms
                                         :else
                                         (recur next-perms
                                                (conj sub-perms
                                                      (conj current-perm-el current-el))))))))))))
  (marwa-seq))
(defn mod* [scale index]
  (mod index (count scale)))
(do
  (defn marwa []
    (let [scale kalayan]
      (->> (marwa-seq)
           (map
            (fn [perms]
              (reduce
               #(-> %1
                    (update-in [(mod* scale (dec %2))] dec)
                    (update-in [(mod* scale %2)] inc))
               scale
               perms))))))
  (marwa))
(clojure.pprint/pprint (marwa-seq))
(clojure.pprint/pprint (marwa))
#_(do

    (defn get-marwa-seq [points]
      {:points (reduce #(conj %1 (+ (last %1) %2)) [0] points)
       :interval-indexes nil }
      )
    (get-marwa-seq kalyan))

(count [[2 2 2 1 2 2 1]
        [2 2 1 2 2 2 1]
        [2 2 2 1 2 1 2]
        [2 2 1 2 2 1 2]
        [2 1 3 1 2 2 1]
        [2 1 2 2 2 2 1]
        [2 1 3 1 2 1 2]
        [2 1 2 2 2 1 2]
        [2 2 2 1 1 3 1]
        [2 2 1 2 1 3 1]
        [2 2 2 1 1 2 2]
        [2 2 1 2 1 2 2]
        [2 1 3 1 1 3 1]
        [2 1 2 2 1 3 1]
        [2 1 3 1 1 2 2]
        [2 1 2 2 1 2 2]
        [1 3 2 1 2 2 1]
        [1 3 1 2 2 2 1]
        [1 3 2 1 2 1 2]
        [1 3 1 2 2 1 2]
        [1 2 3 1 2 2 1]
        [1 2 2 2 2 2 1]
        [1 2 3 1 2 1 2]
        [1 2 2 2 2 1 2]
        [1 3 2 1 1 3 1]
        [1 3 1 2 1 3 1]
        [1 3 2 1 1 2 2]
        [1 3 1 2 1 2 2]
        [1 2 3 1 1 3 1]
        [1 2 2 2 1 3 1]
        [1 2 3 1 1 2 2]
        [1 2 2 2 1 2 2]])

(subvec (into [] (marwa-seq)) 0 32)
