(ns erv.constant-structures.graphics
  (:require
   [erv.constant-structures.core :refer [analyze]]
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.utils.conversions :as convo]
   [quil.core :as q]))

(defn setup []
  #_(q/pixel-density 2)
  (q/frame-rate 1))

(def width 600)
(def height 600)

(def ref-scale (:scale (edo/from-pattern (repeat 22 1))))
(def scale (:scale (cps/make 3 [1 3 7 9 11 15]
                             :norm-fac (* 1 9 11))))
(def scale (->> (cps/make 4 [1 3 9 19 15 21 7]
                          :norm-fac (* 15 21 19 9) ; 
                          )
                :scale
                (group-by :bounded-ratio)
                (map #(-> % second first))
                (sort-by :bounded-ratio)))

(defn ratio->radians [ratio]
  (q/radians (- (* 360 (/ (convo/ratio->cents ratio) 1200)) 90)))
(->> (analyze scale) :interval-data
     (map first))
(->> scale)

(convo/ratio->cents 19/16)
(do
  (def color [0 255 0])
  (def arcs-num 10)
  (def scale (->> (cps/make 2 [7 3 9 19 21])
                  :scale
                  (group-by :bounded-ratio)
                  (map #(-> % second first))
                  (concat (map
                           #(assoc {:color color} :bounded-ratio
                                   #_(convo/cents->ratio %)
                                   %)
                           [;; 57/49
                             ;; 7/6
                             ;; 19/16
                             ;; 76/63
                             ;; 1701/1444
                            19/16
                             ;; 5/4
                             ;; 76/63
                             ;; 24/19
                             ;; 700
                             ;; 900
                             ;; 1020
                            ]))
                  (sort-by :bounded-ratio)))
  (def analysis (-> (analyze scale)
                    :non-cs-intervals))
  (defn make-arcs [scale]
    (->> analysis
         :intervals
         (mapcat (fn [[_ {:keys [intervals]}]]
                   (map
                    (fn [{:keys [steps interval]}]
                      {:start (ratio->radians (first interval))
                       :end (ratio->radians (second interval))
                       :steps steps})
                    intervals)))))

  (def arcs (make-arcs scale)))
(defn draw []
  (let [cx (/ width 2)
        cy (/ height 2)
        diam (* 0.9 height)
        radius (/ diam 2)]
    (q/background 0)
    (q/translate cx cy)
    (q/stroke 255)

    (q/no-fill)
    (q/stroke-weight 4)

    (q/stroke 0 100 0 170)

    #_(doseq [{:keys [bounded-ratio] :as note} ref-scale]
        (q/push-matrix)
        (q/rotate (ratio->radians bounded-ratio))
        (q/line 0 0 radius 0)
        (q/pop-matrix))

    (q/stroke 255)
    (doseq [{:keys [bounded-ratio color] :as note} scale]
      (q/push-matrix)
      (q/rotate (q/radians (- (* 360 (/ (convo/ratio->cents bounded-ratio) 1200)) 90)))
      (if color
        (q/stroke 0 255 0)
        (q/stroke 255))
      (q/line 0 0 radius 0)
      (q/pop-matrix))

    (q/stroke 255)

    (q/ellipse 0 0 diam diam)

    (doseq [[i {:keys [start end]}] (take arcs-num (map-indexed vector arcs))]
      (q/stroke 255 0 0)
      (let [i* (* i 10)]
        (q/arc 0 0 (+ i* (- radius 100))
               (+ i* (- radius 100)) start end)))))         ;; Draw a circle at x y with the correct diameter


(comment
  (q/defsketch example
    :title "CS Tool"
    :settings #(q/smooth 80)
    :setup setup
    :draw draw
    :size [width height]
    ;:renderer :p2d ;; broken
    ))





