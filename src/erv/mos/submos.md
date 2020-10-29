;; MOS     1 2 2 1 2 2 2
;;         a b b a b b b
;;         1 1 1 1 1 1 1 -> normalize the MOS (for derivation)
;; SUBMOS  2   1 2   1 1
;;         2   1 2   2
;;         2   3     2
;;         3     2   2
;;         5         2
;;         5         1 1
;;         5         2
;;         3     3     1
;;         3     4
;;         6           1


;; Grouping the rotations
;; SUBMOS pattern:
;;   2   1  2    1 1
;;r1 (ab) b (ab) b b --rotating to the left
;;r2 (bb) a (bb) b a
;;r2 (ba) b (bb) a b
;; and so on...



;; 1 1   2  1 1   2  1 1   2 1   2 -- MOS
;; 2     1  2     1  2     2     1 -- Grouping Pattern MOS (GPM)
;; (1 1) 2  (1 1) 2  (1 1) (2 1) 2 -- Grouped MOS
;;  2    2   2    2   2     3    2 -- SUBMOS #9


;; 1 1   2  1 1   2  1  1   2 1   2 -- MOS
;; 2     1  2     2     1   2     1 -- GMP rotated (equivalent to rotating the MOS)
;; (1 1) 2  (1 1) (2 1) 1  (2 1)  2 -- Grouped MOS
;; 2     2   2     3    1   3     2 -- SUBMOS #10
