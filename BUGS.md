# 1

```clj
(->> (cps/make 6 [1 3 5 7 9 11]) cps/+all-subcps :subcps keys )
;;gives 6 and should give 1
```

it is because monads are repeating:

```clj
(->> (cps/make 3 [1 3 5 7 9 11])
       cps/+all-subcps
       :subcps
       keys
       (map #(str/split % #" "))
       (group-by last)
       (filter (fn [[_ sets]] (> (count sets) 1))))
```

Question, which of those repeating monads to keep? the 3)3, 1)1 or 2)2?
