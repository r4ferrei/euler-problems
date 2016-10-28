(let [all-fibs (iterate #(list (+ (first %) (second %)) (first %))
                        '(1 0))]
  (println (transduce (comp (map first)
                            (filter even?)
                            (take-while #(< % 4000000)))
                      + all-fibs)))
