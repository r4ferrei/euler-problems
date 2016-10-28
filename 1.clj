(defn solve [curr]
  (cond
    (zero? curr) 0
    (or (zero? (rem curr 3)) (zero? (rem curr 5)))
    (+ curr (solve (dec curr)))
    :else (solve (dec curr))))

(defn solve2 [curr]
  (cond
    (zero? curr) 0
    :else (+ (if (or (zero? (rem curr 3)) (zero? (rem curr 5)))
               curr
               0)
             (solve2 (dec curr)))))

(defn solve3 [curr]
  (reduce + (filter #(or (zero? (rem % 3)) (zero? (rem % 5)))
                    (range (inc curr)))))

(println (solve 999))
(println (solve2 999))
(println (solve3 999))
