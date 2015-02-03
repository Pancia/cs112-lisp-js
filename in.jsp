(def fac
  (fn [n]
    (if (neq n 1)
      (* n (fac (- n 1)))
      n)))

(log. (fac 5))
