(def funct (fn [x y z] (+ x (* y (+ z (- y z))))))
(def y (+ 3 2 (- 4 1)))
(print (funct 2 7 3))
(print y)