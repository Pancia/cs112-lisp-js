(defclass Rocket
([x y] {speed 5 foo 3})
(launch [x] x)
(bar "bar")
)
(def r (new Rocket))
(log (.speed r))
(log (.bar r))
(log (.launch r 42))
