(def x ($ "img"))
(def hello_world (fn []
	(alert (+ 2 3))
	(console.log "log")))
(.click x (fn []
				(alert "img")))
