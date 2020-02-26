;;sleeps for the specified number of seconds
	
	(defun STD-SLEEP (secs / endt)
	(setq endt (+ (getvar "DATE") (/ secs 86400.0)))
	(while (< (getvar "DATE") endt) T)) 