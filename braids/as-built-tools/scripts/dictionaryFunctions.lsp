
(defun nestedDictionaryItem
	(
		theDictionary ;; a vla-object that is a dictionary
		keyPath ;; a list of keys, that form the path, through the nested dicrtionaries, that we want to get at
		/
		i
		x
	)
	
	(setq i 0)
	(setq x theDictionary)
	(while (< i (length keyPath))
		(setq x
			(vla-Item x (nth i keyPath))
		)
		(setq i (+ 1 i))
	)
	
	x
)
;========

(defun keys ;;returns a list of the keys of a Dictionary object
	(
		theDictionary
		/
		keysList
		i
	)
	(setq keysList (list ))
	
	;(type theDictionary)
	
	(setq i 0)
	(while (< i (vla-get-Count theDictionary))
		(setq keysList
			(append
				keysList
				(list 
					(vla-GetName theDictionary 
						(vla-Item theDictionary i)
					)
				)
			)
		)		
		(setq i (+ 1 i))
	)
	
	keysList
)
;========\