; Prompts the user to select a line (using nentsel, so that a nested line may be selected).
; Prompts the user to enter a string.
; Writes the string into the xdata of the selected line entity.
; Runs the annotateMyHandles function to update the annotation texts, text objects placed on each line containing that line's xdata string.

; Registers the application name.
(regapp "neil")

(defun getAttachedString
	(
		arg
		/
		theEname
		theEntity
		returnValue
		thisApplicationName
		xDataGroupCodeForString
	)
	(setq thisApplicationName "neil")
	(setq xDataGroupCodeForString 1000)
	(COND 
		(
			(= (type arg) 'ENAME)
			(setq theEname arg)
		)
		(
			(= (type arg) 'VLA-OBJECT)
			(setq theEname 
				(handent (vla-get-Handle arg))
			)
		)
		(
			T
			(princ "error: getAttachedString was passed an argument that was neither an ENAME nor a VLA-OBJECT.")
		)
		
	)
	
	(setq theEntity 
		(entget theEname (list thisApplicationName))
	)
	(if (assoc -3 theEntity)
		(progn
			; in this case, selectedEntity has some xdata
			; (princ "found some exisiting xData.\n")
			; ; (princ 
				; ; (cdr (assoc 1000 (cdr (nth 0 (cdr
					; ; (assoc -3 theEntity)
				; ; )))))
			; ; )
			(setq returnValue 
				(cdr 
					(assoc xDataGroupCodeForString 
						(cdr 
							(nth 0 
								(cdr
									(assoc -3 theEntity)
								)
							)
						)
					)
				)
			)
		)
		(progn
			;in this case, selectedEntity does not have any existing xData (at least none that we care about)
			;(princ "no exisiting xData found.\n")
			(setq returnValue nil)
		)
	)	
	returnValue
)


(defun setAttachedString
	(
		arg
		newAttachedString
		/
		theEname
		theEntity
		returnValue
		newXData
		thisApplicationName
		xDataGroupCodeForString
	)
	(setq thisApplicationName "neil")
	(setq xDataGroupCodeForString 1000)
	(COND 
		(
			(= (type arg) 'ENAME)
			(setq theEname arg)
		)
		(
			(= (type arg) 'VLA-OBJECT)
			(setq theEname 
				(handent (vla-get-Handle arg))
			)
		)
		(
			T
			(princ "error: setAttachedString was passed a first argument that was neither an ENAME nor a VLA-OBJECT.")
		)
	)
	
	(setq newXData                       
		(list 
			-3 
			(list 
				thisApplicationName                 
				(cons xDataGroupCodeForString  newAttachedString) 
			)
		)                               
	)  
	
	(setq theEntity 
		(entget theEname (list thisApplicationName))
	)
	(if (assoc -3 theEntity)
		(progn
			; in this case, theEntity already has some xdata, so we will replace the existing xData with
			; the new xData
			
			;(princ (strcat "existing xData: "))(princ (assoc -3 theEntity))(princ "\n.\n")
			
			(setq theEntity
			  (subst 
				newXData 			;replacement
				(assoc -3 theEntity) 	;needle
				theEntity 				;haystack
			  )
			) 
		)
		(progn
			;in this case, theEntity does not have any existing xData, so we will simply append the 
			; new xData
			;(princ "there is no existing xData")(princ "\n.\n")
			(setq theEntity
			  (cons newXData theEntity)
			) 
		)
	)
	; Write the newly modified entity to the database.
	(entmod theEntity)
	
	(setq returnValue newAttachedString)
	returnValue
)
	
;========================

;;this is the version that selects a nested object
(defun c:nSET-ATTACHED-STRING
	(
	
	/
		enameOfSelectedEntity
		stringToAttach
	)
	(setq enameOfSelectedEntity
		(nth 0 (nentsel "Select the entity to which you want to attach a string."))
	)	
	(princ "\n")
	(setq stringToAttach
		(getstring T "Type the string that you want to attach to the entity that you just selected: \n")
	)
	(setAttachedString enameOfSelectedEntity stringToAttach)
	(princ "SUCCESS.\n")
	(princ)
)
;========================

;; this is the version that selects an object in the current context (uses entsel rather than nentsel)
(defun c:SET-ATTACHED-STRING
	(
	
	/
		enameOfSelectedEntity
		stringToAttach
	)
	(setq enameOfSelectedEntity
		(nth 0 (entsel "Select the entity to which you want to attach a string."))
	)	
	(princ "\n")
	(setq stringToAttach
		(getstring T "Type the string that you want to attach to the entity that you just selected: \n")
	)
	(setAttachedString enameOfSelectedEntity stringToAttach)
	(princ "SUCCESS.\n")
	(princ)
)
;========================

(defun c:FLAG-AS-CONSTRUCTION
	(
	
	/
		enameOfSelectedEntity
		stringToAttach
		i
		selectionSet
	)
	
	;;check for pre-exisiting selection in order to implement noun-verb command flow.
	(setq selectionSet (ssget "I"))
	(if selectionSet 
		(progn ;;in this case, there was a pre-exisiting selection
			(princ "using exisiting selection...\n")
		)
		(progn
			(prompt "Select all the objects that you want to flag as construction, then press Enter.")
			(setq selectionSet (ssget ))
		)
	)
		;;at this point 
	
	(setq i 0)
	(while (< i (sslength selectionSet))
		(setq enameOfSelectedEntity (ssname selectionSet i))
		(setq stringToAttach "construction"	)
		(setAttachedString enameOfSelectedEntity stringToAttach)
		(setq i (+ i 1))
	)	
	(princ (strcat "Flagged " (itoa i) " entities as construction.\n"))	
	(princ)
)
;========================
