; Takes each line in the doorAnchor layer and, for each, adds a circle in some 
; 'hingeAnnotations' layer around the start point of the line.
; This provides me with a way to quickly visually check that I have the hinges in the right position.
;; to run... (load "annotateDoorHinges.lsp")


(load "dynamicBlockFunctions.lsp")
(load "matrixFunctions.lsp")
(load "attachedStringFunctions.lsp")
(load "polygonRanking.lsp")
(load "variantArrayConversion.lsp")
(load "regionFunctions.lsp")
(load "polygonCentroid.lsp")
(load "StripMtext v5-0c.lsp")
(load "std-sleep.lsp")

;; arg can be an entityName, a vla-object that is an entity, or a string (which is assumed to be a handle to an entity), and 
;; this function will return the entityName
(defun toEntityName 
	(
		arg
		/
		theEname
		theEntity
		returnValue
	)
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
			(= (type arg) 'STR)
			(setq theEname 
				(handent arg)
			)
		)
		(
			T
			(princ "error: toEntityName was passed an argument that was neither an ENAME nor a VLA-OBJECT nor a STRING.\n")
		)	
	)
	(setq returnValue theEname)
	
	(if (not returnValue) (princ "warning: toEntityName() is returning a nil value.\n"))
	
	returnValue
)
;==============

(defun getAllBlockNames
	()
	;; see http://www.theswamp.org/index.php?topic=41178.0 
	(acet-table-name-list (list "block" 1 4 16))
)

;; returns the path to a dwg file that is the 
;; specified block definition ; ths function is just a convenient way to 
;; have one central place to tell this script where we have stored
;; block definition files.
(defun blockNameToBlockFilePath 
	(
		blockName
		/
		returnValue
	)
	
	(setq returnValue
		(strcat 
			".\\block_definitions\\" blockName ".dwg"
		)
	)
	
	(setq returnValue (findfile returnValue))
	
	returnValue
)
;======



(defun clearAnnotations
	( 
		 
		/
		acadObj
		doc
		modelSpace
		blocks
		blockDefinition
		entity
		entitiesToDelete
	)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq blocks (vla-get-Blocks doc))
	(setq entitiesToDelete (list))
	
	(vlax-for blockDefinition blocks
		(vlax-for entity blockDefinition
			(progn
				(if (= "programmaticAnnotations" (vla-get-Layer entity))
					(progn						
						(setq entitiesToDelete 
							(append entitiesToDelete
								(list
									entity
								)
							)
						)
					)
				)
			)
		)
	)
	
	(foreach entity entitiesToDelete
		(progn
			;;(vlax-dump-object entity)
			(vla-Delete entity)
		)
	)
)




(defun annotateLine
	(	
		thisDoorAnchor
		message
		;transform
		/
		message
		hingePoint
		originalLayer
	)
	;(setq message "default message")
	(if T (progn
		;(princ "checkpoint")
		(setq hingePoint 
			(vlax-safearray->list 
				(vlax-variant-value 
					(vlax-get-property thisDoorAnchor 'StartPoint)
				)
			)
		)
		
		(setq hingePoint 
			(append hingePoint 
				'(1)
			)
		)
		
		(setq hingePoint (mxv currentTransform hingepoint))
		
		(setq hingePoint 
			(list
				(nth 0 hingePoint)
				(nth 1 hingePoint)
				(nth 2 hingePoint)
			)
		)
		
		;(princ hingePoint)
		;(write-line "")
		;(vlax-dump-object thisDoorAnchor)

		; ; (setq attachedString (getAttachedString thisDoorAnchor))
		; ; ;(if (not attachedString) (setq attachedString "nil"))
		; ; (if attachedString 
			; ; (setq annotationText (strcat "attachedString: " attachedString))
			; ; (setq annotationText (strcat "message: " message))
		; ; )
		
		
		; (setq annotationText
			; (strcat
				; "attached string: " 
				; attachedString
				; ", "
				; "message: "
				; message
			; )
		; )
		
		(setq annotationText message)
		
		(if  T ;(if (= :vlax-true (vla-get-Visible thisDoorAnchor)) ; I would like to avoid annotaing lines that lie within construction geometry, but I cannot find any way to detect whether an entity is c construction geometry (i.e. 'converted' with the bconstruction command); the .Visible property does not reflect whether an enitty is construction geometry.
			(progn
				(setq originalLayer (getvar "CLAYER"))
				(setvar "CLAYER" "programmaticAnnotations") ;change the current layer
				(setvar "CMDECHO" 0)
				(command-s "CIRCLE" hingePoint '1)
				;(command-s "TEXT"  hingePoint '3.0 '0.0 message) 
				(command-s "TEXT" "style" "standard" hingePoint '0.0 annotationText) 
				(setvar "CLAYER" originalLayer) ;change the current layer
			)
		)
	)
	)

)



;(quit)
(defun applyToLinesWithin
	(
		rootBlock
		layerNameOfTargetLines
		functionToApply
	/
		i
		thisItem
		j
		tempCollection
		listLength
		theList
		tempList
		indexOfEntityWithinBlock
		message
	)
	
	; (princ (type x))
	; (write-line " ")
	; (princ (type (type x)))
	; (write-line " ")
	
	(if (= (type rootBlock) 'LIST) ;;in this case, we assume we are dealing with a list of vla objects
		(progn
			(write-line "found a LIST")
			(setq theList rootBlock)
			;(setq theList (list ))
		)
	)
	
	(if (= (type rootBlock) 'VARIANT) ;;in this case, we assume we are dealing with the value returned by the explode function.
		(progn
			(write-line "found a VARIANT")
			(setq theList (gc:VariantToLispData rootBlock))
			
		)
	)
	
	(if (= (type rootBlock) 'VLA-OBJECT) ;;in this case, we assume we are dealing with an acad collection, such as modelSpace.
		(progn
			; (write-line "found a VLA-OBJECT: ")
			(princ "found a VLA-OBJECT (whose name is '")(princ (vla-get-name rootBlock))(princ "').\n")
			(setq theList (list ))
			(setq i 0)
			(while (< i (vla-get-count rootBlock))
			;(while (< i 5)
				(setq theList
					(append
						theList
						(list (vla-item rootBlock i))
					)
				)
				(setq i (+ i 1))
			)
		)	
	)
	; (setq listLength  (vla-get-count x))
	(setq listLength (vl-list-length theList))

	;; at this point, regardless of whether we started with a variant array or with an autocad collection object, we now have theList, which is what we will use for the remainder of this function.
	
	(setq indexOfEntityWithinBlock 0)
	(setq i 0)
	(while (< i listlength)
		;(setq thisItem (vla-item theCollection i))
		(setq thisItem (nth i theList))
		(if 
			(and 
				(= "AcDbLine" (vla-get-objectname thisItem))
				(= layerNameOfTargetLines (vlax-get-property thisItem 'Layer))
			)
			(progn
				(setq indexOfEntityWithinBlock (+ 1 indexOfEntityWithinBlock))
				(setq message (strcat currentBlockName " -- " (itoa indexOfEntityWithinBlock)))
				;(princ message)
				;(annotateLine thisItem message)
				(apply functionToApply (list thisItem))
			)
			
		)
		
		(if 
			(and 
				(= "AcDbBlockReference" (vla-get-objectname thisItem))
				
				;;(/= (vla-get-Layer thisItem) "construction") ;; I would like to be able to programmatically determine if a block reference has been set as construction geometry (by using the BCONSTRUCTION command -- but I cannot figure out how to do this, so this is a hack workaround.  This requires me to manually set any block references that I want to be construction to be on the contruction layer (in addition to using the bconstruction command))
				;; unfortunately, the above hack to get around the inability to programmatically identify construction geometry within a block defintion does not work, because all construction geometry appears to be  on layer 0 , and you cannot change it to any other layer (although you don't realize this until you close and then reopen the block definition.  The layer assignment does not stick when the object is deisgnated as construction -- layer setting always reverts to "0".)
				;;Therefore, I am employing an alternate hack: I will identify construction geomnetry by seeing if the attached string is "construction". I will have to manually use my (setAttachedString ) function to tag each object that I want to be construction (i.e. not descendable).
				;;(/= "construction" (getAttachedString thisItem))
				(not  (isBConstruction thisItem))
			)
			(progn
				
				(write-line (strcat "descending into block reference " (vla-get-name thisItem)))
				(pushToNestingStack thisItem)
				(applyToLinesWithin 
					(vla-Item blocks (vla-get-name thisItem)) 
					layerNameOfTargetLines
					functionToApply
				)
				(write-line (strcat "ascending out of block reference " (vla-get-name thisItem)))
				(popFromNestingStack)
			)
			
		)
		(setq i (+ i 1))
	)
)



;(quit)

;;similar to applyToLinesWithin, applyToAllLinesWithin does not take a particular layer name.
(defun applyToAllLinesWithin
	(
		rootBlock
		functionToApply
	/
		i
		thisItem
		j
		tempCollection
		listLength
		theList
		tempList
		indexOfEntityWithinBlock
		message
	)
	
	; (princ (type x))
	; (write-line " ")
	; (princ (type (type x)))
	; (write-line " ")
	
	(if (= (type rootBlock) 'LIST) ;;in this case, we assume we are dealing with a list of vla objects
		(progn
			(write-line "found a LIST")
			(setq theList rootBlock)
			;(setq theList (list ))
		)
	)
	
	(if (= (type rootBlock) 'VARIANT) ;;in this case, we assume we are dealing with the value returned by the explode function.
		(progn
			(write-line "found a VARIANT")
			(setq theList (gc:VariantToLispData rootBlock))
			
		)
	)
	
	(if (= (type rootBlock) 'VLA-OBJECT) ;;in this case, we assume we are dealing with an acad collection, such as modelSpace.
		(progn
			(write-line "found a VLA-OBJECT")
			(setq theList (list ))
			(setq i 0)
			(while (< i (vla-get-count rootBlock))
			;(while (< i 5)
				(setq theList
					(append
						theList
						(list (vla-item rootBlock i))
					)
				)
				(setq i (+ i 1))
			)
		)	
	)
	; (setq listLength  (vla-get-count x))
	(setq listLength (vl-list-length theList))
	
	;; at this point, regardless of whether we started with a variant array or with an autocad collection object, we now have theList, which is what we will use for the remainder of this function.
	
	(setq indexOfEntityWithinBlock 0)
	(setq i 0)
	(while (< i listlength)
		;(setq thisItem (vla-item theCollection i))
		(setq thisItem (nth i theList))
		(if 
			(and
				(= "AcDbLine" (vla-get-objectname thisItem))
			)
			(progn
				(setq indexOfEntityWithinBlock (+ 1 indexOfEntityWithinBlock))
				;(princ "applying the function.\n")
				(apply functionToApply (list thisItem))
				;(setq doorAnchorsCounter (+ 1 doorAnchorsCounter))
			)
		)
		
		(if 
			(and 
				(= "AcDbBlockReference" (vla-get-objectname thisItem))
				;;(/= (vla-get-Layer thisItem) "construction") ;; I would like to be able to programmatically determine if a block reference has been set as construction geometry (by using the BCONSTRUCTION command -- but I cannot figure out how to do this, so this is a hack workaround.  This requires me to manually set any block references that I want to be construction to be on the contruction layer (in addition to using the bconstruction command))
				;; unfortunately, the above hack to get around the inability to programmatically identify construction geometry within a block defintion does not work, because all construction geometry appears to be  on layer 0 , and you cannot change it to any other layer (although you don't realize this until you close and then reopen the block definition.  The layer assignment does not stick when the object is deisgnated as construction -- layer setting always reverts to "0".)
				;;Therefore, I am employing an alternate hack: I will identify construction geomnetry by seeing if the attached string is "construction". I will have to manually use my (setAttachedString ) function to tag each object that I want to be construction (i.e. not descendable).
				;;(/= "construction" (getAttachedString thisItem))
				(not  (isBConstruction thisItem))
			)
			(progn
				(write-line (strcat "descending into block reference " (vla-get-name thisItem)))
				(pushToNestingStack thisItem)
				(applyToAllLinesWithin 
					(vla-Item blocks (vla-get-name thisItem)) 
					functionToApply
				)
				(write-line (strcat "ascending out of block reference " (vla-get-name thisItem)))
				(popFromNestingStack)
			)
			
		)
		(setq i (+ i 1))
	)
)



;(quit)



;;similar to applyToAllLinesWithin, but this is not particular to lines..
(defun applyToAllEntitiesWithin
	(
		rootBlock
		functionToApply
	/
		i
		thisItem
		j
		tempCollection
		listLength
		theList
		tempList
		indexOfEntityWithinBlock
		message
	)
	

	(if (= (type rootBlock) 'LIST) ;;in this case, we assume we are dealing with a list of vla objects
		(progn
			(write-line "found a LIST")
			(setq theList rootBlock)
			;(setq theList (list ))
		)
	)
	
	(if (= (type rootBlock) 'VARIANT) ;;in this case, we assume we are dealing with the value returned by the explode function.
		(progn
			(write-line "found a VARIANT")
			(setq theList (gc:VariantToLispData rootBlock))
			
		)
	)
	
	(if (= (type rootBlock) 'VLA-OBJECT) ;;in this case, we assume we are dealing with an acad collection, such as modelSpace.
		(progn
			(write-line "found a VLA-OBJECT")
			(setq theList (list ))
			(setq i 0)
			(while (< i (vla-get-count rootBlock))
			;(while (< i 5)
				(setq theList
					(append
						theList
						(list (vla-item rootBlock i))
					)
				)
				(setq i (+ i 1))
			)
		)	
	)
	; (setq listLength  (vla-get-count x))
	(setq listLength (vl-list-length theList))
	
	;; at this point, regardless of whether we started with a variant array or with an autocad collection object, we now have theList, which is what we will use for the remainder of this function.
	
	(setq indexOfEntityWithinBlock 0)
	(setq i 0)
	(while (< i listlength)
		;(setq thisItem (vla-item theCollection i))
		(setq thisItem (nth i theList))
		(if (= "AcDbBlockReference" (vla-get-objectname thisItem))
			(progn ;;in this case, thisItem is a block reference, whcih means we will have to descend into it
				(if 

					;;(/= (vla-get-Layer thisItem) "construction") ;; I would like to be able to programmatically
					;; determine if a block reference has been set as construction geometry (by using the BCONSTRUCTION command -- 
					;; but I cannot figure out how to do this, so this is a hack workaround.  This requires me 
					;; to manually set any block references that I want to be construction to be on the 
					;; contruction layer (in addition to using the bconstruction command))
					;; unfortunately, the above hack to get around the inability to programmatically
					;; identify construction geometry within a block defintion does not work, 
					;; because all construction geometry appears to be  on layer 0 , and you cannot 
					;; change it to any other layer (although you don't realize this until you 
					;; close and then reopen the block definition.  The layer assignment does not stick when 
					;; the object is deisgnated as construction -- layer setting always reverts to "0".)
					;; Therefore, I am employing an alternate hack: I will identify construction geomnetry by seeing
					;; if the attached string is "construction". I will have to manually use my (setAttachedString ) 
					;; function to tag each object that I want to be construction (i.e. not descendable).
					;;(/= "construction" (getAttachedString thisItem))
					(not  (isBConstruction thisItem))
					
					(progn
						(write-line (strcat "descending into block reference " (vla-get-name thisItem)))
						(pushToNestingStack thisItem)
						(applyToAllEntitiesWithin 
							(vla-Item blocks (vla-get-name thisItem)) 
							functionToApply
						)
						(write-line (strcat "ascending out of block reference " (vla-get-name thisItem)))
						(popFromNestingStack)
					)
				)
			)
			(progn ;;in this case, this entity is something other than a block reference, which means we want to apply the function to it.
				(setq indexOfEntityWithinBlock (+ 1 indexOfEntityWithinBlock))
				;(princ "applying the function.\n")
				(apply functionToApply (list thisItem))
			)
		)
		(setq i (+ i 1))
	)
)



;(quit)




(defun pushToNestingStack
	(
		blockReference
		/
		
	)
	
	(setq currentTransform (mxm (getTransform blockReference) currentTransform))
	
	(setq nestingStack
		(cons
			(list 
				(vla-get-Handle blockReference) 
				currentTransform
			)
			nestingStack
		)
	)
	(setq currentBlockName 
		(vla-get-name
			(vlax-ename->vla-object 
				(handent 
					(nth 0 
						(car nestingStack)
					)
				)
			)
		)
	)
	
)

(defun popFromNestingStack
	(

		/
		returnValue
	)
	(setq returnValue (car nestingStack))
	(setq nestingStack (cdr nestingStack))
	(if (= 0 (vl-list-length nestingStack))
		(progn
			(setq currentTransform identityTransform)
			(setq currentBlockName "")			
		)
		(progn
			(setq currentTransform 
				(nth 1
					(car nestingStack)
				)
			)
			(setq currentBlockName 
				(vla-get-name
					(vlax-ename->vla-object 
						(handent 
							(nth 0 
								(car nestingStack)
							)
						)
					)
				)
			)
		)
	)
	returnValue
)



(defun getTransform 
	(
		blockReference
		/
		returnValue
		insertionPoint
	)
	;; For now, I am only doing a partial implementation - I am handling translations, but no scaling or rotating.
	
	
	(setq insertionPoint
		(vlax-safearray->list 
			(vlax-variant-value 
				(vla-get-InsertionPoint blockReference)
			)
		)
	)
	
	(setq returnValue
		(list
			(list 1 0 0 (nth 0 insertionPoint))
			(list 0 1 0 (nth 1 insertionPoint))
			(list 0 0 1 (nth 2 insertionPoint))
			(list 0 0 0 1)
		)
	)
	
	returnValue
	
)


(defun initializeNestingStack
	(
	/
	)
	(setq nestingStack (list )) ;; a list, each of whose elements is a  list, the first member of which is the handle of a block reference, and the second member is a transformation matrix)
	(setq identityTransform (imat 4))
	(setq currentTransform identityTransform)
	(setq currentBlockName "")
)














(defun initializeGlobalObjects
	(
		/
	)
	(setq acadObject (vlax-get-acad-object))
	(setq acadDocument (vla-get-ActiveDocument acadObject))
	(setq modelSpace (vla-get-ModelSpace acadDocument))
	(setq blocks (vla-get-Blocks acadDocument))
)

(defun getStartPoint
	(
		line
		/
		startPoint
	)
	(setq startPoint 
		(vlax-safearray->list 
			(vlax-variant-value 
				(vlax-get-property line 'StartPoint)
			)
		)
	)
	
	(setq startPoint 
		(append startPoint 
			'(1)
		)
	)
	
	(setq startPoint (mxv currentTransform startPoint))
	
	(setq startPoint 
		(list
			(nth 0 startPoint)
			(nth 1 startPoint)
			(nth 2 startPoint)
		)
	)
	
	startPoint
)








(defun annotateAllDoorAndWindowLines
	(
		
		/
		annotateLineWithIdentifier
		messageSuffix
	)
	(clearAnnotations)
	(initializeNestingStack)
	(setq annotateLineWithIdentifier

		'(lambda
			(
				x	
				/
				attachedString
				message
			)
			(setq attachedString (getAttachedString x))
			(if attachedString	
				(progn
					(setq message attachedString)
					; ; ; (setq door
						; ; ; (cdr (assoc attachedString doorData))
					; ; ; )
					; ; ; (if door
						; ; ; (progn
							; ; ; ; (setq message 
								; ; ; ; (strcat 
									; ; ; ; message
									; ; ; ; " type:" (cdr (assoc "type" door))
									; ; ; ; " widthOfOpening:" (cdr (assoc "widthOfOpening" door))
									; ; ; ; " height:" (cdr (assoc "height" door))
								; ; ; ; )
							; ; ; ; )
						; ; ; )
						; ; ; (setq message 
							; ; ; (strcat 
								; ; ; message
								; ; ; " -- no data found"
							; ; ; )
						; ; ; )
					; ; ; )
				)
				(progn
					;(setq message "NO IDENTIFIER!")
					;(setq message "N")
					(setq message ".")
				)
			)
			(setq message (strcat message messageSuffix))
			;;(annotateLine x message currentTransform)
			(annotateLine x message)
		)
	)





	(setq messageSuffix "")
	(princ "now annotating doorAnchor lines.\n")
	(applyToLinesWithin
		modelSpace
		"doorAnchor"
		annotateLineWithIdentifier
	)
	(applyToLinesWithin
		modelSpace
		"doorGuideNonPivotingSide"
		annotateLineWithIdentifier
	)
	(princ "now annotating windowAnchor lines.\n")
	(applyToLinesWithin
		modelSpace
		"windowAnchor"
		annotateLineWithIdentifier
	)
	(applyToLinesWithin
		modelSpace
		"windowGuide"
		annotateLineWithIdentifier
	)


	(setq messageSuffix " (level2)")
	(applyToLinesWithin
		modelSpace
		"level2-doorAnchor"
		annotateLineWithIdentifier
	)
	(applyToLinesWithin
		modelSpace
		"level2-doorGuideNonPivotingSide"
		annotateLineWithIdentifier
	)
	(applyToLinesWithin
		modelSpace
		"level2-windowAnchor"
		annotateLineWithIdentifier
	)
	(applyToLinesWithin
		modelSpace
		"level2-windowGuide"
		annotateLineWithIdentifier
	)


	(write-line "")
	(write-line "")
	(write-line "annotation finished.")

)



;; copies the attached string from source to destination, only 
;; if destination does not have any attached string to begin with.	
(defun c:COPY-ATTACHED-STRING
	(
	
	/
		enameOfSelectedEntity
		enameOfDestinationEntity
		transformOfDestinationEntity
		nestingPathOfdestinationEntity
		message
	)
	(setq enameOfSourceEntity
		(nth 0 (nentsel "Select the source entity."))
	)	
	
	(if (not (getAttachedString enameOfSourceEntity)) 
		(progn
			(princ "\n")
			(princ "The selected source entity does not have an attached string.")
			(quit)
		)
	)
	(princ "\n")
	(princ (strcat "Ready to copy """  (getAttachedString enameOfSourceEntity) """."))
	(princ "\n")
	(setq x (nentsel (strcat "Select the destination entity.")))
	(setq enameOfDestinationEntity
		(nth 0 x)
	)
	
	;; (setq transformOfDestinationEntity (nth 1 x))
	(setq nestingPathOfdestinationEntity (nth 3 x))
	
	
	(if (getAttachedString enameOfDestinationEntity)
		(progn
			(princ (strcat "The destination entity already has an attached string, namely "  (getAttachedString enameOfDestinationEntity) "."))
			(quit)
		)
	)
	
	;;if we made it this far, we can assume that the source entity has an attached string, the destination entity
	;; does not have an attached string, and we want to copy from source to destination.
	
	(setAttachedString enameOfDestinationEntity (getAttachedString enameOfSourceEntity))
	
	
	(initializeNestingStack)
	(foreach 
		item nestingPathOfdestinationEntity ;;the order in which I am traversing the nestingPath may be backwards -- for the current purpose it doesn't matter because all our transforms are merely translations, therefore commutative.
		(progn
			(pushToNestingStack (vlax-ename->vla-object item))
		)
	)
	
	(annotateLine 
		(vlax-ename->vla-object enameOfDestinationEntity) ;; line to annotate
		(getAttachedString enameOfDestinationEntity)      ;; message
		;;currentTransform                      ;; transform
	)
	(princ "\n")
	(princ "SUCCESS.\n")
	(princ)
)
;====


;; populateDoors
;;  This function creates a global variable doors, which is an associative 
;; array (keys are the door identifiers) whose elements are objects of type door
;; (okay, not true objects, but associative arrays with a uniform structure).
(defun populateDoors
	(
		/
		newDoors
		thisDoorIdentifier
		thisDoor
		newFields
	)
	
	;; Search the drawing for all doorAnchors (i.e. lines on the hinged side) 
	;; and doorGuides (i.e. lines on the unhinged side), and read each one's 
	;; attached string to see which door it belongs to.
	;; warn about any entry in doorData that does not have a corresponding 
	;; anchor line and guide line.
	
	(setq doors doorData) ;; we start with doorData.  doors will be an augmented version of doorData.
	(initializeNestingStack)
	(applyToAllLinesWithin
		modelSpace
		'(lambda 
			(thisLine)
			;(princ "got this far.\n")
			;(princ "layer type: " ) (princ (type (vla-get-layer thisLine)))
			;(princ (vla-get-layer thisLine))
			(if (member (vla-get-layer thisLine) (list "doorAnchor" "doorGuideNonPivotingSide" "level2-doorAnchor" "level2-doorGuideNonPivotingSide"))
				(progn
					(if (getAttachedString thisLine)
						(progn 
							(setq doorIdentifier (getAttachedString thisLine))
							(if (assoc doorIdentifier doors)
								(progn ;in this case, there was a matching entry in doors
									;;we wish to add some fields to the matching entry
									(setq thisDoor (cdr (assoc doorIdentifier doors))) ;;retrieve the exisiting entry
									;;modify thisDoor as required here
									
									(if (member (vla-get-layer thisLine) (list "doorAnchor" "level2-doorAnchor"))
										(progn
											;;check to see if thisDoor already has an anchor, and warn if it does.
											(if (or (assoc  "anchor" thisDoor) (assoc  "nestingPathOfAnchor" thisDoor))
												(princ 
													(strcat
														"warning from populateDoors: There is more than one anchor for " doorIdentifier "."
													)
												)
											)
											
											(setq newFields
												(list
													(cons "anchor" thisLine)
													(cons "nestingPathOfAnchor" nestingStack)
													(cons "transformOfAnchor" currentTransform)
												)
											)
										)
										(progn
											;; ought to check to see if thisDoor already has a guide, and warn if it does.
											(if (or (assoc "guide" thisDoor) (assoc  "nestingPathOfGuide" thisDoor))
												(princ 
													(strcat
														"warning from populateDoors: There is more than one guide for " doorIdentifier "."
													)
												)
											)
											(setq newFields
												(list
													(cons "guide" thisLine)
													(cons "nestingPathOfGuide" nestingStack)
													(cons "transformOfGuide" currentTransform)
												)
											)
										)
									)
									
									(setq thisDoor
										(append thisDoor newFields)
									)
									
									;;update doors with the modified entry
									(setq doors 
										(subst 
											(cons doorIdentifier thisDoor) 	   ;replacement
											(assoc doorIdentifier doors) 	   ;needle
											doors 				               ;haystack
										)
									)
								)
								(progn ; in this case, there was no matching entry in doors
									(princ 
										(strcat 
											"warning from populateDoors: a doorAnchor was found "
											"having doorIdentifier " doorIdentifier ", but no matching "
											"record could be found in doorData."
											"\n"
										)
									)
								)
							)
						)
						(progn ; in this case, doorAnchor did not have an attached string.
							(princ 
								(strcat 
									"warning from populateDoors: a doorAnchor was found "
									"that does not have a doorIdentifier."
									"\n"
								)
							)
						)
					)
				)
			)
		)
	)
	
	;; at this point, each door in doors will now have, in addition to its original contents: anchor, nestingPathOfAnchor, guide, and nestingPathOfGuide .
	;; We should check for and warn about any doors that are lacking any of these elements or have more than one of these elements.
	;; It might make sense to remove or flag such defective door entries so that we avoid attempting to place a 
	;; an icon for them later in the program.
	
	;; add the identifier as a property of the door.
	(setq newDoors (list ))
	
	(foreach item doors
		(progn	
			
			(setq thisDoorIdentifier (car item))
			(setq thisDoor (cdr item))
			
			(setq newFields
				(list
					(cons "identifier" thisDoorIdentifier)
				)
			)
			(setq thisDoor
				(append thisDoor newFields)
			)
			
			;;add the updated door to newDoors
			(setq newDoors 
				(append newDoors
					(list
						(cons thisDoorIdentifier thisDoor)
					)
				)
			)
		)
	)
	
	(setq doors newDoors)
	
	(if T ;nil
		(foreach item doors
			(progn	
				
				(setq thisDoorIdentifier (car item))
				(setq thisDoor (cdr item))
				
				(setq statusMessage (strcat thisDoorIdentifier ": "))
				
				;(setq thereIsAProblem nil)
				
				(if (assoc "anchor" thisDoor)
					(setq statusMessage (strcat statusMessage " has anchor,            "))
					(setq statusMessage (strcat statusMessage " DOES NOT HAVE ANCHOR,  "))
				)
					
				(if (assoc "guide" thisDoor)
					(setq statusMessage (strcat statusMessage " has guide              "))
					(setq statusMessage (strcat statusMessage " DOES NOT HAVE GUIDE    "))
				)
				
				(princ statusMessage) (princ "\n")
			)
		)
	)
)
;======



;; doorIsValid is a test that is used to skip the drawing of certain door icons.  This is
;; for doors that do not yet have both an anchor and a guide.
(defun doorIsValid
	(
		thisDoor
		/
		returnValue
	)
	
	;; the door must have both an anchor and a guide to be valid.
	(if (and (assoc "anchor" thisDoor) (assoc "guide" thisDoor))
		(setq returnValue T)
		(setq returnValue nil)
	)
	
	;; to be thorough, we shoudl probably check more things -- there are certainly other ways a door could be invalid 
	;; besides lacking an anchor or guide.  However, this simple test will suffice for my present purpose.
	
	returnValue
)

;=====

(defun drawDoorIcon
	(
		thisDoor
		/
		anchor  nestingPathOfAnchor  transformOfAnchor  anchorStartPoint  anchorEndPoint
		guide   nestingPathOfGuide   transformOfGuide   guideStartPoint   guideEndPoint
		unspecified
		widthOfOpening
		doorThickness
		doorHeight
		defaultDoorType
		defaultDoorThickness
		defaultDoorHeight
		iconBlockName
		iconBlockReference
		doorXHat
		doorYHat
		doorZHat
		arbitraryPointOnGuide
		q
		doorType
		doorIdentifier
		wallThickness
		defaultWallThickness
		yScaleFactor
		wallEdgeBlockReference
		defaultDoorOpenAngle
		doorOpenAngle
		defaultDoorOpenRatio
		doorOpenRatio
		wallTrimmerBlockName
		
		nameOfNonStandardParameter 
		namesOfNonStandardParameters
		thisValue
		defaultParameterValues
		
	)
	
	(setq wallTrimmerBlockName "wallOpeningEdge")
	
	;; the code to extract the anchor and guide, start points, and endpoints is quite ugly - I am really missing good associative arrays/objects.
	(if (assoc "anchor" thisDoor) ;(doorIsValid thisDoor)
		(progn
			(progn ; extracting info about the anchor
				(setq anchor 
					(cdr (assoc "anchor" thisDoor))
				)
				(setq nestingPathOfAnchor 
					(cdr (assoc "nestingPathOfAnchor" thisDoor))
				)
				(setq transformOfAnchor
					;; (nth 1 (car nestingPathOfAnchor))  
					;; above would of course not evaluate when we are at the root level, therefore I made populateDoors explcitly include the transform
					(cdr (assoc "transformOfAnchor" thisDoor))
				)
				(setq anchorStartPoint
					(mxv transformOfAnchor
						(append
							(vlax-safearray->list 
								(vlax-variant-value 
									(vlax-get-property anchor 'StartPoint)
								)
							)
							'(1)
						)
					)
				)
				(setq anchorStartPoint 
					(list
						(nth 0 anchorStartPoint)
						(nth 1 anchorStartPoint)
						(nth 2 anchorStartPoint)
					)
				)
				(setq anchorEndPoint
					(mxv transformOfAnchor
						(append
							(vlax-safearray->list 
								(vlax-variant-value 
									(vlax-get-property anchor 'EndPoint)
								)
							)
							'(1)
						)
					)
				)
				(setq anchorEndPoint 
					(list
						(nth 0 anchorEndPoint)
						(nth 1 anchorEndPoint)
						(nth 2 anchorEndPoint)
					)
				)
			)
			;==============

			
			
			(if (assoc "guide" thisDoor)
				(progn ; extracting info about the guide
					(setq guide 
						(cdr (assoc "guide" thisDoor))
					)
					(setq nestingPathOfGuide
						(cdr (assoc "nestingPathOfGuide" thisDoor))
					)
					(setq transformOfGuide
						;;(nth 1 (car nestingPathOfGuide))
						;; above would of course not evaluate when we are at the root level, therefore I made populateDoors explcitly include the transform
						(cdr (assoc "transformOfGuide" thisDoor))
					)
					(setq guideStartPoint
						(mxv transformOfGuide
							(append
								(vlax-safearray->list 
									(vlax-variant-value 
										(vlax-get-property guide 'StartPoint)
									)
								)
								'(1)
							)
						)
					)
					(setq guideStartPoint 
						(list
							(nth 0 guideStartPoint)
							(nth 1 guideStartPoint)
							(nth 2 guideStartPoint)
						)
					)
					(setq guideEndPoint
						(mxv transformOfAnchor
							(append
								(vlax-safearray->list 
									(vlax-variant-value 
										(vlax-get-property guide 'EndPoint)
									)
								)
								'(1)
							)
						)
					)
					(setq guideEndPoint 
						(list
							(nth 0 guideEndPoint)
							(nth 1 guideEndPoint)
							(nth 2 guideEndPoint)
						)
					)
				) 
			)
			;=============
			
			
			;; at this point, we have the start point and end point of both the door and the guide. (with coordinates in in the root context).
			
			(setq angleOfAnchor (vla-get-Angle anchor)) ;;this is a hack -- we really ought to transform the angle.  In our case, there is no rotation of block references, so we ca get away with it, still, this is sloppy.
			(setq midpointOfAnchor
				(list
					(/ (+ (nth 0 anchorStartPoint) (nth 0 anchorEndPoint)) 2.0)
					(/ (+ (nth 1 anchorStartPoint) (nth 1 anchorEndPoint)) 2.0)
					(/ (+ (nth 2 anchorStartPoint) (nth 2 anchorEndPoint)) 2.0)
				)
			) ;;sloppy and brute-force'ish, but it gets the job done.
			
			(setq anchorLength (vla-get-Length anchor)) ;;again, this is a hack for the sake of rapid development.  This only works properly if the transform did no scaling -- in our case, we can get away with it, but we really ought to transform the line, and then take its length.
			
			;; Remember, the widhtOfOpening field is a string that is to be evaluated 
			;; as a lisp expression right now, with all the variables (such as a anchorLength)
			;;	available in this scope.  This lets set the  widthOfOpening to something like 
			;; "anchorLength", and that will evaluate to the actual anchor length.
			
			;; compute parameters from doorData.
			(setq unspecified nil) ;; 'unspecified' is used as an expression in doorData which basically means "use the default value"
			
			(if nil (progn ;;deprecated hardcoded default parameter values.
				; (setq defaultDoorThickness 1.5)
				; (setq defaultDoorHeight 80)
				; (setq defaultDoorType "standard")
				; (setq defaultWallThickness 8.0)
				; (setq defaultDoorOpenAngle 90) ;;in degrees
				; (setq defaultDoorOpenRatio 0.8)
			))
			;==============
			

			;; all door icon block definitions are expected to have the following dynamic parameters.
			(setq namesOfStandardParameters 
				(list 
					"doorIdentifier" 
					"wallThickness"
				)
			)
			
			(setq doorIdentifier 
				(cdr (assoc "identifier" thisDoor))
			)
			
			(setq widthOfOpening 
				(eval
					(read
						(cdr (assoc "widthOfOpening" thisDoor))
					)
				)
			)
			(if (not widthOfOpening) (setq widthOfOpening anchorLength)) ;;default value
			
			(setq defaultParameterValues
				(list
					(cons "doorThickness"    1.5            )
					(cons "doorHeight"       80             )
					(cons "wallThickness"    8.0            )
					(cons "doorOpenAngle"    90             )
					(cons "doorOpenRatio"    0.8            )
					(cons "panel1OpenRatio"  0              ) ;; for slidingGlass door
					(cons "panel2OpenRatio"  0.7            ) ;; for slidingGlass door
					(cons "doorWidth"        widthOfOpening ) 
				)
			)

			
			
			(setq defaultDoorType "standard")
			(setq doorType
				(cdr (assoc "doorType" thisDoor))
			)
			(if (= "unspecified" doorType) (setq doorType defaultDoorType)) ;;default value

			;; whatever transform M is applied to the doorBlock, we want to have 
			;; (M * yHat) points from the pivot point away from the guide.
			;; pick any point on the guide and take the vector (call it q) that points from 
			;; the pivot to the point on the guide.  Look at the dot product between q and M*y: 
			;; if negative, then set YScale = 1, if positive set yScale = -1;
			
			(setq doorXHat ;;doorXHat is the unit vector that points from anchorStartPoint to anchorEndPoint 
				(unit
					(list
						(- (nth 0 anchorEndPoint) (nth 0 anchorStartPoint))
						(- (nth 1 anchorEndPoint) (nth 1 anchorStartPoint))
						(- (nth 2 anchorEndPoint) (nth 2 anchorStartPoint))
					)
				) ;;sloppy and brute-force'ish, but it gets the job done.
			)
			
			;(setq doorZHat ...) ;;for now, I am jsut hardcodng a literal, assuming that doorZhat is always the same as the world zHat.
			
			(setq doorYHat
				(mxv 
					(list
						(list	0  -1  0)
						(list	1  0   0)
						(list	0  0   1)
					) ;;<<rotator that rotates +90 degrees about zHat>>
					doorXHat
				)
			)
			
			
			; (princ "doorXHat: ") (princ doorXHat) (princ "\n")
			; (princ "doorYHat: ") (princ doorYHat) (princ "\n")
			
			;; we are allowing for the absence of a guide, in which case we
			;;  will adopt reasonable defaults for wall thickness and chirality
			;; this allows us to get some workable output in the testing phase.
			(if (assoc "guide" thisDoor) 
				(progn
					(setq arbitraryPointOnGuide guideStartPoint)
					(setq q ;; q is a vector that points from the anchorStartPoint to an arbitrary point on the guide.
						(list
							(- (nth 0 arbitraryPointOnGuide) (nth 0 anchorStartPoint))
							(- (nth 1 arbitraryPointOnGuide) (nth 1 anchorStartPoint))
							(- (nth 2 arbitraryPointOnGuide) (nth 2 anchorStartPoint))
						)
					)
				)
				(progn
					;;in this case, there is no guide, so we will construct the q that we
					;; would have had if there had been a guide a distance (cdr (assoc "wallThickness" defaultParameterValues ))
					;; away from the anchor in the negative doorYHat direction.
					(setq q ;; q is a vector that points from the anchorStartPoint to an arbitrary point on the guide.
						(vxs doorYHat (- (cdr (assoc "wallThickness" defaultParameterValues ))))
					)
				)
			)
		

			(setq wallThickness (abs (vxv q doorYHat))) ;;(vxv ) is dot product.
			
			;;if q <dot> doorYHat is positive, flip the block reference about doorXHat (i.e. set yScale to -1)
			(if (> (vxv q doorYHat)  0)
				(progn
					;(princ "flipping the doorBlockReference about its x axis")
					(setq yScaleFactor -1.0)
					;(vlax-put-property iconBlockReference 'YScaleFactor -1.0) 
				)
				(progn
					;(princ "flipping the doorBlockReference about its x axis")
					(setq yScaleFactor 1.0)
				)
			)
			
						;; insert a door block
			
			;; allow explicit chirality specification, if present, and if set to 
			;; one of ("left", "right") to override the yScale factor
			;; computed above.  Chirality only has  relevance for standard single doors.
			;; left chirality is the default (i.e. yScaleFactor=1).
			;; right chirality means yScaleFactor=-1.
			;; the chirality is the side that the hinge is on as you walk through the door going 
			;; from the non hinged to the hinged side.
			;; Technically, the chirality can be fully encoded in the choice of which of the two ends of the anchor
			;; we call the start point.  However, in practice, it is useful to be able to declare
			;; the chirality in a more explicit way. (because it is inconvenient, when working in the autocad
			;; drawing to check which end of the anchor is the stratPoint, and it is also inconvenient to mdofy 
			;; which end is startPoint.)
			(setq insertionAngle angleOfAnchor) ;;this is the default and will only be changed if needed to account for an explicit chirality specification.
			(if (assoc "chirality" thisDoor) 
				(progn
					(setq newYScaleFactor nil)
					(COND
						(
							(= (cdr (assoc "chirality" thisDoor)) "left" )
							(setq newYScaleFactor 1.0)
						)
						(
							(= (cdr (assoc "chirality" thisDoor)) "right" )
							(setq newYScaleFactor -1.0)
						)
						(
							(= (cdr (assoc "chirality" thisDoor)) "unspecified" )
							;;DO NOTHING HERE
						)
						(
							T
							(progn
								(princ (strcat "warning: Ambiguous chirality value " """" ))
								(princ (cdr (assoc "chirality" thisDoor)))
								(princ (strcat """" " encountered while processing " doorIdentifier ".  We will ignore this chirality property.\n") )
							)
						)
					;;note: all other values for chirality, besides "lfet" and "right"
					;; will be ignored, as if the chirality setting were not present.
					)
					(if newYScaleFactor
						(progn
							(if (/= newYScaleFactor yScaleFactor)
								(progn
									(setq insertionAngle (+ insertionAngle PI)) ;;add 180 degrees to insertion angle
									(setq yScaleFactor newYScaleFactor) ;;update the yScale factor
								)
							)
						)
					)	
				)
			)
			;; I do not deny that the above handling of chirality is  bit tortured (having to change both the insertionAngle and the yScale factor), but it gets the job done.
			
			
			;; choose which block to insert based on the door's "type" field.
			
			
			(setq iconBlockName (strcat "door" "_" doorType))
			
			;; the following conditional renaming of iconBlockName gets around 
			;; an apparent bug, which I encountered, where, if you make
			;; repeated calls to modelSpace.InsertBlock() with the same 
			;; blockName, and that blockNAme is a file path, and the block
			;; in question is a dynamic block, then only the last blockReference created 
			;; is dynamic -- it is as if each call to InsertBlock makes every previous 
			;; reference to that block static before performing the insertion.
			;;Curiously, when I attempt to do the same thing using the autocad command line 
			;; (the "INSERT" command), I do not have this problem.  However, when doing this
			;; with the "INSERT" command, every insertion of a given pathname-block after the first
			;; causes a warning/error dialog to pop up with two options: Cancel, or "Redefine" the block; this same error/warning does not appear in any guise when using the InsertBlock() method.
			;;The fix that follows is to only insert the block from the path name on the first insertion, and thereafter 
			;; use the exisiting block defintion in the current drawing file.
			;; What a JOKE! -- From this and other experiences, I have the distinct impression
			;; that the Autocad features
			;;		{
			;;			1.  Dynamic constraints and dynamic blocks 
			;;			2.  text fields
			;;			3.  annotative scales / annotation contexts
			;;		}
			;; are all half-baked features that were bolted 
			;; onto the side of the core Autocad engine (likely by someone other than 
			;; the orginal creator of Autocad), and that none of these features is really ready for
			;; prime time.  Pity.
			
			(if (not (member iconBlockName (getAllBlockNames))) 
				(setq iconBlockName (blockNameToBlockFilePath iconBlockName))
			)
			(princ "now inserting ") (princ iconBlockName) (princ "\n")
			(setq iconBlockReference
				(vla-InsertBlock modelSpace 
					(vlax-3D-point midpointOfAnchor)     ;;insertion point
					iconBlockName   ;;name of block to insert
					1 ;;Xscale ;;later, we will be manipulate the scale factors based on the chirality of the door (as determined by the position of the guide relative to the anchor.)
					yScaleFactor ;;Yscale
					1 ;;Zscale	
					insertionAngle ;; rotation angle.
				) 
			)
			(vlax-put-property iconBlockReference 'Layer "door") 
			

			;; the standard parameters
			(LM:setdynpropvalue iconBlockReference "doorIdentifier" doorIdentifier	)
			(LM:setdynpropvalue iconBlockReference "wallThickness" wallThickness	)
			
			
			;;set any nonstandard parameters that the door icon block definition might have.
			(setq namesOfNonStandardParameters 
				(vl-remove-if
					'(lambda (x) (member x namesOfStandardParameters))
					(mapcar 'car (LM:getdynprops iconBlockReference))
				)
			)
			
			(foreach nameOfNonStandardParameter namesOfNonStandardParameters
				(setq thisValue nil)
				;; attempt to retrieve thisValue from the doorDatum.
				;; This retrieval attempt will result in thisValue being nil either if this doorDatum does not have an entry with the key nameOfNonStandardParameter
				;; or if the doorDatum does have such an entry, but the value evaluates to nil (as would happen happen if the value were "unspecified")
				(if (assoc nameOfNonStandardParameter thisDoor) ; if this doorDatum has this non standard parameter...
					(progn
						(setq thisValue
							(eval
								(read
									(cdr (assoc nameOfNonStandardParameter thisDoor))
								)
							)
						)
					)
				)
				
				(if (not thisValue) ; if thisValue is still nill, attempt to retrieve it from the defaults defined above
					(progn
						(setq thisValue
							(cdr (assoc nameOfNonStandardParameter defaultParameterValues)) ;;this returns nil if the specified parameter does not exist
						)
					)
				)
				
				;; if thisValue is still nil, we simply won't set the parameter in the dyamic block, which will therefore revert to the default defined in the block definition.
				(if thisValue ;;if we were able to succesfully find thisValue...
					(progn
						(LM:setdynpropvalue iconBlockReference nameOfNonStandardParameter thisValue)
					)
				)
				
			)
			
			(if nil (progn ;;deprecated hard-coded setting of nonstandard parameters
				(if (assoc "doorThickness" (LM:getdynprops iconBlockReference))
					(progn
						(setq doorThickness nil)
						(if (assoc "doorThickness" thisDoor)
							(setq doorThickness 
								(eval
									(read
										(cdr (assoc "doorThickness" thisDoor))
									)
								)
							)
						)
						(if (not doorThickness) (setq doorThickness (cdr (assoc "doorThickness" defaultParameterValues )))) ;;default value
						(LM:setdynpropvalue iconBlockReference "doorThickness" doorThickness)
					)
				)
				
				(if (assoc "doorOpenAngle" (LM:getdynprops iconBlockReference))
					(progn
						(setq doorOpenAngle nil)
						(if (assoc "doorOpenAngle" thisDoor)
							(setq doorOpenAngle 
								(eval
									(read
										(cdr (assoc "doorOpenAngle" thisDoor))
									)
								)
							)
						)
						(if (not doorOpenAngle) (setq doorOpenAngle (cdr (assoc "doorOpenAngle" defaultParameterValues )))) ;;default value
						(LM:setdynpropvalue iconBlockReference "doorOpenAngle" (* doorOpenAngle (/ PI 180.0)))
					)
				)
				
				
				(if (assoc "doorOpenRatio" (LM:getdynprops iconBlockReference))
					(progn
						(setq doorOpenRatio nil)
						(if (assoc "doorOpenRatio" thisDoor)
							(setq doorOpenRatio 
								(eval
									(read
										(cdr (assoc "doorOpenRatio" thisDoor))
									)
								)
							)
						)
						(if (not doorOpenRatio) (setq doorOpenRatio (cdr (assoc "doorOpenRatio" defaultParameterValues )))) ;;default value
						
						(LM:setdynpropvalue iconBlockReference "doorOpenRatio" doorOpenRatio)
					)
				)
			))
			;;==============
			

			(if T ;;nil
				(progn 
					(if (not (member wallTrimmerBlockName (getAllBlockNames))) 
						(setq wallTrimmerBlockName (blockNameToBlockFilePath wallTrimmerBlockName))
					)
					(princ "now inserting ") (princ wallTrimmerBlockName) (princ "\n")
					(setq wallEdgeBlockReference
						(vla-InsertBlock modelSpace 
							(vlax-3D-point midpointOfAnchor)     ;;insertion point
							wallTrimmerBlockName   ;;name of block to insert
							1 ;;Xscale ;;later, we will be manipulate the scale factors based on the chirality of the door (as determined by the position of the guide relative to the anchor.)
							yScaleFactor ;;Yscale
							1 ;;Zscale	
							insertionAngle ;; rotation angle.
						) 
					)
					(vlax-put-property wallEdgeBlockReference 'Layer "wall") 
					(LM:setdynpropvalue wallEdgeBlockReference "wallThickness" wallThickness	)
					(LM:setdynpropvalue wallEdgeBlockReference "widthOfOpening" widthOfOpening )
					(LM:setdynpropvalue wallEdgeBlockReference "identifier" doorIdentifier	)	

					;;(vla-Explode wallEdgeBlockReference) 
					;;well, that's stupid: it looks like when I explode a dynamic block using the 
					;;block reference object's Explode method, the particular dynamic values are not
					;;transferred to the explodded objects. STUPID!!.
					
					;;Fortunately, issuing "EXPLODE" on the command line does work as desired.  So I need to
					;;figure out a reliable way of invoking the command-line version of explode, and making sure I can explode the 
					;; correct object.  -- As long as noun-verb command flow is enabled (which it is by default),
					;; I should be able to programmatically select the block reference.  Then, when I issue "EXPLODE" to the command line,
					;; I will be sure that I am exploding the right block reference.
					;;(vla-Delete wallEdgeBlockReference)
				)
			)
		)
		(progn ;;in this case, the door was invalid and will be skipped
			;(princ "Skipping an invalid door...") (princ "\n")
			(princ "Skipping door that does not have an anchor...") (princ "\n")
		)
	)
)
;======




;; populateWindows
;;  This function creates a global variable windows, which is an associative 
;; array (keys are the window identifiers) whose elements are objects of type window
;; (okay, not true objects, but associative arrays with a uniform structure).
(defun populateWindows
	(
		/
		newWindows
		thisWindowIdentifier
		thisWindow
		newFields
	)
	
	;; Search the drawing for all windowAnchors 
	;; and windowGuides , and read each one's 
	;; attached string to see which window it belongs to.
	;; warn about any entry in windowData that does not have a corresponding 
	;; anchor line and guide line.
	
	(setq windows windowData) ;; we start with windowData.  windows will be an augmented version of windowData.
	(initializeNestingStack)
	(applyToAllLinesWithin
		modelSpace
		'(lambda 
			(thisLine)
			;(princ "got this far.\n")
			;(princ "layer type: " ) (princ (type (vla-get-layer thisLine)))
			;(princ (vla-get-layer thisLine))
			(if (member (vla-get-layer thisLine) (list "windowAnchor" "windowGuide" "level2-windowAnchor" "level2-windowGuide"))
				(progn
					(if (getAttachedString thisLine)
						(progn 
							(setq windowIdentifier (getAttachedString thisLine))
							(if (assoc windowIdentifier windows)
								(progn ;in this case, there was a matching entry in windows
									;;we wish to add some fields to the matching entry
									(setq thisWindow (cdr (assoc windowIdentifier windows))) ;;retrieve the exisiting entry
									;;modify thisWindow as required here
									
									(if (member (vla-get-layer thisLine) (list "windowAnchor" "level2-windowAnchor"))
										(progn
											;;check to see if thisWindow already has an anchor, and warn if it does.
											(if (or (assoc  "anchor" thisWindow) (assoc  "nestingPathOfAnchor" thisWindow))
												(princ 
													(strcat
														"warning from populateWindows: There is more than one anchor for " windowIdentifier "."
													)
												)
											)
											
											(setq newFields
												(list
													(cons "anchor" thisLine)
													(cons "nestingPathOfAnchor" nestingStack)
													(cons "transformOfAnchor" currentTransform)
												)
											)
										)
										(progn
											;; ought to check to see if thisWindow already has a guide, and warn if it does.
											(if (or (assoc "guide" thisWindow) (assoc  "nestingPathOfGuide" thisWindow))
												(princ 
													(strcat
														"warning from populateWindows: There is more than one guide for " windowIdentifier "."
													)
												)
											)
											(setq newFields
												(list
													(cons "guide" thisLine)
													(cons "nestingPathOfGuide" nestingStack)
													(cons "transformOfGuide" currentTransform)
												)
											)
										)
									)
									
									(setq thisWindow
										(append thisWindow newFields)
									)
									
									;;update windows with the modified entry
									(setq windows 
										(subst 
											(cons windowIdentifier thisWindow) 	   ;replacement
											(assoc windowIdentifier windows) 	   ;needle
											windows 				               ;haystack
										)
									)
								)
								(progn ; in this case, there was no matching entry in windows
									(princ 
										(strcat 
											"warning from populateWindows: a windowAnchor was found "
											"having windowIdentifier " windowIdentifier ", but no matching "
											"record could be found in windowData."
											"\n"
										)
									)
								)
							)
						)
						(progn ; in this case, windowAnchor did not have an attached string.
							(princ 
								(strcat 
									"warning from populateWindows: a windowAnchor was found "
									"that does not have a windowIdentifier."
									"\n"
								)
							)
						)
					)
				)
			)
		)
	)
	
	;; at this point, each window in windows will now have, in addition to its original contents: anchor, nestingPathOfAnchor, guide, and nestingPathOfGuide .
	;; We should check for and warn about any windows that are lacking any of these elements or have more than one of these elements.
	;; It might make sense to remove or flag such defective window entries so that we avoid attempting to place a 
	;; an icon for them later in the program.
	
	;; add the identifier as a property of the window.
	(setq newWindows (list ))
	
	(foreach item windows
		(progn	
			
			(setq thisWindowIdentifier (car item))
			(setq thisWindow (cdr item))
			
			(setq newFields
				(list
					(cons "identifier" thisWindowIdentifier)
				)
			)
			(setq thisWindow
				(append thisWindow newFields)
			)
			
			;;add the updated window to newWindows
			(setq newWindows 
				(append newWindows
					(list
						(cons thisWindowIdentifier thisWindow)
					)
				)
			)
		)
	)
	
	(setq windows newWindows)
	
	(if T ;nil
		(foreach item windows
			(progn	
				
				(setq thisWindowIdentifier (car item))
				(setq thisWindow (cdr item))
				
				(setq statusMessage (strcat thisWindowIdentifier ": "))
				
				;(setq thereIsAProblem nil)
				
				(if (assoc "anchor" thisWindow)
					(setq statusMessage (strcat statusMessage " has anchor,            "))
					(setq statusMessage (strcat statusMessage " DOES NOT HAVE ANCHOR,  "))
				)
					
				(if (assoc "guide" thisWindow)
					(setq statusMessage (strcat statusMessage " has guide              "))
					(setq statusMessage (strcat statusMessage " DOES NOT HAVE GUIDE    "))
				)
				
				(princ statusMessage) (princ "\n")
			)
		)
	)
)
;======


(defun drawWindowIcon
	(
		thisWindow
		/
		anchor  nestingPathOfAnchor  transformOfAnchor  anchorStartPoint  anchorEndPoint
		guide   nestingPathOfGuide   transformOfGuide   guideStartPoint   guideEndPoint
		unspecified
		widthOfOpening
		windowVerticalExtent
		sillHeight
		defaultWindowType
		defaultWindowVerticalExtent
		defaultSillHeight
		iconBlockName
		iconBlockReference
		windowXHat
		windowYHat
		windowZHat
		arbitraryPointOnGuide
		q
		windowType
		windowIdentifier
		wallThickness
		defaultWallThickness
		yScaleFactor
		insertionAngle
		wallTrimmerBlockName
	)
	
	(setq wallTrimmerBlockName "wallOpeningEdge")
	;; the code to extract the anchor and guide, start points, and endpoints is quite ugly - I am really missing good associative arrays/objects.
	(if (assoc "anchor" thisWindow) 
		(progn
			(progn ; extracting info about the anchor
				(setq anchor 
					(cdr (assoc "anchor" thisWindow))
				)
				(setq nestingPathOfAnchor 
					(cdr (assoc "nestingPathOfAnchor" thisWindow))
				)
				(setq transformOfAnchor
					;;(nth 1 (car nestingPathOfAnchor))
					;; above would of course not evaluate when we are at the root level, therefore I made populateWindows explcitly include the transform
					(cdr (assoc "transformOfAnchor" thisWindow))
				)
				(setq anchorStartPoint
					(mxv transformOfAnchor
						(append
							(vlax-safearray->list 
								(vlax-variant-value 
									(vlax-get-property anchor 'StartPoint)
								)
							)
							'(1)
						)
					)
				)
				(setq anchorStartPoint 
					(list
						(nth 0 anchorStartPoint)
						(nth 1 anchorStartPoint)
						(nth 2 anchorStartPoint)
					)
				)
				(setq anchorEndPoint
					(mxv transformOfAnchor
						(append
							(vlax-safearray->list 
								(vlax-variant-value 
									(vlax-get-property anchor 'EndPoint)
								)
							)
							'(1)
						)
					)
				)
				(setq anchorEndPoint 
					(list
						(nth 0 anchorEndPoint)
						(nth 1 anchorEndPoint)
						(nth 2 anchorEndPoint)
					)
				)
			)
			;==============

			
			
			(if (assoc "guide" thisWindow)
				(progn ; extracting info about the guide
					(setq guide 
						(cdr (assoc "guide" thisWindow))
					)
					(setq nestingPathOfGuide
						(cdr (assoc "nestingPathOfGuide" thisWindow))
					)
					(setq transformOfGuide
						;;(nth 1 (car nestingPathOfGuide))
						;; above would of course not evaluate when we are at the root level, therefore I made populateWindows explcitly include the transform
						(cdr (assoc "transformOfGuide" thisWindow))
					)
					(setq guideStartPoint
						(mxv transformOfGuide
							(append
								(vlax-safearray->list 
									(vlax-variant-value 
										(vlax-get-property guide 'StartPoint)
									)
								)
								'(1)
							)
						)
					)
					(setq guideStartPoint 
						(list
							(nth 0 guideStartPoint)
							(nth 1 guideStartPoint)
							(nth 2 guideStartPoint)
						)
					)
					(setq guideEndPoint
						(mxv transformOfAnchor
							(append
								(vlax-safearray->list 
									(vlax-variant-value 
										(vlax-get-property guide 'EndPoint)
									)
								)
								'(1)
							)
						)
					)
					(setq guideEndPoint 
						(list
							(nth 0 guideEndPoint)
							(nth 1 guideEndPoint)
							(nth 2 guideEndPoint)
						)
					)
				) 
			)
			;=============
			
			
			;; at this point, we have the start point and end point of both the window and the guide. (with coordinates in in the root context).
			
			(setq angleOfAnchor (vla-get-Angle anchor)) ;;this is a hack -- we really ought to transform the angle.  In our case, there is no rotation of block references, so we ca get away with it, still, this is sloppy.
			(setq midpointOfAnchor
				(list
					(/ (+ (nth 0 anchorStartPoint) (nth 0 anchorEndPoint)) 2.0)
					(/ (+ (nth 1 anchorStartPoint) (nth 1 anchorEndPoint)) 2.0)
					(/ (+ (nth 2 anchorStartPoint) (nth 2 anchorEndPoint)) 2.0)
				)
			) ;;sloppy and brute-force'ish, but it gets the job done.
			
			(setq anchorLength (vla-get-Length anchor)) ;;again, this is a hack for the sake of rapid development.  This only works properly if the transform did no scaling -- in our case, we can get away with it, but we really ought to transform the line, and then take its length.
			
			;; Remember, the widhtOfOpening field is a string that is to be evaluated 
			;; as a lisp expression right now, with all the variables (such as a anchorLength)
			;;	available in this scope.  This lets set the  widthOfOpening to something like 
			;; "anchorLength", and that will evaluate to the actual anchor length.
			
			;; compute parameters from windowData.
			(setq unspecified nil) ;; 'unspecified' is used as an expression in windowData which basically means "use the default value"
			(setq defaultWindowVerticalExtent 45)
			(setq defaultSillHeight 25)
			(setq defaultWindowType "standard")
			(setq defaultWallThickness 8.0)
			
			(setq windowIdentifier 
				(cdr (assoc "identifier" thisWindow))
			)
			
			(setq widthOfOpening 
				(eval
					(read
						(cdr (assoc "widthOfOpening" thisWindow))
					)
				)
			)
			(if (not widthOfOpening) (setq widthOfOpening anchorLength)) ;;default value
			
			(setq windowType
				(cdr (assoc "windowType" thisWindow))
			)
			(if (or (not windowType) (= "unspecified" windowType)) 
				(setq windowType defaultWindowType) ;;default value
			) 
			
			(setq windowVerticalExtent 
				(eval
					(read
						(cdr (assoc "windowVerticalExtent" thisWindow))
					)
				)
			)
			(if (not windowVerticalExtent) (setq windowVerticalExtent defaultWindowVerticalExtent)) ;;default value
			
			(setq sillHeight 
				(eval
					(read
						(cdr (assoc "sillHeight" thisWindow))
					)
				)
			)
			(if (not sillHeight) (setq sillHeight defaultSillHeight)) ;;default value
			
			
			
			;; insert a window block
			
			;; choose which block to insert based on the window's "type" field.
			
			
			(setq iconBlockName (strcat "window" "_" windowType))
			

			
			;; whatever transform M is applied to the windowBlock, we want to have 
			;; (M * yHat) points from the pivot point away from the guide.
			;; pick any point on the guide and take the vector (call it q) that points from 
			;; the pivot to the point on the guide.  Look at the dot product between q and M*y: 
			;; if negative, then set YScale = 1, if positive set yScale = -1;
			
			(setq windowXHat ;;windowXHat is the unit vector that points from anchorStartPoint to anchorEndPoint 
				(unit
					(list
						(- (nth 0 anchorEndPoint) (nth 0 anchorStartPoint))
						(- (nth 1 anchorEndPoint) (nth 1 anchorStartPoint))
						(- (nth 2 anchorEndPoint) (nth 2 anchorStartPoint))
					)
				) ;;sloppy and brute-force'ish, but it gets the job done.
			)
			
			;(setq windowZHat ...) ;;for now, I am jsut hardcodng a literal, assuming that windowZhat is always the same as the world zHat.
			
			(setq windowYHat
				(mxv 
					(list
						(list	0  -1  0)
						(list	1  0   0)
						(list	0  0   1)
					) ;;<<rotator that rotates +90 degrees about zHat>>
					windowXHat
				)
			)
			
			
			; (princ "windowXHat: ") (princ windowXHat) (princ "\n")
			; (princ "windowYHat: ") (princ windowYHat) (princ "\n")
			
			;; we are allowing for the absence of a guide, in which case we
			;;  will adopt reasonable defaults for wall thickness
			;; this allows us to get some workable output in the testing phase.
			(if (assoc "guide" thisWindow) 
				(progn
					(setq arbitraryPointOnGuide guideStartPoint)
					(setq q ;; q is a vector that points from the anchorStartPoint to an arbitrary point on the guide.
						(list
							(- (nth 0 arbitraryPointOnGuide) (nth 0 anchorStartPoint))
							(- (nth 1 arbitraryPointOnGuide) (nth 1 anchorStartPoint))
							(- (nth 2 arbitraryPointOnGuide) (nth 2 anchorStartPoint))
						)
					)
				)
				(progn
					;;in this case, there is no guide, so we will construct the q that we
					;; would have had if there had been a guide a distance defaultWallThickness
					;; away from the anchor in the negative windowYHat direction.
					(setq q ;; q is a vector that points from the anchorStartPoint to an arbitrary point on the guide.
						(vxs windowYHat (- defaultWallThickness))
					)
				)
			)
		

			(setq wallThickness (abs (vxv q windowYHat))) ;;(vxv ) is dot product.
			
			;;if q <dot> windowYHat is positive, flip the block reference about windowXHat (i.e. set yScale to -1)
			(if (> (vxv q windowYHat)  0)
				(setq yScaleFactor -1.0)
				(setq yScaleFactor 1.0)
			)
			
			(setq insertionAngle angleOfAnchor)
			
			;;the chirality stuff does not really apply to windows, it is here vestigially when this code was copied (yuck - hack in the sake of rapid development) from the door section.  Nevertheless, chirality does not hurt for windows in the current project.
			(if (not (member iconBlockName (getAllBlockNames))) 
				(setq iconBlockName (blockNameToBlockFilePath iconBlockName))
			)
			(princ "now inserting ") (princ iconBlockName) (princ "\n")
			(setq iconBlockReference
				(vla-InsertBlock modelSpace 
					(vlax-3D-point midpointOfAnchor)     ;;insertion point
					iconBlockName   ;;name of block to insert
					1 ;;Xscale ;;later, we will be manipulate the scale factors based on the chirality of the door (as determined by the position of the guide relative to the anchor.)
					yScaleFactor ;;Yscale
					1 ;;Zscale	
					insertionAngle ;; rotation angle.
				) 
			)
			(vlax-put-property iconBlockReference 'Layer "window") 
			; (vlax-put-property iconBlockReference 'Rotation angleOfAnchor) 
			
			; ; ; ; (if (= (LM:getdynpropvalue thisWindowBlock "chirality") "leftHanded")
				; ; ; ; (vlax-put-property thisWindowBlock 'XScaleFactor  -1.0) 
				; ; ; ; (vlax-put-property thisWindowBlock 'XScaleFactor  1.0) 
			; ; ; ; )
			
			
			(LM:setdynpropvalue iconBlockReference "sillHeight" sillHeight )
			(LM:setdynpropvalue iconBlockReference "wallThickness" wallThickness	)
			(LM:setdynpropvalue iconBlockReference "windowIdentifier" windowIdentifier	)
			(LM:setdynpropvalue iconBlockReference "windowVerticalExtent" windowVerticalExtent )
			(LM:setdynpropvalue iconBlockReference "windowWidth" widthOfOpening )
			
			(if T ;;nil
				(progn 
					(if (not (member wallTrimmerBlockName (getAllBlockNames))) 
						(setq wallTrimmerBlockName (blockNameToBlockFilePath wallTrimmerBlockName))
					)
					(princ "now inserting ") (princ wallTrimmerBlockName) (princ "\n")
					(setq wallEdgeBlockReference
						(vla-InsertBlock modelSpace 
							(vlax-3D-point midpointOfAnchor)     ;;insertion point
							wallTrimmerBlockName   ;;name of block to insert
							1 ;;Xscale ;;later, we will be manipulate the scale factors based on the chirality of the door (as determined by the position of the guide relative to the anchor.)
							yScaleFactor ;;Yscale
							1 ;;Zscale	
							insertionAngle ;; rotation angle.
						) 
					)
					(vlax-put-property wallEdgeBlockReference 'Layer "wall") 
					(LM:setdynpropvalue wallEdgeBlockReference "wallThickness" wallThickness	)
					(LM:setdynpropvalue wallEdgeBlockReference "widthOfOpening" widthOfOpening )
					(LM:setdynpropvalue wallEdgeBlockReference "identifier" windowIdentifier	)	

					;;(vla-Explode wallEdgeBlockReference) 
					;;well, that's stupid: it looks like when I explode a dynamic block using the 
					;;block reference object's Explode method, the particular dynamic values are not
					;;transferred to the explodded objects. STUPID!!.
					
					;;Fortunately, issuing "EXPLODE" on the command line does work as desired.  So I need to
					;;figure out a reliable way of invoking the command-line version of explode, and making sure I can explode the 
					;; correct object.  -- As long as noun-verb command flow is enabled (which it is by default),
					;; I should be able to programmatically select the block reference.  Then, when I issue "EXPLODE" to the command line,
					;; I will be sure that I am exploding the right block reference.
					;;(vla-Delete wallEdgeBlockReference)
				)
			)
		)
		(progn ;;in this case, the window was invalid and will be skipped
			;(princ "Skipping an invalid window...") (princ "\n")
			(princ "Skipping window that does not have an anchor...") (princ "\n")
		)
	)
)
;======



;; populateRooms
;;  This function creates a global variable rooms, which is an associative 
;; array (keys are the identifiers) whose elements are objects of type room
;; (okay, not true objects, but associative arrays with a uniform structure).
(defun populateRooms
	(
		/
		newRooms
		thisIdentifier
		thisRoom
		newFields
	)
	
	;; Search the drawing for all room centroid points (i.e. points on the 'roomCentridPoint') 
	;; and roomGuides (i.e. lines on the unhinged side), and read each one's 
	;; attached string to see which room it belongs to.
	;; warn about any entry in roomData that does not have a corresponding 
	;; anchor line and guide line.
	
	(setq rooms roomData) ;; we start with roomData.  rooms will be an augmented version of roomData.
	(initializeNestingStack)
	(applyToAllEntitiesWithin
		modelSpace
		'(lambda 
			(thisEntity)
			(if 
				(and
					(= "roomCentroidPoint" (vla-get-Layer thisEntity))
					(= "AcDbPoint" (vla-get-ObjectName thisEntity))
				)
				(progn
					(if (getAttachedString thisEntity)
						(progn 
							(setq roomIdentifier (getAttachedString thisEntity))
							(if (assoc roomIdentifier rooms)
								(progn ;in this case, there was a matching entry in rooms
									;;we wish to add some fields to the matching entry
									(setq thisRoom (cdr (assoc roomIdentifier rooms))) ;;retrieve the exisiting entry
									;;modify thisRoom as required here
									
	
									;;check to see if thisRoom already has an centroidPoint, and warn if it does.
									(if (assoc  "centroidPoint" thisRoom) 
										(princ 
											(strcat
												"warning from populateRooms: There is more than one centroid point for " roomIdentifier "."
											)
										)
									)
									
									(setq newFields
										(list
											(cons "centroidPoint" thisEntity)
											(cons "nestingPathOfCentroidPoint" nestingStack)
											(cons "transformOfCentroidPoint" currentTransform)
										)
									)
										

									(setq thisRoom
										(append thisRoom newFields)
									)
									
									;;update rooms with the modified entry
									(setq rooms 
										(subst 
											(cons roomIdentifier thisRoom) 	   ;replacement
											(assoc roomIdentifier rooms) 	   ;needle
											rooms 				               ;haystack
										)
									)
								)
								(progn ; in this case, there was no matching entry in rooms
									(princ 
										(strcat 
											"warning from populateRooms: a roomCentroidPoint was found "
											"having identifier " roomIdentifier ", but no matching "
											"record could be found in roomData."
											"\n"
										)
									)
								)
							)
						)
						(progn ; in this case, roomAnchor did not have an attached string.
							(princ 
								(strcat 
									"warning from populateRooms: a roomCentroid point was found "
									"that does not have a roomIdentifier."
									"\n"
								)
							)
						)
					)
				)
			)
		)
	)
	
	;; at this point, each room in rooms will now have, in addition to its original contents: centroidPoint, nestingPathOfcentroidPoint, transformOfCentroidPoint .
	;; We should check for and warn about any rooms that are lacking any of these elements or have more than one of these elements.
	;; It might make sense to remove or flag such defective room entries so that we avoid attempting to place a 
	;; an icon for them later in the program.
	
	;; add the identifier as a property of the room.
	(setq newRooms (list ))
	
	(foreach item rooms
		(progn	
			
			(setq thisIdentifier (car item))
			(setq thisRoom (cdr item))
			
			(setq newFields
				(list
					(cons "identifier" thisIdentifier)
				)
			)
			(setq thisRoom
				(append thisRoom newFields)
			)
			
			;;add the updated room to newRooms
			(setq newRooms 
				(append newRooms
					(list
						(cons thisIdentifier thisRoom)
					)
				)
			)
		)
	)
	
	(setq rooms newRooms)
	
	(if T ;nil
		(foreach item rooms
			(progn	
				
				(setq thisIdentifier (car item))
				(setq thisRoom (cdr item))
				
				(setq statusMessage (strcat thisIdentifier ": "))

				
				(if (assoc "centroidPoint" thisRoom)
					(setq statusMessage (strcat statusMessage " has centroidPoint.            "))
					(setq statusMessage (strcat statusMessage " DOES NOT HAVE centroidPoint.  "))
				)
					
				(princ statusMessage) (princ "\n")
			)
		)
	)
)
;======

;; drawRoomAnnotation is to rooms as drawDoorIcon is to doors
;; drawRoomAnnotation is to rooms as drawWindowIcon is to windows
(defun drawRoomAnnotation
	(
		thisRoom
		/
		centroidPoint  nestingPathOfCentroidPoint  transformOfCentroidPoint 
		unspecified
		annotationBlockName
		annotationBlockReference
		roomType 
		defaultRoomType
		identifier
		ceilingHeight 
		defaultCeilingHeight
		notes
		roomName
		roomDescription
		insertionAngle
		
		defaultRoomName
		defaultRoomDescription
		defaultNotes
	)
	
	(if (assoc "centroidPoint" thisRoom) 
		(progn
			(progn ; extracting info about the centroidPoint
				(setq centroidPoint 
					(cdr (assoc "centroidPoint" thisRoom))
				)
				(setq nestingPathOfCentroidPoint 
					(cdr (assoc "nestingPathOfCentroidPoint" thisRoom))
				)
				(setq transformOfCentroidPoint
					;;(nth 1 (car nestingPathOfCentroidPoint))
					;; above would of course not evaluate when we are at the root level, therefore I made populateRooms explcitly include the transform
					(cdr (assoc "transformOfCentroidPoint" thisRoom))
				)
				(setq centroidPointWCSCoordinates
					(mxv transformOfCentroidPoint
						(append
							(vlax-safearray->list 
								(vlax-variant-value 
									(vlax-get-property centroidPoint 'Coordinates)
								)
							)
							'(1)
						)
					)
				)
				(setq centroidPointWCSCoordinates 
					(list
						(nth 0 centroidPointWCSCoordinates)
						(nth 1 centroidPointWCSCoordinates)
						(nth 2 centroidPointWCSCoordinates)
					)
				)
			)
			;============
			
			;; at this point, we have centroidPointWCSCoordinates (with coordinates in in the root context).
			
			;; compute parameters from roomData.
			(setq unspecified nil) ;; 'unspecified' is used as an expression in roomData which basically means "use the default value"
			(setq defaultRoomType "standard")
			(setq defaultCeilingHeight -1)
			
			
			;;I would have preferred the following three defaults to be empty strings, but an empty string, when generated by an autocad field, produces the value "---", to indicate nonexistent field text, so I amd
			;; setting the defaults to be a single space.
			(setq defaultRoomName " ")
			(setq defaultRoomDescription " ")
			(setq defaultNotes " ")
			
			(setq identifier 
				(cdr (assoc "identifier" thisRoom))
			)
			
			
			(setq roomType
				(cdr (assoc "roomType" thisRoom))
			)
			(if (or (not roomType) (= "unspecified" roomType)) 
				(setq roomType defaultRoomType) ;;default value
			) 
			
			(setq roomName
				(cdr (assoc "roomName" thisRoom))
			)
			(if (or (not roomName) (= "" roomName)) 
				;;(setq roomName nil)
				(setq roomName defaultRoomName)
			) 
			
			(setq roomDescription
				(cdr (assoc "roomDescription" thisRoom))
			)
			(if (or (not roomDescription) (= "" roomDescription)) 
				(setq roomDescription defaultRoomDescription)
			) 
			
			(setq notes
				(cdr (assoc "notes" thisRoom))
			)
			(if (or (not notes) (= "" notes)) 
				(setq notes defaultNotes)
			) 
			
			(setq ceilingHeight 
				(eval
					(read
						(cdr (assoc "ceilingHeight" thisRoom))
					)
				)
			)
			(if (not ceilingHeight) (setq ceilingHeight defaultCeilingHeight)) ;;default value
			
			
			
			
			;; insert a roomAnnotation block
			(setq insertionAngle 0)
			;; choose which block to insert based on the room's "type" field.
			(setq annotationBlockName (strcat "roomAnnotation" "_" roomType))
			(if (not (member annotationBlockName (getAllBlockNames))) 
				(setq annotationBlockName (blockNameToBlockFilePath annotationBlockName))
			)
			(princ "now inserting ") (princ annotationBlockName) (princ "\n")
			(setq annotationBlockReference
				(vla-InsertBlock modelSpace 
					(vlax-3D-point centroidPointWCSCoordinates)     ;;insertion point
					annotationBlockName   ;;name of block to insert
					1 ;;Xscale ;;later, we will be manipulate the scale factors based on the chirality of the door (as determined by the position of the guide relative to the centroidPoint.)
					1 ;;Yscale
					1 ;;Zscale	
					insertionAngle ;; rotation angle.
				) 
			)
			(vlax-put-property annotationBlockReference 'Layer "room_annotation") 
			
			(LM:setdynpropvalue annotationBlockReference "roomName" roomName )
			(LM:setdynpropvalue annotationBlockReference "roomDescription" roomDescription	)
			(LM:setdynpropvalue annotationBlockReference "roomIdentifier" identifier	)
			(LM:setdynpropvalue annotationBlockReference "notes" notes )
			(LM:setdynpropvalue annotationBlockReference "ceilingHeight" ceilingHeight )

		)
		(progn ;;in this case, the room was invalid and will be skipped
			;(princ "Skipping an invalid room...") (princ "\n")
			(princ "Skipping room that does not have a centroidPoint...") (princ "\n")
		)
	)
)
;======




;; loads the data from the json files doorData.json and windowData.json.
;;the conversion from json is accomplished by means of a php script that
;;generates the lisp code.


(defun loadData
	(
	/
	)
	;;I moved the following (startapp) functionality, which generates "data.lsp" from doorData.json, windowData.json, and roomData.json, into the makefile
	;;  because I can't get a reliable
	;; working path with the (Startapp) command and so (Startapp) would occasionally start in some odd directory.
	;; This means that this lisp script expects data.lsp to already exist.
	;; Doing it with the makefile has the added advantage that I don't have to have the hack of running the STD-SLEEP function below to wait for data.lsp to be generated.
	
	;; (startapp "php convertJsonToLisp.php")
	
	
	;;Using this sleep function here is a major hack because (startapp ) 
	;; is asynchronous, I think.
	;;I was getting 
	;;"malformed list on input" error when autocad would try to load 
	;;the data.lsp file at the same time php was in the process of 
	;;genereating it.
	; ; (defun STD-SLEEP (secs / endt)
	; ; (setq endt (+ (getvar "DATE") (/ secs 86400.0)))
	; ; (while (< (getvar "DATE") endt) T)) 
	; ; ; (load "std-sleep.lsp")
	; ; ; (STD-SLEEP 3) 
	
	
	(load "data.lsp")  ;;load the door, window, and room data.
)
;============

;; this function will create all the layers that are used by this script, if they do not exist already,
;; in order to avoid errors later on in the program when we try to add objects to those layers.
(defun makeRequiredLayers
	(
	/
		requiredLayers
	)
	
	(setq requiredLayers 
		(list
			(list
				(cons 	"name" 		"wall")
				(cons 	"color" 	"Green")
			)
			(list	
				(cons 	"name" 		"door")
				(cons 	"color" 	"Cyan")
			)
			(list	
				(cons 	"name" 		"window")
				(cons 	"color" 	"Blue")
			)			
			(list	
				(cons 	"name" 		"stair")
				(cons 	"color" 	"Magenta")
			) 	
			(list	
				(cons 	"name" 		"landing"	)
				(cons 	"color" 	"Red"		)
			) 	
			(list	
				(cons 	"name" 		"programmaticAnnotations")
				(cons 	"color" 	"Yellow")
			)
			(list	
				(cons 	"name" 		"room_annotation")
				(cons 	"color" 	"12")
			)
			(list	
				(cons 	"name"      "text_windowAnnotation")
				(cons 	"color" 	"7")
			)
			(list	
				(cons 	"name"      "text_roomAnnotation")
				(cons 	"color" 	"7")
			)
		)
	)
	;======
			
	; "wall" ;; will contain the final wall polylines, which are formed by trimming and joining geometry that starts out on the "wallUnbroken" layer.
	; "door",  ;; will contain the door block references
	; "window", ;;will contain the window block referenc
	; "stair",
	; "landing",			
	; "programmaticAnnotations" ;;this one probably doesn't need to be in the final product.

	;; note: we will not explicitly create layers that are assumed to exist from 
	;; the input drawing -- for instance "wallUnbroken", "doorAnchor", etc.  In fact, 
	;; doorAnchor and the like are leftovers from the build process and arguably do not have 
	;; any place in the final deliverable -- we do not want to confuse the client with any
	;; strange-looking leftovers of the build process.
	
	;; I should probably also initialize the colors and linetypes, etc. for these layers here, 
	;; but I won't bother with that at the moment.
		
	(foreach layer requiredLayers
		
		;;(princ (cdr (assoc "name" layer))) (princ "\n") (princ)
		;;(princ layer) (princ "\n") (princ)
		(command-s
			"-LAYER"
			"make"
			(cdr (assoc "name" layer))
			"color"
			(cdr (assoc "color" layer))
			(cdr (assoc "name" layer))
			;;"" ;; equivalent to sending Enter keystroke.
			"" ;; equivalent to sending Enter keystroke.
		)
	)	
)
;========

;; scans through all the blocks, any instance of "r<three-digits>" in a block name will be replaced with "room<three-digits>"
(defun standardizeRoomBlockNames
	(
		/
		acadObj
		doc
		modelSpace
		blocks
		block
		regex
		originalName
		newName
	)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq blocks (vla-get-Blocks doc))
	(setq regex (vlax-create-object "vbscript.regexp" ) )
	;(vlax-put-Property  regex "Pattern" "^(.*)r(\\d{3})(.*)$")
	(vlax-put-Property  regex "Pattern" "r(?=\\d{3})") ;; this pattern matches any r that is followed by 3 digits.
	(vlax-put-Property  regex "Global" 1)
	(vlax-dump-object regex)
	(vlax-for block blocks
		(setq originalName (vla-get-Name block))
		(setq newName (vlax-invoke-method regex 'Replace originalName "room"))
		(princ "originalName: ")(princ originalName) (princ "\n")
		(princ "newName: ")(princ newName) (princ "\n")
		(princ ".....\n")
		(vla-put-Name block newName		)
	)
)
;=============

;;scans through all entities in every block definition.  
;; if the entity is a polyline and is on layer "wallUnbroken" and we are not in one of the blocks to be skipped,
;; then set the rank of the polygon to 1.  The blocks to be skipped are blocks that define external walls.
;; We will leave the rank of these external polygons unmodified (i.e. default, which is rank 0 (which is additive)).
;; On second thought, instead of skipping over the external walls, let use explicitly set their rank as  zero.
;; What we are doing here is making subtractive every wallUnbroken polyline that represents an internal wall boundary.
;;This is a one-time hack for making the exisiting drawing ready to be used with the new region combination 
;; scheme for creating cutouts for doors and anchors.  ;; In the future, I anticipate setting the ranks manually as I am creating each room block.
(defun fixRoomPolygonRanks ;;one-time hack
	(
		/
		acadObj
		doc
		modelSpace
		layers
		groups
		blocks
		block
		numberOfPolylinesModified
		namesOfBlocksRepresentingExteriorWalls
		defaultRankForExteriorWalls
	)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))
	(setq blocks (vla-get-Blocks doc))
	
	(setq namesOfBlocksRepresentingExteriorWalls
		(list
			"common_exterior"
			"level1_exterior"
			"level2_exterior"
		)
	)
	
	(setq defaultRankForExteriorWalls 10)
	(setq defaultRankForinteriorWalls 21)
	
	(setq numberOfPolylinesModified 0)
	(vlax-for blockDefinition blocks
		(vlax-for entity blockDefinition
			(if 
				(and
					(= "AcDbPolyline" (vla-get-ObjectName entity))
					(= "wallUnbroken" (vla-get-Layer entity))	
				)
				(if 
					(member (vla-get-Name blockDefinition) namesOfBlocksRepresentingExteriorWalls) ;; if blockDefinition represents exterior walls, ...
					(progn
						(setRank entity defaultRankForExteriorWalls)
						(princ (strcat "Set a polyline in the " (vla-get-Name blockDefinition) " block definition to have rank " (itoa defaultRankForExteriorWalls) "." "\n"))
						(setq numberOfPolylinesModified (+ 1 numberOfPolylinesModified))
					)
					(progn
						(setRank entity defaultRankForinteriorWalls)
						(princ (strcat "Set a polyline in the " (vla-get-Name blockDefinition) " block definition to have rank " (itoa defaultRankForinteriorWalls)  "." "\n"))
						(setq numberOfPolylinesModified (+ 1 numberOfPolylinesModified))
					)
				)
			)
		)
	)
	
	(princ "fixRoomPolygonRanks modified ") (princ numberOfPolylinesModified) (princ " polygons.")(princ "\n")
	(princ)
	

)
;=====

;; closes wihtout sacing, then reopens the  file
;; that is currently open. Returns the newly-created Document
;; object. ;;NOT WORKING
(defun reloadCurrentDocument
	(
	
		/
		acadObj
		doc
		application
		documents
		
		path
		
	)
	(setq acadObj (vlax-get-acad-object))
    
    (setq documents (vla-get-Documents acadObj))
	(setq doc (vla-get-ActiveDocument acadObj))
	
	
	(setq path 

			(vla-get-FullName doc)

	)
	
	(vla-Close doc :vlax-false) ;;close without saving changes
	;(vlax-invoke-method doc 'Close :vlax-false )
	;;unfortunately, this won't work, because
	; the script runs as a child of the document -- we can't kill
	; the document without also killing this script.
	
	;;(princ path) (princ "\n")

	(vla-Open documents path)
)
;==========

;; this is a one-time hack to add a centroid point to each room block definition.
;; the centroid block is similar to the doorAnchor and windowAnchor: a locatable object
;; that survives explosion of the room block and can be used to
;; get programmatically, the position of the room.
;; In normal practice, the centroid point would be placed (manually ) in each room block while creating each
;; the room.  Centroid point is perhaps too technical -- it does not have to be teh exact centroid -- just a convenient
;; central point of the room for the purpase of knowing where to place the annotation.
(defun addACentroidPointToEachRoomBlockDefinition
	
	(
		/
		acadObj
		doc
		modelSpace
		blocks
		layers
		layer
		block
		centroidPointsLayer
		oddRankPolylines
		centroids
		meanCentroid
		roomIdentifiers
	)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq blocks (vla-get-Blocks doc))
	(setq layers (vla-get-Layers doc))
	
	(setq centroidPointsLayer
		(vla-Add layers "roomCentroidPoint") ;;this will add the layer, if it does not already exist, and get a reference to it.
	)
	
	(loadData)
	
	(setq roomIdentifiers
		(mapcar 'car roomData)
	)
	
	;;(princ roomIdentifiers) (princ "\n")
	
	(setq numberOfBlockDefinitionsModified 0)
	(vlax-for blockDefinition blocks
		(if (member (vla-get-Name blockDefinition) roomIdentifiers) 
			(progn
				;;collect all polylines with an odd (i.e. subtractive primeRank that are on 'wallUnbroken' layer 
				(setq oddRankPolylines (list ))
				(vlax-for entity blockDefinition
					(if 
						(and
							(member (vla-get-ObjectName entity) (list "AcDbPolyline" "AcDbCircle" "AcDbRegion"))
							(= "wallUnbroken" (vla-get-Layer entity))
							(= 1 
								(rem (+ 2 (rem (car (getRank entity)) 2)) 2) ;; this gnarly expression simply returns (car (getRank entity))  MODULO 2 (the rigamarrol with adding 2 ensures that the result is always positive because the rem function sometimes returns negative numbers.
							)	
						)
						(progn
							
							(setq oddRankPolylines
								(append
									oddRankPolylines
									(list entity)
								)
							)
						)
					)
				)
				;;(princ (vla-get-Name blockDefinition))(princ " got this far.\n")
				
				;;(princ (vla-get-Name blockDefinition))(princ "  (length oddRankPolylines): ")(princ (length oddRankPolylines)) (princ "\n")
				
				;; now we have our list oddRankPolylines.  next task is to compute the centroid.
				
				(if (> (length oddRankPolylines) 0)
					(progn
						(setq centroids
							(mapcar
								'(lambda (x) (LM:PolyCentroid (toEntityName x)))
								oddRankPolylines 
							)
						)
						
						;;(princ centroids)
						
						;; set meanCentroid to be the average of the individual centroid points.
						(setq meanCentroid 
							(mapcar 
								'(lambda (myList) (/ (apply '+ myList) (length myList)) )  ;; 'mean
								(trp centroids)	;; this will be alist of the form (  (x1 x2 x3 x4 ...)   (y1 y2 y3 y4 ...)    (z1 z2 z3 z4 ...)  
							)
						)
					)
					(progn
						;;in this case there were no rank 1 polylines, so we will simply set the meanCentroid to be the origin and issue a warning
						;;(princ "warning: no rank1 polylines. ")
						(setq meanCentroid (list 0 0 0))
					)
				)
				(princ (vla-get-Name blockDefinition)) (princ "::\tmeanCentroid: ")(princ meanCentroid)(princ "\n")
				(setq roomCentroidPoint
					(vla-AddPoint blockDefinition
						(vlax-3d-point meanCentroid)
					)
				)
				(vla-put-Layer roomCentroidPoint (vla-get-Name centroidPointsLayer))
				(setAttachedString roomCentroidPoint (vla-get-Name blockDefinition))
				(setq numberOfBlockDefinitionsModified (+ 1 numberOfBlockDefinitionsModified) )
			)
		)
	)
	
	(princ "addACentroidPointToEachRoomBlockDefinition modified ") (princ numberOfBlockDefinitionsModified) (princ " block definitions.")(princ "\n")
	(princ)
)
;==========

; this function has the same effect as running the TORIENT (expressTool) command and selecting all.
;;Much of this code is lifted from "acettxt.lsp" in the Express tools folder, which is where (C:TORIENT) is defined.
;; this command  requires express tools to be loaded.
(defun TOrientAll
	(
		myAngle
		/
		flt
		ss
		mode
	)
	(command)(command)
	
	(load "acettxt.lsp") ;;just in case it was not already loaded as part of express tools.
	
	(progn  ;;; ==== CODE COPIED FROM acettxt.lsp (WITH REFORMATTING, MODIFICATIONS AS INDICATED) ======;;| 																														  
		(setq flt                                                                                       ;;|
			'(                                                                                          ;;|
				(-4 . "<OR")                                                                            ;;|
				(0 . "TEXT")                                                                            ;;|
				(0 . "ATTDEF")                                                                          ;;|
				(0 . "MTEXT")                                                                           ;;|
				(-4 . "<AND")                                                                           ;;|
				(0 . "INSERT")                                                                          ;;|
				(66 . 1)                                                                                ;;|
				(-4 . "AND>")                                                                           ;;|
				(-4 . "OR>")                                                                            ;;|
			)                                                                                           ;;|
		);setq                                                                                          ;;|
		(if                                                                                             ;;|                              
			(and                                                                                        ;;|                              
				;;(setq ss (ssget flt))                                                                 ;;|   ;;NEIL COMMENTED OUT THIS LINE                                                           
				(setq ss (ssget "_X" flt))                                                              ;;|   ;;NEIL ADDED THIS LINE (selects everything, without user interaction)                    
				(setq ss                                                                                ;;|
					(car                                                                                ;;|
						(acet-ss-filter                                                                 ;;|
							(list                                                                       ;;|
								ss                                                                      ;;|         
								'(                                                                      ;;|
									("LAYERUNLOCKED") ;Dis-allow locked layers, non-current-space       ;;|              
									("CURRENTUCS")    ;and dis-allow objects not in current ucs         ;;|              
								)                                                                       ;;|           
								T                                                                       ;;|            
							);list                                                                      ;;|              
						);acet-ss-filter                                                                ;;|                          
					);car                                                                               ;;|                           
				);setq                                                                                  ;;|                             
			);and                                                                                       ;;|                             
			(progn                                                                                      ;;|                             
				(setq ss (bns_annotation_ss ss))                                                        ;;|                             
				;;;(setq mode (getangle "\nNew absolute rotation <Most Readable>: "));setq              ;;|   ;;NEIL COMMENTED OUT THIS LINE       
				(setq mode myAngle);setq                                                                  ;;|   ;;NEIL ADDED THIS LINE               
				;;(setvar "highlight" 0)                                                                ;;|   ;;NEIL COMMENTED OUT THIS LINE                            
				(setq j (bns_trot ss mode))                                                             ;;|                              
				(if ss                                                                                  ;;|                              
					(command "_.select" ss "")                                                          ;;|                              
				);if                                                                                    ;;|                              
				(princ (strcat "\n" (itoa j) " objects modified."))                                     ;;|                              
																										;;|							  
			);progn then                                                                                ;;|                              
			(princ "\nNo valid objects selected.")                                                      ;;|                              
		);if                                                                                            ;;|                              
	) ;;;=============  END CODE COPIED FROM acettxt.lsp ===============================================;;|
	;========
	
	(princ)                                                                             
)
;====

;;this is an absolute hack to retrieve the names of all the user parameters,
;; so that I can then delete them with the -PARAMETERS command.
;; The -PARAMAETERS command really ought to be able to delete all parameters in one fell swoop,
;; but this is the workaround.
(defun getAllParameterNames

	(

		/
		thisLine
		logFile
		regex
		parameterNames
		thisLine
		thisParameterName
		numberOfLinesToSkip
		originalCmdEcho
	)
	(setq originalCmdEcho (getvar "CMDECHO"))
	(setvar "CMDECHO" 1) ;;we rely on cmdecho being on for this function to work
	
	;;(vl-file-delete (getvar "LOGFILENAME")) ;;can't delete the exisiting log file because it is in use by autocad -- have to record the current number of lines instead.
	(setq numberOfLinesToSkip 0)
	(if 
		(setq logFile 
			(open (getvar "LOGFILENAME") "r")
		)
		(progn
			(while (read-line logFile)
				(setq numberOfLinesToSkip (+ 1 numberOfLinesToSkip))
			)
			(close logFile)
		)
	)
	
	(princ (strcat "\n(getAllParameterNames) is skipping the first " (itoa numberOfLinesToSkip) " lines of the log file.\n"))
	
	
	
	(vl-cmdf "LOGFILEON")
	(vl-cmdf "._-PARAMETERS" "?")
	(vl-cmdf "LOGFILEOFF")
	(STD-SLEEP 1) ;;this delay may not be strictly necessary.  The idea is to rest for a moment to allow the log file to finish being written.
	
	;; At this point there is a text file, whose path is logFilePath, containing the output from the "-PARAMETERS ?" command, which
	;; will contain, among other things, the paramater names in question.  We simply need to parse the text file.

	(setq parameterNames (list ))
	(if (setq logFile (open (getvar "LOGFILENAME") "r"))
		(progn 
			(setq regex (vlax-create-object "vbscript.regexp" ) )
			(vlax-put-Property  regex "Pattern" "^Parameter:\\s+(\\S+)\\s+") 
			(vlax-put-Property  regex "Global" 0)
			(setq lineIndex 0)
			(while (setq thisLine (read-line logFile))
				(if (>= lineIndex numberOfLinesToSkip)
					(progn
						(setq matchCollection
							(vlax-invoke-method regex 'Execute thisLine)
						)
						(if (> (vlax-get-property matchCollection "Count") 0) ;;if there was a match
							(progn
								(if nil ;;abandoned version -- declaring each level of the next as its own explicit variable
									(progn
										(princ "matchCollection: \n")
										(LM:dump matchCollection)(princ "\n")
										
										
										(setq match 
											(vlax-get-property matchCollection 'Item 0)
										)
										(princ "match: \n")
										(LM:dump match)(princ "\n")
										
										
										(setq subMatches
											(vlax-get-property match "SubMatches")
										)
										(princ "subMatches: \n")
										(LM:dump subMatches)(princ "\n")
										
										
										(setq thisParameterName 
											(vlax-variant-value 
												(vlax-get-property subMatches 'Item 0)
											)
										)
										(princ "thisParameterName: \n")
										(princ thisParameterName)(princ "\n")
					
										(setq parameterNames
											(append
												parameterNames
												(list  thisParameterName)
											)
										)
									)
								)
								;=====
								
								;;much simpler: single statement
								(setq parameterNames
									(append
										parameterNames
										(list  
											(vlax-variant-value 
												(vlax-get-property 
													(vlax-get-property 
														(vlax-get-property matchCollection 'Item 0) 
														"SubMatches"
													) 
													'Item 0
												)
											)
										)
									)
								)
								;======
								
							)
						)
					)
				)
				(setq lineIndex (+ 1 lineIndex))
			)
			(close logFile)
		)
	)
	
	(setvar "CMDECHO" originalCmdEcho)
	
	parameterNames
)
;=======

;;This funciton is part of the cleanup process.
;;PURGE does not get rid of leftover parameters,
;;so I have to do it with this function.
(defun deleteAllParameters
	(
		/
		numberOfParametersDeleted
		parameterNames
	)
	
	(setq parameterNames (getAllParameterNames))
	(princ "\n")(princ "(length parameterNames): ")(princ (length parameterNames))(princ "\n")
	(setq numberOfParametersDeleted 0)
	(foreach parameterName parameterNames
	
		(vl-cmdf "._-PARAMETERS" "Delete" parameterName)
		(setq numberOfParametersDeleted (+ 1 numberOfParametersDeleted))
	)
	
	(princ "\n")(princ (strcat "\n(deleteAllParameters) has deleted " (itoa numberOfParametersDeleted) " parameters."))(princ "\n")
	(princ)
)
;=============

;; caution: this function DOES NOT respect already-assigned identifiers; it overwrites
;; existing identifiers.  This function is intended to be deliberately, manually, invoked by the user
(defun autoNumberDoorsAndWindows
	(
		/
		lineCount
		index
		acadObject
		acadDocument
		modelSpace
		blocks
		sowDoorAnchor
		sowWindowAnchor
		orderingFunction
		doorAnchors
		windowAnchors
		i
		thisDoorAnchor
		thisHingePoint
		thisDoorIdentifier
		thisWindowAnchor
		thisStartPoint
		thisWindowIdentifier 
		newItem
		handlesOfProcessedLines
	)


	
	(clearAnnotations)
	(setq lineCount 0)
	(setq index 0)
	(setq acadObject (vlax-get-acad-object))
	(setq acadDocument (vla-get-ActiveDocument acadObject))
	(setq modelSpace (vla-get-ModelSpace acadDocument))
	(setq blocks (vla-get-Blocks acadDocument))
	(initializeNestingStack)
	
	(setq doorAnchors (list ))
	(setq windowAnchors (list ))
	(setq handlesOfProcessedLines (list ) ) ; we keep a list of handles of lines that we have already processed in order to ensure that we do not process the same line more than once (this might happen if there were multiple block references in model space all pointing to the same block definition.)
	
	(defun sowDoorAnchor
		(
			doorAnchor
			/
			newItem
		)

		(if (member (vla-get-Handle doorAnchor) handlesOfProcessedLines) ; if we have already processed this line...
			(progn
				; in this case, we have already seen this handle
				(princ "The line with handle ") (princ (vla-get-Handle doorAnchor)) (princ " has been seen more than once - we are being careful not to overcount it.") (princ "\n")
			)
			(progn
				; in this case, we have not already seen this handle
				(setq newItem
					(list
						doorAnchor
						(getStartPoint doorAnchor)
					)
				)
				(setq doorAnchors 
					(append
						doorAnchors
						(list newItem)
					)
				)
				(setq handlesOfProcessedLines 
					(append
						handlesOfProcessedLines
						(list (vla-get-Handle doorAnchor))
					)
				)
			)
		)
	)
	
	
	
	(defun sowWindowAnchor
		(
			windowAnchor
			/
			newItem
		)

		(if (member (vla-get-Handle windowAnchor) handlesOfProcessedLines) ; if we have already processed this line...
			(progn
				; in this case, we have already seen this handle
				(princ "The line with handle ") (princ (vla-get-Handle windowAnchor)) (princ " has been seen more than once - we are being careful not to overcount it.") (princ "\n")
			)
			(progn
				; in this case, we have not already seen this handle
				(setq newItem
					(list
						windowAnchor
						(getStartPoint windowAnchor)
					)
				)
				(setq windowAnchors 
					(append
						windowAnchors
						(list newItem)
					)
				)
				(setq handlesOfProcessedLines 
					(append
						handlesOfProcessedLines
						(list (vla-get-Handle windowAnchor))
					)
				)
			)
		)
	)
	
	(princ "now sowing doorAnchors.\n")
	(applyToLinesWithin modelSpace "doorAnchor" 'sowDoorAnchor)
	
	(princ "now sowing windowAnchors.\n")
	(applyToLinesWithin modelSpace "windowAnchor" 'sowWindowAnchor)
	
	
	(setq orderingFunction
		'(lambda
			(
				a
				b
				/
				ax ay az bx by bz aHingePoint bHhingePoint returnValue
			)
			(setq aHingePoint (nth 1 a))
			(setq bHingePoint (nth 1 b))
			
			(setq ax (nth 0 aHingePoint))
			(setq ay (nth 1 aHingePoint))
			(setq az (nth 2 aHingePoint))
			
			(setq bx (nth 0 bHingePoint))
			(setq by (nth 1 bHingePoint))
			(setq bz (nth 2 bHingePoint))
			
			(setq returnValue nil)
			(COND
				(
					(< ay by)
					(setq returnValue 1)
				)
				(
					(> ay by)
					(setq returnValue nil)
				)
				(
					T  ; else ay must equal by, in which case we look at the x's to determine the order
					(COND
						(
							(< ay by)
							(setq returnValue 1)
						)
						(
							(> ay by)
							(setq returnValue nil)
						)
						(
							T
							(setq returnValue nil)
						)
					)
				)
			)
			
			returnValue
		)
	)

	; sort the doorAnchors list according to location of hinge points, first by y then by x.
	(setq doorAnchors
		(vl-sort	
			doorAnchors
			orderingFunction
		)
	)

	(setq windowAnchors
		(vl-sort	
			windowAnchors
			orderingFunction
		)
	)


	; assign an identifier to each doorAnchor
	(setq i 0)
	(foreach
		item doorAnchors 
		(progn
			(setq i (+ 1 i)) ;increment i
			(setq thisDoorAnchor (nth 0 item))
			(setq thisHingePoint (nth 1 item))
			(setq thisDoorIdentifier (strcat "door" (itoa i)))
			(setAttachedString thisDoorAnchor thisDoorIdentifier)
			(princ (getAttachedString thisDoorAnchor)) (princ " at ") (princ thisHingePoint) (princ ".\n")
		)
	)

	; assign an identifier to each windowAnchor
	(setq i 0)
	(foreach
		item windowAnchors 
		(progn
			(setq i (+ 1 i)) ;increment i
			(setq thisWindowAnchor (nth 0 item))
			(setq thisStartPoint (nth 1 item))
			(setq thisWindowIdentifier (strcat "window" (itoa i)))
			(setAttachedString thisWindowAnchor thisWindowIdentifier)
			(princ (getAttachedString thisWindowAnchor)) (princ " at ") (princ thisStartPoint) (princ ".\n")
		)
	)


	(princ "last assigned window identifier: ") (princ  thisWindowIdentifier) (princ  ".\n" ) 
	(princ "last assigned door identifier: ") (princ  thisDoorIdentifier) (princ  ".\n" ) 
	(write-line "")
	(write-line "")
	(princ)
	(annotateAllDoorAndWindowLines)
)
;=============

;; isBConstruction() taks, as an argument either an ename or a vla-object.
;; it returns a boolean indicating whether the entity has been flagged as construction using the
;; BCONSTRUCTION command
;;(setq handlesOfConstructionEntities (list )) ;;debugging
(defun isBConstruction
	(
		arg
		/
		theEname
		theEntity
		returnValue
		bconstructionApplicationName
		xDataGroupCodeForInteger
		theHandle
	)
	(setq returnValue nil) ;default assumption is that the entity is not bconstruction.
	(setq bconstructionApplicationName "ACAD_BCONSTRUCTION")
	(setq xDataGroupCodeForInteger 1070) ;;see http://help.autodesk.com/view/ACD/2016/ENU/?guid=GUID-3481BF7B-73CB-4FD5-B421-C25BE92C6D56
	; resolve the argument into an ename.
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
			(princ "error: isBConstruction was passed an argument that was neither an ENAME nor a VLA-OBJECT.")
		)
	)
	(setq theHandle (vla-get-Handle (vlax-ename->vla-object theEname)))
	(setq theEntity 
		(entget theEname (list bconstructionApplicationName))
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
			(if 
				(= 1 
					(cdr 
						(assoc xDataGroupCodeForInteger 
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
				(progn
					; (vlax-dump-object (vlax-ename->vla-object theEname))
					; (princ "isBConstruction has determined that the entity with handle ")(princ theHandle)(princ " is construction geometry.")(princ "\n\n")
					; (prompt "enter any key to continue")
					
					; ; (setq handlesOfConstructionEntities 
						; ; (append handlesOfConstructionEntities
							; ; (list
								; ; theHandle
							; ; )
						; ; )
					; ; )
					
					(setq returnValue T)
				)
			)
		)
		(progn
			;in this case, selectedEntity does not have any existing xData (at least none that we care about)
			;(princ "no exisiting xData found.\n")
		)
	)
	returnValue	
)
;=============

(defun setAsBconstruction
	(
		arg
		/
		theEname
		theEntity
		returnValue
		bconstructionApplicationName
		xDataGroupCodeForInteger
		theHandle
	)
	(setq returnValue nil) 
	(setq bconstructionApplicationName "ACAD_BCONSTRUCTION")
	(setq xDataGroupCodeForInteger 1070) ;;see http://help.autodesk.com/view/ACD/2016/ENU/?guid=GUID-3481BF7B-73CB-4FD5-B421-C25BE92C6D56
	; resolve the argument into an ename.
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
			(princ "error: setAsBconstruction was passed an argument that was neither an ENAME nor a VLA-OBJECT.")
		)
	)
	(setq theHandle (vla-get-Handle (vlax-ename->vla-object theEname)))

	
	(setq newXData                       
		(list 
			-3 
			(list 
				bconstructionApplicationName                 
				(cons xDataGroupCodeForInteger  1) 
			)
		)                               
	)
	
	(setq theEntity 
		(entget theEname (list bconstructionApplicationName))
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
	
	returnValue
)

;;applySelectiveTrimming finds all block definitions that contain (as an immediate child, not via nested reference)
;; one or more polylines (or circles or regions) on the designated trimming layer.
;; Then, it modifies each such block definition as follows:
;;  fully explode all block references
;;  apply the trimming polyline to each wall region or stairland region.
;;  When I say "apply the trimmingRegion to an entity x", what I mean is to do
;;	(rankwiseCombineRegions 
;;		(list
;;			x
;;			(list
;;				(append
;;					(getRank x)
;;					(list 1) ;; the appended '1' in the rank means that the trimmingRegion will subtract from whatever x is doing when it is rankwiseCombined with other things.
;;				)
;;				trimmingRegion
;;			)
;;		)
;;	)
;; then, convert the resulting region back to polylines and set the rank of each to (getRank x)  (i.e. the rank of the original entity x).
;; This allows us to have trimmingRegions that affect only certain entities without having to go through the contortions of designating a unique layer name for each trimming domain,
;; and then processing the special layer names explicitly in generateDelivrableDrawing.
(defun applySelectiveTrimming
	(

	/
		acadObject
		acadDocument
		modelSpace
		blockDefinitions
		
		blockDefinition
		processThisBlockDefinition
		entity
		nameOfTrimmingLayer
		trimmingRegions
		rankOfEntity
		thisTrimmedRegion
		polyline
		layerOfEntity
		
		blockReferencesToBeExploded
		blockReferenceToBeExploded
		
		regionsToBeTrimmed
		regionToBeTrimmed
	)
	
	(setq acadObject (vlax-get-acad-object))
	(setq acadDocument (vla-get-ActiveDocument acadObject))
	(setq modelSpace (vla-get-ModelSpace acadDocument))
	(setq blockDefinitions (vla-get-Blocks acadDocument))
	
	(setq nameOfTrimmingLayer "interLevelTrimming")
	(vlax-for blockDefinition blockDefinitions
		(setq weNeedToProcessThisBlockDefinition nil)
		(setq trimmingRegions (list ))
		;; look for regions in the trimming layer to figure out if we need to process this block definition, 
		;; and collect the trimmingRegions along the way.
		(vlax-for entity blockDefinition
			(if
				(and
					(= (vla-get-Layer entity) nameOfTrimmingLayer)
					(or
						(= "AcDbPolyline" (vla-get-ObjectName entity))
						(= "AcDbCircle" (vla-get-ObjectName entity))
						(= "AcDbRegion" (vla-get-ObjectName entity))
					)
				)
				(progn
					(setq trimmingRegions (cons entity trimmingRegions))
					(setq weNeedToProcessThisBlockDefinition T)
				)
			)
		)
		(if weNeedToProcessThisBlockDefinition
			(progn	
				;; fully explode all non-construction block references
				(setq blockReferencesToBeExploded (list ))
				(vlax-for entity blockDefinition 
					(if
						(and
							(= "AcDbBlockReference" (vla-get-ObjectName entity))
							(not (isBConstruction entity))
						)
						(setq blockReferencesToBeExploded (cons entity blockReferencesToBeExploded))
					)
				)
				(foreach  blockReferenceToBeExploded blockReferencesToBeExploded
					(fullyExplodeBlockReference blockReferenceToBeExploded)
					(if (not (vlax-erased-p blockReferenceToBeExploded)) (vla-Delete blockReferenceToBeExploded))
				)
				
				;; trim regions on the wallUnbroken or stairLands layers using trimmingRegions
				(setq regionsToBeTrimmed (list))
				(vlax-for entity blockDefinition 
					(if
						(and
							(or 
								(= "wallUnbroken" (vla-get-Layer entity))
								(= "stairLands" (vla-get-Layer entity))
							)
							(or
								(= "AcDbPolyline" (vla-get-ObjectName entity))
								(= "AcDbCircle" (vla-get-ObjectName entity))
								(= "AcDbRegion" (vla-get-ObjectName entity))
							)
						)
						(progn
							(setq regionsToBeTrimmed (cons entity regionsToBeTrimmed))
						)
					)
				)
				(foreach regionToBeTrimmed regionsToBeTrimmed
					(setq rankOfRegionToBeTrimmed (getRank regionToBeTrimmed))
					(setq layerOfRegionToBeTrimmed (vla-get-Layer regionToBeTrimmed))
					(setq thisTrimmedRegion
						(rankwiseCombineRegions
							(append
								(list
									(list (list 0) regionToBeTrimmed)
								)
								(mapcar 
									'(lambda (x) (list (list 1) x))
									trimmingRegions
								)
							)
						)
					)
					(setRank thisTrimmedRegion rankOfRegionToBeTrimmed)
					(vla-put-Layer thisTrimmedRegion layerOfRegionToBeTrimmed)
					(if 
						(and
							(not (vlax-erased-p regionToBeTrimmed))
							(not (= (vla-get-ObjectID regionToBeTrimmed) (vla-get-ObjectID thisTrimmedRegion))) ;;I am not sure it would ver happen that entity and thisTrimmedRegion would ever point to the same object (maybe it could happen as a result of the way rankwiseCombineRegions works, not cloning the starting region in the combine), but if ti did happen, then we of course would not want to invoke entity's Delete method.  This check guards against this.
						)
						(vla-Delete regionToBeTrimmed)
					)
					;; I want to avoid converting trimmed region to polylines because,
					;; if the trimmed region has holes, polylines cannot properly represent these (mainly because I can't access the rank information
					;; that I need from Autocad internals when converting region into polylines.  The trimmed region may need to be rankwise combined with other regions 
					;; later on.
					;; I can preserve hole-y regions properly by leaving them as true region objects.  
					;; I will have to be aware that what started out as polylines may now be represented as regions, and handle this if needed before generating the final drawing.
					;; Another reason to avoid using convertRegionToPolylines, at least as of the moment I write this (2017/01/05) is that convertRegionToPolylines relies on sending
					;; commands to autocad console, and I am therefore unsure about whether it would work when passed a region that is within a blockDefinition (other than modelSpace).
					
					; (foreach polyline (convertRegionToPolylines thisTrimmedRegion)
						; (vla-put-Layer polyline layerOfEntity)
					; )
				)
			)
		)
	)
)
;==============

;; explodes the blockReference and all nested blockReferences.  Returns an autolisp list of the resulting entities.
(defun fullyExplodeBlockReference 
	(
		blockReference
		/
		explosionProducts
		entity
		blockReferencesToDelete
		blockReferenceToDelete
		moreExplosionProducts
		i
		result
	)
	(setq explosionProducts (gc:VariantToLispData (vla-Explode blockReference)))
	(setq moreExplosionProducts (list ))
	(setq blockReferencesToDelete (list ))
	;;(princ "(length explosionProducts): ")(princ (length explosionProducts))(princ "\n")

	(foreach entity explosionProducts
		;;(setq result entityObjectName ;;todo detect deleted object by catching the exception that happens when we lookup ObjectName.
		(if 
			(and
				(not (vlax-erased-p entity))
				(= "AcDbBlockReference" (vla-get-ObjectName entity))
			)
			(progn
				(setq moreExplosionProducts
					(append
						moreExplosionProducts
						(fullyExplodeBlockReference entity)
					)
				)
				(vla-Delete entity) ;delete the blockReference that we just exploded.
			)
		)
	)
	(setq explosionProducts
		(append explosionProducts moreExplosionProducts)
	)
	
	;;filter out any erased entities from explosionProducts (such as the block references that I might have deleted abov, and other entites that become erased when they are exploded out of a block reference (dimensional constraints, for instance)
	(setq
		explosionProducts
		(vl-remove-if
			'vlax-erased-p
			explosionProducts
		)
	)
	explosionProducts
)



;;=== ACTIONS: ===============================


;;spits out a report for each block definition saying which block definitions it refers to (directly, not as nested references)
(defun reportOnBlockDependence
	(
	/
		acadObject
		acadDocument
		modelSpace
		blockDefinitions
		
		blockDefinition
		blockNames
		blockName
		namesOfReferencedBlocks
		nameOfReferencedBlock
		blockReference
		entity
		report
		hasRooms
		regex
		roomsCount
	)
	
	(setq acadObject (vlax-get-acad-object))
	(setq acadDocument (vla-get-ActiveDocument acadObject))
	(setq modelSpace (vla-get-ModelSpace acadDocument))
	(setq blockDefinitions (vla-get-Blocks acadDocument))
	(setq report "")
	
	(setq regex (vlax-create-object "vbscript.regexp" ) )
	(vlax-put-Property  regex "Pattern" "room\\d+") ;; this pattern matches any r that is followed by 1 or more digits
	(vlax-put-Property  regex "Global" 1)
	
	(setq blockNames (list ))
	(vlax-for blockDefinition blockDefinitions
		(setq blockNames (cons (vla-get-Name blockDefinition) blockNames))
	)
	(setq blockNames
		(vl-remove-if
			'(lambda (x) (= :vlax-true (vlax-invoke-method regex 'Test x))) ;;if the name of the block is of the form roomn...)
			blockNames
		)
	)
	(setq blockNames (vl-sort blockNames '<))
	(foreach blockName blockNames
		(setq roomsCount 0)
		(setq blockDefinition (vla-Item blockDefinitions blockName))
		
		(setq namesOfReferencedBlocks (list))
		(vlax-for entity blockDefinition
			(if 
				(= "AcDbBlockReference" (vla-get-objectname entity))
				(progn
					(if 
						(= :vlax-true (vlax-invoke-method regex 'Test (vla-get-Name entity))) ;;if the name of the block is of the form roomn...
						(progn
							(setq roomsCount (1+ roomsCount))
						)
						(progn
							;;(vlax-dump-object entity)
							(setq namesOfReferencedBlocks (cons (vla-get-EffectiveName entity) namesOfReferencedBlocks))
						)
					)
				)
			)
		)
		(setq namesOfReferencedBlocks (vl-sort namesOfReferencedBlocks '<))
		(if (> roomsCount 0)
			(setq namesOfReferencedBlocks
				(append
					namesOfReferencedBlocks
					(list (strcat "(" (itoa roomsCount) " rooms)"))
				)
			)
		)
		
		(princ blockName)(princ "\n")
		(foreach nameOfReferencedBlock namesOfReferencedBlocks
			(princ "\t")(princ nameOfReferencedBlock)(princ "\n")
		)
		(princ "===========================\n")
		
	)
)

;; it is a bit of a hack to initializeGlobalObjects in this script, which ought to be merely a library of
;; function definitions.
;; TO DO: invoke initializeGlobalObjects as needed in functions that need it.
(initializeGlobalObjects)

(defun putPaperSpaceOriginAtCornerOfPaper
	(
		/
		doc
		modelSpace
		layers
		groups
		blockDefinitions
		plotOrigin
	)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq paperSpace (vla-get-PaperSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))
	(setq blockDefinitions (vla-get-Blocks doc))
	
	(setq activeLayout (vla-get-ActiveLayout doc))
	
	(setq toInches '(lambda (x) (/ x 25.4)))
	
	 ;; Read and display the original value
    (setq plotOrigin (gc:VariantToLispData (vla-get-PlotOrigin activeLayout)))
	(princ (vla-get-Name activeLayout))(princ "\n")
	(princ "plotOrigin: ")(princ (mapcar toInches plotOrigin))(princ " inches")(princ "\n")
	
	(vla-GetPaperSize activeLayout 'myWidth 'myHeight)
	(setq paperSize (list myWidth myHeight ))
	(princ "paperSize: " )(princ (mapcar toInches paperSize))(princ " inches")(princ "\n")
	
	(vla-GetPaperMargins activeLayout 'myLowerLeft 'myUpperRight)
	(setq lowerLeftPaperMargin (gc:VariantToLispData myLowerLeft)	)
	(setq upperRightPaperMargin	(gc:VariantToLispData myUpperRight)	)
	
	(princ "lowerLeftPaperMargin: " )(princ (mapcar toInches lowerLeftPaperMargin))(princ " inches")(princ "\n")
	(princ "upperRightPaperMargin: " )(princ (mapcar toInches upperRightPaperMargin))(princ " inches")(princ "\n")
	;(vlax-dump-object activeLayout)
	;(princ (mapcar '(lambda (x) (- x)) lowerLeftPaperMargin))
	
	(vla-put-PlotOrigin activeLayout
		(gc:2dPointListToVariant
			(list
				(mapcar '(lambda (x) (- x)) lowerLeftPaperMargin)
			)
		)
	)
)


;;the following is equivalent to vlax-2d-point, if there were such a  function
(defun toVariantArrayOfDouble (lst)
	(vlax-make-variant
		(vlax-safearray-fill
			(vlax-make-safearray
				vlax-VbDouble
				(cons 0 (1- (length lst)))
			)
			lst
		)
	)
)
(princ)
