(vl-load-com)
(load "variantArrayConversion.lsp")
(load "polygonRanking.lsp")



(defun c:Example_AddRegion ()

    ;; This example creates a region from an arc and a line.
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))

    ;; Define the arc
    (setq centerPoint (vlax-3d-point 5 9 0)
          radius 2
          startAngle 0
          endAngle 3.141592)
    (setq arc (vla-AddArc modelSpace centerPoint radius startAngle endAngle))
    
    ;; Define the line
    (setq line (vla-AddLine modelSpace (vla-get-StartPoint arc) (vla-get-EndPoint arc)))
	
	(setq pline 
		(vla-AddPolyline modelSpace 
			(gc:3dPointListToVariant
				(list
					(list 2 2 0)
					(list 8 2 0)
					(list 8 6 0)
					(list 2 6 0)
					(list 2 2 0)
				)
			)
		)
	)

    ; (setq curves (vlax-make-safearray vlax-vbObject '(0 . 1)))
    ; (vlax-safearray-put-element curves 0 arc)
    ; (vlax-safearray-put-element curves 1 line)
	
	(setq region1BoundaryCurves  (list pline))
	(setq region2BoundaryCurves  (list arc line))
  
    ;; Create the regions
    (setq region1 
		(car
			(gc:VariantToLispData
				(vla-AddRegion modelSpace  
					(gc:ObjectListToVariant 
						region1BoundaryCurves
					)
				)
			)
		)
	)
	
	(setq region2
		(car
			(gc:VariantToLispData
				(vla-AddRegion modelSpace  
					(gc:ObjectListToVariant 
						region2BoundaryCurves
					)
				)
			)
		)
	)
	
	;;delete the original boundary curves
	(foreach entity (append region1BoundaryCurves region2BoundaryCurves)
		(vla-Delete entity)
	)
	
	;;modify region1 by subtracting region2 from it.
	;(setq copyOfRegion2 (vla-Copy region2))
	
	;(vla-Boolean region1 acSubtraction region2) ;;it appears that region2 is consumed in this process.
	(vla-Boolean region1 acUnion region2)
	
	(convertRegionToPolylines region1)
	
	

	
	
	; (setq region2 (vla-AddRegion modelSpace  (gc:ObjectListToVariant (list arc line))))
	
	; (foreach region (gc:VariantToLispData region1)
		; (vlax-dump-object region)
	; )
	
	;(princ (handent (vla-get-Handle region1)))
    
	;(vla-ZoomAll acadObj)
	(princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;when you attempt to invoke the Explode method of a region that happens to be empty,
;;, autocad throws a "Automation Error. Not applicable" excpetion, when what
;; we really want is just an empty list.  This function (safelyExplodeRegion) 
;; returns a
(defun safelyExplodeRegion
	(
		region
		/
		result
		returnValue
	)

	(setq result (vl-catch-all-apply 'vla-Explode (list region)))
	(if (vl-catch-all-error-p result)
		(progn
			; in this case, there was an exception, presumably due to having an empty region.
			;(princ "handled an exception when invoking a region's Explode method.\n")
			(setq returnValue (list ))
		)
		(progn
			(setq returnValue (gc:VariantToLispData result))
		)
	)
	
	returnValue
)
;============

;;When you explode a region using the default explode method, 
;; you get the lines,arcs, etc. that form the boundary of the region... UNLESS
;; the region is disjoint, in which case you get a set of regions (apparently all non-disjoint).
;; This function acts identically to the built-in Region.Explode() method with the exception that,
;;, for a disjoint region, this function returns the boundary lines of all the sub-regions instead
;; of the subregions themselves.  This function goes the extra mile and explodes subregions, unlike the
;; built-in Region.Explode() method.
(defun fullyExplodeRegion 
	(
		region
		/
		explosionProduct
		explosionProducts
		newExplosionProducts
		returnValue
		weNeedToLoop
		regionsToDelete
		result
	)
	
	(setq explosionProducts (safelyExplodeRegion region))
	
	
	;; In all likelihood, when you explode a disjoint region, the set of resulting objects never contains a disjoint region. However, out of an abundance of caution,
	;; I am allowing for the possibility that when you explode a disjoint region, you occasionally get further disjoint regions, requiring yet more exploding. 
	
	
	;;the built-in Region.Explode() method does not delete the region.  
	;; So, when we run this fullyExplodeRegion function, we expect to end up with the original region and the newly created lines/arcs/etc.
	;; In the case of a disjoint region, the intermediate explosions will result in left-over sub-regions, unless we explicitly delete them, 
	;; hence the regionsToDelete
	(setq regionsToDelete (list)) 
	(setq weNeedToLoop T)
	(while weNeedToLoop
		(setq newExplosionProducts (list ))
		(setq weNeedToLoop nil) ;; this will remain nil unless we encounter another region
		(foreach explosionProduct explosionProducts
			(if (= "AcDbRegion" (vla-get-ObjectName explosionProduct)) ;;if this explosion product is a region... ,
				(progn ;; ... then explode it and append the results to newExplosionPRoducts
					(setq newExplosionProducts 
						(append
							newExplosionProducts
							(safelyExplodeRegion explosionProduct)
						)
					)
					(setq weNeedToLoop T) ;;request another go-round through this loop to handle any regions that might have resulted from exploding this region.
					(setq regionsToDelete
						(append 
							regionsToDelete
							(list explosionProduct)
						)
					)
				)
				(progn ;;  ... else just append explosionProduct to newExplosionProducts
					(setq newExplosionProducts 
						(append
							newExplosionProducts
							(list
								explosionProduct
							)
						)
					)				
				)
			)
		)

		(setq explosionProducts newExplosionProducts)
	)
	
	(foreach regionToDelete regionsToDelete
		(vla-Delete regionToDelete)
	)
	
	(setq returnValue explosionProducts)
	returnValue
)
;;=======


;;takes a region as input
;;returns a list of polylines.
;; ideally, I would want this fuinction
;; to assign a rank to each polyline in such that way that 
;; (convertRegionToPolylines) and (convertPolylinesToRegions) could be inverses of one another,
;; but without help from Autocad's low-level API, it is difficult to figure out which boundary polygons are inside others,
;; and so I have not yet implemented such functionality.

;; 2017/01/05 TODO: refactor convertRegionToPolylines so that it does not rely on sending autocad commands to the console and so that it will work with a region in a space other than modelSpace (I am not sure if it is currently able to do this).
(defun convertRegionToPolylines
	(
		region
		/
		acadObj
		doc
		modelSpace
		;mySelectionSet
		explosionProducts
		explosionProduct
		originalPickFirst
		originalPickAdd
		i
		originalLayerOfRegion
		tempLayer
		tempLayerName
		layers
		newPolylinesSelectionSet
		newPolylines
		groups
		tempGroup
		tempGroupName
		originalPeditAccept
	)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))

	
	;; create a new temporary layer
	(setq tempLayerName 
		(strcat
			"tempLayer_"
			(rtos (fix (* (getvar "date") 3600 24 1000)))
		)
	)
	; (setq tempGroupName 
		; (strcat
			; "tempGroup_"
			; (rtos (fix (* (getvar "date") 3600 24 1000)))
		; )
	; ) ;;resutls in 'invalid group name' error.
	(setq tempGroupName "TEMPGROUP")
	(setq tempLayer (vla-Add layers tempLayerName))
	(setq tempGroup (vla-Add groups tempGroupName))
	
	
	;;store the original layer of region for later
	(setq originalLayerOfRegion (vla-get-Layer region))
	
	;; move region to tempLayer, so that when we explode the region into lines (arcs, etc.) and then join those lines 
	;; into polylines, the set of polylines that we want to return is precisely the set of polylines on tempLayer.
	(vla-put-Layer region tempLayerName)
	
	(setq explosionProducts (fullyExplodeRegion region))

	;;we may want to delete region at this point, or at least move it back to its original layer.
	(vla-put-Layer region originalLayerOfRegion)
	
	(setq newPolylines (list))
	(if (> (length explosionProducts) 0)   
		(progn
			(setq mySelectionSet (ssadd))
			(foreach explosionProduct explosionProducts
				(setq mySelectionSet 
					(ssadd  
						(handent (vla-get-Handle explosionProduct))
						mySelectionSet
					)
				)
			)
			(vla-AppendItems tempGroup (gc:ObjectListToVariant explosionProducts))   
			
			;;at this point, mySelectionSet is a selection set containing all the lines, arcs, etc, that were created when
			;; we exploded the region.  Now we want to join them together.
			
			;;at this point, tempGroup is a group containing all the lines, arcs, etc, that were created when
			;; we exploded the region.  Now we want to join them together.
			
			(setq originalPeditAccept (getvar "PEDITACCEPT"))	
			(setq originalPickAdd (getvar "PICKADD"))
			(setq originalPickFirst (getvar "PICKFIRST"))
			(setvar "PEDITACCEPT" 0)
			(setvar "PICKADD" 0)
			(setvar "PICKFIRST" 1)


			
			
			(command-s
				"pedit"
				"Multiple"
				"Group"
				tempGroupName
				""
				""
				"Join"
				""
				""
			)	
			(setvar "PICKADD" originalPickAdd)
			(setvar "PICKFIRST" originalPickFirst)
			(setvar "PEDITACCEPT" originalPeditAccept)
			;; It was very surprising to me that the JOIN command did not work here.
			;; I could not get JOIN to take a selection of multiple objects and join them together.
			;; it is as if the JOIN command that runs when invoike from a script with the (command ) function
			;; is a different command from that which runs when you manaully type in "JOIN" on the autocad command line.
			;; Specifically, the JOIN command as invoked by the (Command ) function seems to lack 
			;; the capability to deal with a selection of multiple objects -- it seems to want to start with a single 
			;; selected object.  I could almost make this work, except that in this single-object mode, join
			;;will not handle mixtures of arcs and lines -- only one or the other.
			;;Also, it seems that Document.SendCommand() behaves exactly like what you manually type at the command 
			;; string -- I was able to make JOIN work as expected when using Document.SendCommand(), but this 
			;; was problematic because Dodument.SendCommand() seems to wait until after the whole lisp script is finished before
			;; sending the command -- this does not work for me because I need to programmatically manipulate the objects that 
			;; the JOIN command creates, so I need to be able to execute more script AFTER the Join command has completed.
			
			;;Fortunately, the PEDIT command does appear to be identical whether invoked manually on the Command line or invoked
			;; via the (command ) function.  And Pedit can be made to do the joining that I want to do, although it is a bit more clunky.
			
			;; newPolylinesSelectionSet will contain everything on tempLayer
			
			(setq newPolylinesSelectionSet
				(ssget "_X" (list (cons 8 tempLayerName)))
			)
			
			
			
			(setq i 0)
			(while (< i (sslength newPolylinesSelectionSet))
				(setq newPolylines
					(append
						newPolylines
						(list (vlax-ename->vla-object (ssname newPolylinesSelectionSet i)))
					)
				)
				(setq i (+ 1 i))
			)
			

			
			(foreach item newPolylines
				(vla-put-Layer item originalLayerOfRegion )
			)
			
			;;(princ "(length newPolylines): ")(princ (length newPolylines)) (princ "\n")
		)
	)
	;;delete the temp layer
	;(vla-Delete (vla-Item layers tempLayerName))
	(vla-Delete tempLayer)
	
	;;delete the temp group
	;(vla-Delete (vla-Item groups tempGroupName))
	(vla-Delete tempGroup)
	
	
	;(vla-Update acadObj)
	;(vla-Update (vla-get-Application acadObj))
	
	; ; (setq newlyAddedEntities (list))
	
	; ; (setq currentEntity (handent handleOfCurrentEntity))
	; ; ;;collect all the newly added entities
	; ; (while (setq newlyAddedEntity (entnext currentEntity))
		; ; (setq currentEntity newlyAddedEntity)
		; ; (setq  newlyAddedEntities
			; ; (append
				; ; newlyAddedEntities
				; ; (list newlyAddedEntity)
			; ; )
		; ; )
	; ; )
	;; This does not seem to be working.  IDEA:: create a temporary layer, and move the objects to be joined
	;; to that layer before joining,  then after joining, the new polylines will be the only thing on that
	;; new layer.
	
	; ; (princ "(length newlyAddedEntities): ") (princ (length newlyAddedEntities)) (princ "\n")
	
	;;at this point, newlyAddedEntities should be a list of the enames of all the objects added as a result of the above join
	;; command
	
	; ; (foreach newlyAddedEntity newlyAddedEntities
		; ; (vlax-dump-object (vlax-ename->vla-object newlyAddedEntity))
	; ; )
	; ; newlyAddedEntities
	
	
	;;(command "JOIN")
	
	
	;;
	;; I found that running (setvar "pickfirst"... ) and (setvar "pickadd" ...) here caused the above JOIN
	;; command to run as if nothing was selected.  I suspect that vla-SendCommand is somehow asynchronous with other
	;; commands.  (command "JOIN") was even worse -- I could never get it to run and see the currently selected
	;; objects -- it always behaved as if no objects were selected.
	

	
	newPolylines
)
;=================

;;takes a polyline (expected to be closed and not self-intersecting,)
;; and converts it into a region.  Also, assigns the rank of the polyline to
;; the region.  (copying the rank is the only reason to use this function instead of  
;; using the built-in ModelSpace.AddRegion() function directly.
;; We might alternately call this function 'rankwiseConvertPolylineToRegion'.
(defun convertPolylineToRegion
	(
		polyline
		/
		acadObj
		doc
		modelSpace
		layers
		groups
		region		
		returnValue
		regions
		owningSpace
		tempGroupName
		tempGroup
	)
	(setq acadObj (vlax-get-acad-object))
    ;(setq doc (vla-get-ActiveDocument acadObj))
    (setq doc (vla-get-Document polyline))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))
	
	
	(setq owningSpace (vla-ObjectIDToObject  doc (vla-get-OwnerID polyline)))
	
	(setq regions
		(gc:VariantToLispData
			(vla-AddRegion owningSpace  
				(gc:ObjectListToVariant 
					(list polyline)
				)
			)
		)
	)
	
	(if (/= 1 (length regions)) ;;if regions does not contain exactly one element...
		(princ 
			(strcat
				"Warning from (convertPolylineToRegion): "
				"The input polyline generated " (itoa (length regions)) " regions.  "
				"We were expecting exactly 1."
			)
		)
	)
	;;perhaps the best way to handle the case of regions containing more than one element would be to boolean combine all elements of regions together into a single region.
	
	(setq region (car regions))
	
	(setRank region (getRank polyline))
	
	(setq returnValue region)
	returnValue
)
;===========

(defun makeHatchFromRegion (region /
		document
		returnValue
		owningSpace
		hatch
		e
		explosionProduct
		explosionProducts
		tempGroup
		tempGroupName
		whiteSpace
		commandString
		waitCounter
		wait
		localActiveDocument
		pathOfLocalActiveDocument
		addslashes
		result
		remoteDocument
	)
	
	(defun wait (seconds / stop)
		(setq stop (+ (getvar "DATE") (/ seconds 86400.0)))
		(while (> stop (getvar "DATE")))
	) 
	
	;;I adapted the following addslashes function from Lee Mac's "escapeWildCards"
	(defun addslashes ( str )
		(vl-list->string
			(apply 'append
				(mapcar
				   '(lambda ( c )
						(if (member c (vl-string->list "\\"))
							(list (car (vl-string->list "\\")) c)
							(list c)
						)
					)
					(vl-string->list str)
				)
			)
		)
	)
	
	(setq document (vla-get-Document region))
	(setq owningSpace (vla-ObjectIDToObject  document (vla-get-OwnerID region)))
	(setq localActiveDocument (vla-get-ActiveDocument (vlax-get-acad-object)))
	(setq pathOfLocalActiveDocument (vla-get-FullName localActiveDocument))
	
	
	;;the code in the following (if nil...) block ought to work, but it fails in case the region contains a hole.  But we know that the ui "HATCH" command 
	;; behaves as desired.  Therefore, as a workaround, we will attempt to send commands to the document to create the hatch.
	(if nil 
		(progn
			(setq hatch 
				(vla-AddHatch owningSpace
					1 			;; patternType
					"SOLID" 	;; patternName
					:vlax-false	;; associativity
				)
			)
			(princ "now attempting to add a hatch to region with append outer loop. (")(princ (vla-get-Handle region))(princ ":")(princ (vla-get-ObjectName region))(princ ") : ")
			(setq e 
				(vl-catch-all-apply 
					'vla-AppendOuterLoop (list hatch (gc:ObjectListToVariant (list region)))
					;; this will throw an excpetion if region happens to be disjoint. (curiously the ui HATCH function allows you to select a disjoint region and works without error)
					;; if that happens, the fix is to explode region, which produces multiple
					;; non-disjoint regions.
					;; We cannot just explode region in all cases, because exploding a non-disjoint region 
					;; produces the boundary lines, arcs, etc. (Crazy)
				)
			)
			(if (vl-catch-all-error-p  e)
				(progn
					(princ " failed with exception: ")(princ (vl-catch-all-error-message e))(princ "\n")
					(princ "now attempting to add a hatch to region with append inner loop. (")(princ (vla-get-Handle region))(princ ":")(princ (vla-get-ObjectName region))(princ ") : ")
					(setq e 
						(vl-catch-all-apply 
							'vla-AppendInnerLoop (list hatch (gc:ObjectListToVariant (list region)))
							;; this will throw an excpetion if region happens to be disjoint. (curiously the ui HATCH function allows you to select a disjoint region and works without error)
							;; if that happens, the fix is to explode region, which produces multiple
							;; non-disjoint regions.
							;; We cannot just explode region in all cases, because exploding a non-disjoint region 
							;; produces the boundary lines, arcs, etc. (Crazy)
						)
					)
				)
			)
			(if (vl-catch-all-error-p  e)
				(progn
					(princ " failed with exception: ")(princ (vl-catch-all-error-message e))(princ "\n")
					;; if we get here, it means that vla-AppendOuterLoop threw an exception (presumably because the region is disjoint)
					;; so, we need to explode the region and try again.			
					(setq explosionProducts (gc:VariantToLispData (vla-Explode region)))
					(foreach explosionProduct explosionProducts
						(princ "now appending explosionProduct (")(princ (vla-get-Handle explosionProduct))(princ ":")(princ (vla-get-ObjectName explosionProduct))(princ ") as outer loop to hatch: ")
						(setq e 
							(vl-catch-all-apply 
								'vla-AppendOuterLoop (list hatch (gc:ObjectListToVariant (list explosionProduct)))
							)
						)
						(if (vl-catch-all-error-p  e)
							(progn
								(princ "failed with exception: ")(princ (vl-catch-all-error-message e))(princ "\n")
							)
							(progn 
								(princ " succeeded.")(princ "\n")
							)
						)
						(vla-Delete explosionProduct)
					)
					;; by making a copy of the region, then exploding the copy, and after creating the hatch, deleting the explosion poroducts, we are left with one hatch and one region (the original region) as desired.  If we did not create a copy of the region, we would be left with 5 regions, which breaks the pattern (Autocad is nice enough to deal intelligently with, and have a concept of, a disjoint region -- we might as well use this capability.)			)
					;; actually, we do not need to make a copy of the region before exploding it because the vlax explode fuinction (unlike the UI EXPLODE function) preserves the original object.
				)
				(progn 
					(princ " succeeded.")(princ "\n")
				)
			)
		)
	)
	
	
	(if nil
		(progn
			(setq tempGroupName "TEMPGROUP")
			;;(setq tempGroupName (GUID ))
			;;(setq tempGroupName  (strcat "temp" (vl-string-subst "" "."  (rtos (getvar 'CDATE)))))
			(setq tempGroup 
				(vl-catch-all-apply 'vla-item (list (vla-get-Groups document) tempGroupName))
			)
			(if (vl-catch-all-error-p tempGroup)
				(progn 
					(setq tempGroup (vla-Add (vla-get-Groups document) tempGroupName))
				)
				(progn
					;remove all items from the group, just in case it was left over
					(vlax-for entity tempGroup
						(vla-RemoveItems tempGroup (gc:ObjectListToVariant (list entity)))
					)
				)
			)
			(vla-AppendItems tempGroup (gc:ObjectListToVariant (list region)))
			
			
			;; we will use the system variable "USERS1" (there are variables USERS1, USERS2, ... , USERS5 which serve as slots for custom data)
			;; as a way to communicate with the document
			(vla-SetVariable document "USERS1" "")
			;;
			(setq whiteSpace " ")
			; (setq commandString 
				; (strcat
					; "(progn"                                                                                  whiteSpace
						; "(setq dwl nil)"                                                                      whiteSpace
						; "(vla-Activate "                                                                      whiteSpace
							; "(cdr"                                                                            whiteSpace
								; "(assoc (strcase \"" (addSlashes pathOfIntitiallyActiveDocument) "\")"        whiteSpace
									; "(vlax-for doc (vla-get-documents (vlax-get-acad-object))"                whiteSpace              ;this evaluates to a dotted list whose elements are of the form (<documentName> . <documentObject>)
										; "(setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))"   whiteSpace
									; ")"                                                                       whiteSpace
								; ")"                                                                           whiteSpace
							; ")"                                                                               whiteSpace
						; ")"                                                                                   whiteSpace
                    ; ")"					                                                                      whiteSpace
					; "\n"
				; )
			; )
			; (princ "commandString: ") (princ commandString)(princ "\n")(princ)
			; (if writeToLog
				; (progn
					; (writeToLog "\n")(writeToLog "commandString: ") (writeToLog commandString)(writeToLog "\n")
				; )
			; )
			; (vla-SendCommand document commandString)
			; (vla-Activate document)
			
			
			(setq commandString 
				(strcat
					"(progn "
					"("                                                                                  whiteSpace
						"command"                                                                        whiteSpace 
						;;"command-s"                                                                      whiteSpace 
						;;"vl-cmdf"                                                                         whiteSpace 
						"\"_-HATCH\""                                                                     whiteSpace
						"\"Select\""                                                                      whiteSpace
						"\"Group\""                                                                       whiteSpace
						"\"" tempGroupName "\""                                                           whiteSpace
						"\"\""                                                                            whiteSpace
						"\"Properties\""                                                                  whiteSpace
						"\"Solid\""                                                                       whiteSpace
						"\"Advanced\""                                                                    whiteSpace
						"\"Associativity\""                                                               whiteSpace
						"\"No\""                                                                          whiteSpace
						"\"Style\""                                                                       whiteSpace
						"\"Normal\""                                                                      whiteSpace
						"\"\""                                                                            whiteSpace
						"\"\""                                                                            whiteSpace
					")"	                                                                                  whiteSpace
					"(setvar \"USERS1\" (vla-get-Handle (vlax-ename->vla-object (entlast))))"             whiteSpace
					"(setq dwl nil)"                                                                      whiteSpace
					"(vla-Activate "                                                                      whiteSpace
						"(cdr"                                                                            whiteSpace
							"(assoc (strcase \"" (addSlashes pathOfIntitiallyActiveDocument) "\")"        whiteSpace
								"(vlax-for doc (vla-get-documents (vlax-get-acad-object))"                whiteSpace              ;this evaluates to a dotted list whose elements are of the form (<documentName> . <documentObject>)
									"(setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))"   whiteSpace
								")"                                                                       whiteSpace
							")"                                                                           whiteSpace
						")"                                                                               whiteSpace
					")"                                                                                   whiteSpace
					")"                                                                                   whiteSpace
					;"\n"
				)
			)
			(if writeToLog
				(progn
					(writeToLog "\n")(writeToLog "commandString: ") (writeToLog commandString)(writeToLog "\n")
				)
			)
			;; I am sending a nested lisp call, rather than sending the "_-HATCH" command directly via document.SendCommand(), because based on the
			;; description of the document::SendCommand() function in the manual ( http://help.autodesk.com/view/ACD/2016/ENU/?guid=GUID-E13A580D-04CA-46C1-B807-95BB461A0A57 ),
			;; it seems that SendCommand would wait for user input if I tried to send the "_-HATCH" command directly.  Hopefully, sending as a lisp call to the lisp function 
			;; "command-s" will prevent this from happening.  
			;; Unfortunately, wrapping in the lisp "command-s" function does not prevent the returning upon the HATCH command wanting user input.
			(setq finishMessage "aboutTo post command\n")
			(vla-PostCommand document commandString)
			(setq finishMessage "posted command\n")
			;;(getint "enter any integer to continue")
			;;(alert "have we done it?")
			;;(vla-PostCommand document "(setvar \"USERS1\" (vla-get-Handle (vlax-ename->vla-object (entlast))))")
			;;(getint "and again, enter any integer to continue")
			;(alert "and again")
			;; (princ "1  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "_-HATCH ")                                                                                               (princ "2  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Select ")                                                                                                (princ "3  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Group ")                                                                                                 (princ "4  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document (strcat tempGroupName whitespace))                                                                        (princ "5  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document " ")                                                                                                      (princ "6  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Properties ")                                                                                            (princ "7  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Solid ")                                                                                                 (princ "8  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Advanced ")                                                                                              (princ "9  CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Associativity ")                                                                                         (princ "10 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "No ")                                                                                                    (princ "11 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Style ")                                                                                                 (princ "12 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "Normal ")                                                                                                (princ "13 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document " ")                                                                                                      (princ "14 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "\n")                                                                                                     (princ "15 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document (strcat "(setvar\"USERS1\"\"" (vla-get-Handle (vlax-ename->vla-object (entlast))) "\" )"))                (princ "16 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			; (vla-PostCommand document "\n")                                                                                                     (princ "17 CMDNAMES: ")(princ "\"")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\"")(princ "\n")
			
			
			;;(vla-SendCommand document "(setvar \"USERS1\" (vla-get-Handle (vlax-ename->vla-object (entlast))))\n")
			
			(setq waitCounter 0)
			(while 
				(and 
					(= "" (vlax-variant-value (vla-GetVariable document "USERS1")))
					(< waitCounter 3)
				)
				(if writeToLog
					(progn
						 (writeToLog "waiting for USERS1 to be set, with waitCounter=")(writeToLog waitCounter)(writeToLog "\n")
					)
				)
				(wait 0.1)
				(setq waitCounter (+ 1 waitCounter))
			)
			(if writeToLog 
				(progn
					(writeToLog "waited ")(writeToLog (itoa waitCounter))(writeToLog " times for USERS1 to become non-empty string, namely \"")(writeToLog (vlax-variant-value (vla-GetVariable document "USERS1")))(writeToLog "\"")(writeToLog "\n")
				)
			)
			(princ "waited ")(princ (itoa waitCounter))(princ " times for USERS1 to become non-empty string, namely \"")(princ (vlax-variant-value (vla-GetVariable document "USERS1")))(princ "\"")(princ "\n")
			;;(princ "CMDACTIVE: ")(princ (vlax-variant-value (vla-GetVariable document "CMDACTIVE")))(princ "\n")
			;;(princ "CMDNAMES: ")(princ (vlax-variant-value (vla-GetVariable document "CMDNAMES")))(princ "\n")
			(princ)
			; (setq hatch 
				; (vlax-ename->vla-object (entlast))
			; )
			
			(if (/= "" (vlax-variant-value (vla-GetVariable document "USERS1")))
				(progn
					(setq hatch 
						(vl-catch-all-apply 
							'vla-HandleToObject (list document (vlax-variant-value (vla-GetVariable document "USERS1")))
						)
					)
					(if (vl-catch-all-error-p hatch)
						(progn
							(princ "handleToObject failed with exception: ")(princ (vl-catch-all-error-message hatch))(princ "\n")
							(setq hatch nil)
						)
						(progn
							(princ "handleToObject succeeded and you should now have a hatch.\n")
							(if writeToLog
								(progn
									(writeToLog "handleToObject succeeded and you should now have a hatch, namely ")(writeToLog hatch)(writeToLog "\n")
								)
							)
						)
					)
				)
			)
			
			;(vla-Delete tempGroup)
		)
	)
	
	(if T
		(progn
			;; in strategy 1 above, wherin we tried to orchestrate the generation of the hatch enitrely through calls to the ActiveX objects, we were stymied by the fact that the Hatch object's function for adding geometry to the hatch (AppendOuterLoop) does not accept a region that has a hole.
			;; in strategy 2 above, wherein we attempted to send commands to the document containing the region for whih to create a hatch, we were stymied by the fact that the functions for sending commands to a document (SendCommand) and PostCommand) both cause the target document to become active, which in
			;; turn causes this very script to stop running (or, with SendCommand (sort of), to pause and wait for its owning document to become active once again).
			;; so here we are in strategy 3, wherein we will use Document::CopyObjects to copy the region into the currently active document, where we will generate the hatch by sending commands with the old-style autolisp (command-s ) function,
			;; (which we know works), and then we will use copyobjects once again to copy the resultant hatch into the original document.
			

			(setq hatch 
				(vla-AddHatch owningSpace
					1 			;; patternType
					"SOLID" 	;; patternName
					:vlax-false	;; associativity
				)
			)
			(princ "now attempting to add a hatch to region with append outer loop. (")(princ (vla-get-Handle region))(princ ":")(princ (vla-get-ObjectName region))(princ ") : ")
			(setq e 
				(vl-catch-all-apply 
					'vla-AppendOuterLoop (list hatch (gc:ObjectListToVariant (list region)))
					;; this will throw an excpetion if region happens to be disjoint. (curiously the ui HATCH function allows you to select a disjoint region and works without error)
					;; if that happens, the fix is to explode region, which produces multiple
					;; non-disjoint regions.
					;; We cannot just explode region in all cases, because exploding a non-disjoint region 
					;; produces the boundary lines, arcs, etc. (Crazy)
				)
			)
			(if (vl-catch-all-error-p  e)
				(progn
					(princ " failed with exception: ")(princ (vl-catch-all-error-message e))(princ "\n")
					(princ "now attempting to add a hatch to region using the copyObjects strategy using the currently active document as a workspace.\n")
					; get rid of the failed hatch that we created above
					(vla-Delete hatch)
					(vlax-release-object hatch)
					(setq hatch nil)	
					(setq result 
						(vla-CopyObjects 
							(vla-get-Document region) 			        ; the database whose "CopyObjects" method we are calling (this is the database from which we are copying things)
							(gc:ObjectListToVariant (list region))		; the list of objects to be copied
							(vla-get-ModelSpace localActiveDocument) 	; the owner to whom thses objects will be copied
						)
					)
					;;if everything worked as expected, result will be a variant array of the new entities, which should consist of exactly one entity (the newly created region).
					(setq localRegion (nth 0 (gc:VariantToLispData result)))
					
					(setq tempGroupName "TEMPGROUP")
					;;(setq tempGroupName (GUID ))
					;;(setq tempGroupName  (strcat "temp" (vl-string-subst "" "."  (rtos (getvar 'CDATE)))))
					(setq tempGroup 
						(vl-catch-all-apply 'vla-item (list (vla-get-Groups localActiveDocument) tempGroupName))
					)
					(if (vl-catch-all-error-p tempGroup)
						(progn 
							(setq tempGroup (vla-Add (vla-get-Groups localActiveDocument) tempGroupName))
						)
						(progn
							;remove all items from the group, just in case it was left over
							(vlax-for entity tempGroup
								(vla-RemoveItems tempGroup (gc:ObjectListToVariant (list entity)))
							)
						)
					)
					(vla-AppendItems tempGroup (gc:ObjectListToVariant (list localRegion)))
					
					(command-s    ;;"vl-cmdf"   ;;command                                                        
						"_-HATCH"                                                                
						"Select"                                                                 
						"Group"                                                                  
						tempGroupName                                                     
						""                                                                       
						"Properties"                                                             
						"Solid"                                                                  
						"Advanced"                                                               
						"Associativity"                                                          
						"No"                                                                     
						"Style"                                                                  
						"Normal"                                                                 
						""                                                                       
						""                                                                       
					)
					(setq localHatch (vlax-ename->vla-object (entlast))) ;;to do: record the initial value of entlast, and make sure that this value is different and that it corresponds to a region, as a check that the above command-s succeeded in creating a region.
					(vla-Delete tempGroup)
					
					;;copy localHatch to the original document
					(setq result 
						(vla-CopyObjects 
							(vla-get-Document localHatch) 						; the database whose "CopyObjects" method we are calling (this is the database from which we are copying things)
							(gc:ObjectListToVariant (list localHatch))		     ; the list of objects to be copied
							(vla-ObjectIdToObject (vla-get-Document region) (vla-get-OwnerID region)) 	; the owner to whom thses objects will be copied
						)
					)
					;;if everything worked as expected, result will be a variant array of the new entities, which should consist of exactly one entity (the newly created hatch).
					(setq hatch (nth 0 (gc:VariantToLispData result)))
					;;delete localHatch and localRegion
					(vla-Delete localHatch)
					(vla-Delete localRegion)
				)
				(progn
					(princ "succeeded.\n")
				)
			)
		)
	)
	
	
	(setq returnValue hatch)
	returnValue
)

;; rankwiseCombineRegions() takes as an argument a set of regions, and produces as output a single region (not sure yet if the returned region should itself have a rank).
;; Actually, the input list can contain a mixture of regions and polylines.. each polyline will be converted into a region using convertPolylineToRegion.
;; rankwiseCombineRegions() reads the rank property (if defined -- see polygonRanking.lsp, rank is a custom property that I invented and am implementing using extended Data attached to each region.) 
;; of each region in the input set.  regions which do not have a specified rank property are assumed to be rank 0.
;; The returned region is constructed as a boolean combination of the input regions as follows:
;; Take the boolean sum of all rank 0 regions.
;; From this subtract all rank 1 regions.
;; To this, add all rank 2 regions.
;; From this, subtract all rank 3 regions.
;; etc. (continue the pattern until all regions in the set have been accounted for.)

;;the argument is a list, each element of which is either a polyline or a region or a list of the form (rank object).  If an element is of the latter form, the rank of the object that is stored in the object's xdata will be ignored and the object's rank will be taken to be rank.  This provides a way to override the rank attached to an object.
(defun rankwiseCombineRegions
	(
		regionsArg
		/
		acadObj
		doc
		modelSpace
		layers
		groups	
		returnRegionHasBeenInitialized	
		returnRegion
		returnValue
		ranks
		region
		thisRank
		theseRegions
		rank
		newRegions
		regions
		item
		rankedRegions
		newRankedRegions
	)
	

	

	;;we really ought to handle the case where regionsArg is empty.
	;;(princ regionsArg)
	;;(princ (mapcar '(lambda (x) (listp x)) regionsArg))
	
	; remove any element of regionsArg that is nil or ay element that is a list and has its second element being nil.
	;(princ "before filtering nils, regionsArg: ")(princ regionsArg)(princ "\n")
	(setq regionsArg
		(vl-remove-if 
			'(lambda (x) 
				(or 
					(not x) 
					(and 
						(= (type x) 'LIST)
						(not (cadr x))
					)
				)
			)
			regionsArg
		)
	)
	;(princ "after filtering nils, regionsArg: ")(princ regionsArg)(princ "\n")
	(if (> (length regionsArg) 0)
		(progn	
			(setq rankedRegions
				(mapcar
					'(lambda (x / rank object) 
						(if (listp x)
							(progn
								(setq object (cadr x))
								(setq rank (car x))
							)
							(progn
								(setq object x)
								(setq rank (getRank x))
							)
						)
						(list rank object)
					)
					regionsArg
				)
			)
			
			;; at this point, rankedRegions is a list, each element of which is a list of the form {rank, object}, where object is a polyline (or circle) or region, and rank is the rank to be used for that object.
			;;(setq doc (vla-get-ActiveDocument acadObj))
			(setq doc (vla-get-Document (cadr (car rankedRegions))))
			
			(setq acadObj (vlax-get-acad-object))
			(setq modelSpace (vla-get-ModelSpace doc))
			(setq layers (vla-get-Layers doc))
			(setq groups (vla-get-Groups doc))
			;; convert any polylines or circles into true regions
			(setq rankedRegions
				(mapcar
					'(lambda 
						(x / rank object) 
						(setq rank (car x))
						(setq object (cadr x))
						(if 
							(or
								(= "AcDbPolyline" (vla-get-ObjectName object))
								(= "AcDbCircle" (vla-get-ObjectName object))
							)
							(progn
								(setq object (convertPolylineToRegion object))
							)
						)
						(list rank object)
					)
					rankedRegions
				)
			)
			
			;;remove any elements of rankedRegions which have a nil object (such objects could have been created above by convertPolylineToRegion)
			(setq rankedRegions
				(vl-remove-if
					'(lambda (x) (not (cadr x)))
					rankedRegions
				)
			)
			;; at this point, rankedRegions is a list, each element of which is a list of the form {rank, region}, where region is a region, and rank is the rank to be used for that region.
			
			(if nil (progn ;;deprecated code from prior to the advent of ranks that are lists
				(setq regions regionsArg)
				;; convert any polylines that are in regions into true regions.
				(setq newRegions (list))
				(foreach item regions 
					(if 
						(or
							(= "AcDbPolyline" (vla-get-ObjectName item))
							(= "AcDbCircle" (vla-get-ObjectName item))
						)
						(progn
							(setq region (convertPolylineToRegion item))
						)
						(progn
							(setq region item)
						)
					)
					(if region ;; this 'if' here is to exclude a nil region that might have been generated by convertPolylineToRegion.
						(setq newRegions
							(append
								newRegions
								(list region)
							)
						)
					)
				)
				
				(setq regions newRegions)
				
					
						; 
				
				(setq talliedRegions (list)) ;; talliedRegions will be a list, each element is of the form (<rank> . <listOfRegions>), where <listOFRegions> contains all the regions of the specified rank.
				;;construct talliedRegions, which we want to be sorted by ascending rank, so that when we run foreach, 
				;; below, we will process the regions starting with rank 0.
				
				;; populate ranks, a list of all ranks that appear in regions.
				(setq ranks (list ))
				(foreach region regions
					(setq thisRank (getRank region))
					(if (not (member thisRank ranks))
						(setq ranks (cons thisRank ranks))
					)
				)
				(setq ranks (vl-sort ranks '<)) ;; sort ranks ascending.
				
				(foreach rank ranks
					(setq talliedRegions
						(append
							talliedRegions
							(list (cons rank (list )))
						)
					)
				)
				;;;(princ "\n")(princ talliedRegions)(princ "\n")(quit)
				

				;;at this point talliedRegions has all the ranks in order, but all the lists of regions belonging to each rank are empty and need to be filled in...
				

				
				(foreach region regions
					(setq thisRank (getRank region))
					
					;;add thisRegion to the appropriate sub list of talliedRegions.
					(setq talliedRegions
						(subst 
							;; 1. replacement: 
							(cons thisRank
								(append
									(cdr (assoc thisRank talliedRegions))
									(list region)
								)
							)
							
							;; 2.  needle: 
							(assoc thisRank talliedRegions)
							
							;; 3.  haystack:
							talliedRegions 
						)
					)
				)
				
				;; At this point, talliedRegions is fully formed and ready to use.
				;;;(princ "\n")(princ talliedRegions)(princ "\n")(quit)
				
					
				
			))
			;===========
			
			;primeRanks will be a list of all the first elements of the ranks
			(setq primeRanks
				(LM:Unique
					(mapcar 
						'(lambda (x / rank) 
							(setq rank (car x))
							(car rank)
						)
						rankedRegions
					)
				)
			)
			(setq primeRanks (vl-sort primeRanks '<)) ;; sort primeRanks ascending.
			;;(princ "primeRanks: ")(princ primeRanks)(princ "\n")
			;==========
			
			;;sweep through primeRanks, and for each primeRank, deal with all the elements of rankedRegions that belong to that primeRank.
			(setq talliedRegions
				(mapcar
					'(lambda (primeRank / rankedRegionsHavingTheSpecifiedPrimeRank subRankedRegions theseRegions)
						;; collect all the regions in rankedRegions that have primeRank as the first element of their rank.
						(setq rankedRegionsHavingTheSpecifiedPrimeRank
							(vl-remove-if-not
								'(lambda (x / rank) (setq rank (car x)) (= primeRank (car rank)))
								rankedRegions
							)
						)
						
						;; theseRegions will be a list of regions.  theseRegions will consist of all the elements of subRankedRegions with a rank of length 1, and 
						;; one additional region which is the rankwiseCombine of the subRankedRegions whose rank has length > 1 (with the primeRank stripped).
						
						;; scan through rankedRegionsHavingTheSpecifiedPrimeRank and accumulate two lists:
						;;		theseRegions: Any regions whose rank contained only one element (namely primeRank) will go into theseRegions
						;;		subRankedRegions: Any regions whose rank contains more than one element will have the first element stripped from their rank (to form subrank), and then the {subrank, region} pair 
						;;			will be appended to subRankedRegions.
						;;			subRankedRegions will be suitable for passing into rankwiseCombineRegions (yes, this very function (it's re-entrant))
						(setq theseRegions (list ))
						(setq subRankedRegions (list ))
						(mapcar 
							'(lambda (x / rank subrank region) 
								(setq rank (car x)) ;;we already know that the first element of rank is primeRank, because of the filtering that we did above
								(setq subrank (cdr rank)) ;;subrank is the rest of rank (i.e. we strip the first element). Subrank may very well be nil (this happens if the length of rank is 1.)
								(setq region (cadr x))
								(if (> (length subrank) 0) ; if the length of the subrank is greater than zero (i.e. if the rank had more than one element)
									(progn
										;; append the {subrank, object} pair to subRankedRegions
										(setq subRankedRegions
											(append subRankedRegions
												(list 
													(list subrank region)
												)
											)
										)
									)
									(progn
										;; in this case, the length of subrank is zero (i.e. the rank contained only one element (namely, primeRank))
										;; append the region to theseRegions
										(setq theseRegions
											(append theseRegions
												(list 
													region
												)
											)
										)
									)
								)
							)
							rankedRegionsHavingTheSpecifiedPrimeRank
						)
						;==== 
						
						;; apply rankwiseCombine to the subRnkedRegions (if there are any), and append the result to theseRegions
						;; Oops.  That is not generally correct, because if the subRanked regions are subtractive, a rankwiseCombine produces a null region (or, in practice, exceptions, because null regions are not handled very well)
						;;  The correct approach is to rankwise combine all the subranked regions together with theseRegions (using rank zero for each of theseRegions).
						;; In other words, if we have some regions with rank (3), some regions with rank (3 4), and some regions with rank (3 4 5),
						;; we want to treat the rank (3) as rank (3 0 0) and the rank (3 4) as rank (3 4 0).
						(if (> (length subRankedRegions) 0) 
							;;generate consolidatedRegion, which will take the place of all theseRegions and all the subrankedregions in the final talliedList.
							(progn
								;;(princ "rankwiseCombineRegions is recursing with primeRank ")(princ primeRank)(princ ".\n")
								(setq consolidatedRegion
									(rankwiseCombineRegions  ;; THIS IS THE RE-ENTRY
										(append
												(mapcar '(lambda (x) (list (list 0) x)) theseRegions)
												subRankedRegions
										)
									)
									;;as rankwiseCombineRegions calls itself recursively, we will eventually reach a level where (length subRankedRegions) will be zero.
									;; This is guaranteed to eventually happen because we are stripping off the first element of the ranks on each iteration (i.e. reducing their length by 1).
									;; Once we get to the iteration where (length subRankedRegions) is zero, the above 'IF' condition will evaluate to false, and so we will stop recursing.
								)
								(setq theseRegions (list consolidatedRegion))
							)
							(progn
								;;(princ "rankwiseCombineRegions is skipping recursion for primeRank ")(princ primeRank)(princ ".\n")
							)
						)
						;=======
						
						; (list primeRank theseRegions)
						;;to my mind, the above makes more sense as an element of talliedRegions, but I will use the below to stay compatible with the rest of the code.
						(cons primeRank theseRegions)
					)
					primeRanks
				)	
			)
			;=========
			;; Beyond this point, the code did not need to be modified at all during the expansion from the single-integer rank concept to the list-of-integers rank concept.
			;; When we added the implementation of the list-of-integers rank concept, the construction of talliedRegions became a bit more complicated (requiring recursion, for instance), but 
			;; the modified code produces the same sort of talliedRegions list as the original code, and so the rest of the program digests the talliedRegions list
			;; just like it always has.
			;;(princ "talliedRegions: ")(princ talliedRegions)(princ "\n")
			
			
			(setq returnRegionHasBeenInitialized nil )
			(foreach item talliedRegions
				(setq thisRank (car item))
				(setq theseRegions (cdr item))
				
				(if (= 0 (rem thisRank 2)) ;;if thisRank is even...
					(progn ;; then we need to boolean add theseRegions
						;;initialise returnRegion if necessary
						(if (not returnRegionHasBeenInitialized)
							(progn
								(setq returnRegion (car theseRegions))
								(setq returnRegionHasBeenInitialized T)
								;; (setq theseRegions (cdr theseRegions))
								;;actually, the above discarding of the first member of theseRegions is not necessary --
								;; it doesn't hurt to boolean add the same region twice.
								;;ACTUALLY, Autocad complains when you try to union a region with itself, so we better trim the first element after all.
								(setq theseRegions (cdr theseRegions))
							)
						)
						(foreach  region theseRegions
							(vla-Boolean returnRegion acUnion region)
						)
					)
					(progn ;; else thisRank must be odd, in which case, we need to boolean subtract theseRegions
						(if (not returnRegionHasBeenInitialized)
							(progn
								;;in this case, we are trying to subtract regions from an empty region, which is perfectly valid, but does at least deserve a warning.
								(princ 	
									(strcat 
										"Warning: (rankwiseCombineRegions) was asked to subtract " 
										(itoa (length theseRegions)) " rank " (itoa thisRank) 
										" region(s) from a non-existent/empty region.  "
										"This subtraction request has been ignored." "\n"
									)
								)
							)
							(progn
								;;in this case, returnRegion has been initialized, so our task is to boolean subtract each of theseRegions from it.
								(foreach  region theseRegions
									(vla-Boolean returnRegion acSubtraction region)
								)	
							)	
						)
					)	
				)
			)
			
			;;at this point, returnRegion is the result that we were hoping for (note: returnRegion will be nil
			;; if there were no additive (i.e. even) rank regions in regions.  Autocad does not seem to have the concept
			;; of an empty region, which is what I really would prefere to return in that case, so nill will have to suffice.)
			
			;;I haven't yet considered what is to become of the input regions.  
			;; Ideally, I would want (rankwiseCombineRegions) to leave the input regions untouched.
			;; The acad documentation seems to imply that Region.Boolean() does not
			;; modify its argument, but that does not always appear to be the case -- specifically for subtrction operations, the region that is subtracted 
			;; seems to be consumed (i.e. becomes deleted) in the process.
			;; Perhaps I should
			;; create a working copy of input regions and build the returnRegion from this working copy, then delete the workingcCopy,
			;; in order to guard against the possibility that Region.Boolean() modifies its argument.
			(setq returnValue returnRegion)
		)
	)

	returnValue	
)
;=============

;; Unique  -  Lee Mac
;; Returns a list with duplicate elements removed.
(defun LM:Unique ( l )
    (if l (cons (car l) (LM:Unique (vl-remove (car l) (cdr l)))))
)
;==============

;; takes every polyline on layer 0 and rankwiseCombine's all of them into a singkle region, which 
;; we move to layer "Layer1"
(defun playground1
	(
	
		/
		acadObj
		doc
		modelSpace
		layers
		groups
		regionsToBeCombined
		region
		hatch
		tempGroupName
		tempGroup
		requiredLayers
		nameOfLayerOnWhichToPlaceResult
		entitiesToDelete
		entityToDelete
	)
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))
	(setq nameOfLayerOnWhichToPlaceResult "rankwiseCombination")
	
	
	(progn ;;create the required layers
		(setq requiredLayers 
			(list
				(list
					(cons 	"name" 		"0")
					(cons 	"color" 	"7")
				)
				(list	
					(cons 	"name" 		"rankwiseCombination")
					(cons 	"color" 	"Red")
				)
			)
		)
		;======
		
		(foreach layer requiredLayers
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
	;=============
	
	;;delete everything on LayerOnWhichToPlaceResult, to clean the slate.
	(setq entitiesToDelete (list ))
	(vlax-for entity modelSpace
		(if 
			(= (vla-get-Layer entity) nameOfLayerOnWhichToPlaceResult)	
			(progn
				;; append entity to entitiesToDelete
				(setq entitiesToDelete
					(append
						entitiesToDelete
						(list entity)
					)
				)
			)
		)
	)
	(foreach entityToDelete entitiesToDelete
		(vla-Delete entityToDelete)
	)
	
	(setq regionsToBeCombined (list))
	
	(vlax-for entity modelSpace
		(if ;;if entity is a polyline or circle on layer '0' ...
			(and
				(or
					(= "AcDbPolyline" (vla-get-ObjectName entity))
					(= "AcDbCircle" (vla-get-ObjectName entity))
				)
				(= "0" (vla-get-Layer entity))	
			)
			;; append entity to regionsToBeCombined
			(setq regionsToBeCombined
				(append
					regionsToBeCombined
					(list entity)
				)
			)
		)
	)
	;=======
	
	(setq region (rankwiseCombineRegions regionsToBeCombined))
	
	(princ "region: ")(princ region) (princ "\n")
	;;(vlax-dump-object region)
	(vla-put-Layer region nameOfLayerOnWhichToPlaceResult)
	

	
	;; fill the region in with a solid color
	; ; (setq hatch 
		; ; (vla-AddHatch modelSpace
			; ; 1 ;;patternType
			; ; "SOLID" ;;patternName
		; ; )
	; ; ) ;;modelSpace.AddHatch() won't do what I need it to do because it does
	;; not convert a Region into a hatch,  Instead, it requires you to build the hatch by adding inner and out loops.
	;; However, the command-line 'HATCH' command will convert a region into a hatch.
	;;we create a temp group to contain the region simply to facilitate 
	;; invoking the 'HATCH' command.
	(setq tempGroupName "TEMPGROUP")
	(setq tempGroup (vla-Add groups tempGroupName))
	(vla-AppendItems tempGroup (gc:ObjectListToVariant (list region)))
	
	(command-s
		"_-HATCH"
		;; we're now sitting at root of -HATCH menu
		
		"Select"
		"Group"
		tempGroupName
		""
		;; we're now back at root of -HATCH menu
		
		"Color"
		"ByLayer" ;"Red"
		;; we're now back at root of -HATCH menu
		
		"Properties"
		"Solid"
		;; we're now back at root of -HATCH menu
		
			"Advanced"
			;; we're now  at the 'Advanced' submenu of -HATCH menu
			"Associativity"
			"Yes"
			;; we're now  back at the 'Advanced' submenu of -HATCH menu
			"Style"
			"Normal" ;;the default style is 'Outer' --this is not the best option for converting a region to a hatch because islands within islands are npot hatched when Style is 'Outer'.  However, when 'Style' is 'Normal', the region is converted into a hatch exactly as expected.  --This drove me bat-shit until I figured it out.
			""
		;; we're now back at root of -HATCH menu
		
		
		"" ;; a final ENTER to execute the command.
	)
	
	(setq hatch 
		(vlax-ename->vla-object (entlast))
	)
	
	
	
	;(vlax-dump-object hatch)
	
	(vla-put-Layer hatch nameOfLayerOnWhichToPlaceResult)
	
	(vla-Delete tempGroup)
	(princ)
)
;=============

;;========= ACTIONS: =================
;;(convertRegionToPolylines (vlax-ename->vla-object (car (entsel))))
;(playground1)
; (rankwiseCombineRegions nil)
; (c:Example_AddRegion)