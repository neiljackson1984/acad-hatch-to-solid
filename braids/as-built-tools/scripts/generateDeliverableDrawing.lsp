(load "annotateDoorHinges5.lsp")

;; The starting point for this script is to have a COPY of the master dwg file open in Autocad.




(defun generateDeliverableDrawing
	(
		nameOfStartingBlock
		levelFilter ; if levelFilter (intended to be a list containing zero or more integers) is not nil, then any door or window that has a level property and for which the level property is not a member of levelFilter will be ignored.  Ths allows us to ignore doors and windows in transitional rooms which are not on the main level that we are drawing.
		/
		acadObj
		doc
		modelSpace
		layers
		groups
		blockDefinitions
		blockName
		blockDefinition
		weNeedToLoop
		tempLayer
		tempLayerName
		tempGroup
		tempGroupName


		entity
		i
		startTime
		endTime
		functionRunTime
		thisDoor
		thisDoorIdentifier
		thisWindow
		thisWindowIdentifier
		wallPolylines
		originalCmdEcho
		originalPeditAccept
		originalPickAdd
		originalPickFirst
		originalParameterCopyMode
		item
		ss
		mode
		interLevelTrimmingPolylines
		stairLandPolygon
		stairLandPolygons
		result
		thisTrimmedRegion
		rankOfStairLandPolygon
		layerOfStairLandPolygon
		entitiesToDelete
		attributeReference
		blockReferencesToBeSetAsConstruction
		blockReferencesToDelete
	)
	(setq startTime (getvar "date"))
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq modelSpace (vla-get-ModelSpace doc))
	(setq layers (vla-get-Layers doc))
	(setq groups (vla-get-Groups doc))
	(setq blockDefinitions (vla-get-Blocks doc))
	
	;; set up the modal system variables in a reversible way.
	(setq originalCmdEcho (getvar "CMDECHO"))
	(setq originalPeditAccept (getvar "PEDITACCEPT"))	
	(setq originalPickAdd (getvar "PICKADD"))
	(setq originalPickFirst (getvar "PICKFIRST"))
	(setq originalParameterCopyMode (getvar "PARAMETERCOPYMODE"))
	(setvar "CMDECHO" 0)
	(setvar "PEDITACCEPT" 0)
	(setvar "PICKADD" 0)
	(setvar "PICKFIRST" 1)
	(setvar "PARAMETERCOPYMODE" 0) ;; Do not copy any dimensional constraints or constraint parameters. Constraints are removed from copied objects. ;; I include this on the off-chance that it will prevent constraints and parameters from coming out of block references as they are exploded, although I don't think it has that effect.
	
	
	(setq tempGroupName "TEMPGROUP")
	(setq tempGroup (vla-Add groups tempGroupName))
	
	

	
	;;set every block reference to alignmentTarget to be bconstruction so that the alignment lines won't end up in the final drawing.
	(setq blockReferencesToBeSetAsConstruction (list ))
	(vlax-for blockDefinition blockDefinitions
		(vlax-for entity blockDefinition
			(if 
				(and 
					(= "AcDbBlockReference" (vla-get-ObjectName entity))
					(= "alignmentTarget" (vla-get-EffectiveName entity))
				)
				(progn
					(setq blockReferencesToBeSetAsConstruction (cons entity blockReferencesToBeSetAsConstruction))
					;;(princ "found an alignment target reference.\n")
				)
			)	
		)
	)
	(foreach blockReference blockReferencesToBeSetAsConstruction
		(setAsBconstruction blockReference)
	)
	(setq blockReferencesToBeSetAsConstruction nil)
	
	; ; ; ;;the above conversion to construction worked for all refs except the top level (actually, it worked for all, I just had to put applySelectiveTrimming after this part, rather than before)
	; ; ; ;;DELETE every block reference to alignmentTarget to be bconstruction so that the alignment lines won't end up in the final drawing.
	; ; ; (setq blockReferencesToDelete (list ))
	; ; ; (vlax-for blockDefinition blockDefinitions
		; ; ; (vlax-for entity blockDefinition
			; ; ; (if 
				; ; ; (and 
					; ; ; (= "AcDbBlockReference" (vla-get-ObjectName entity))
					; ; ; (= "alignmentTarget" (vla-get-EffectiveName entity))
				; ; ; )
				; ; ; (progn
					; ; ; (setq blockReferencesToDelete (cons entity blockReferencesToDelete))
					; ; ; ;;(princ "found an alignment target reference.\n")
				; ; ; )
			; ; ; )	
		; ; ; )
	; ; ; )
	; ; ; (foreach blockReference blockReferencesToDelete
		; ; ; (if (not (vlax-erased-p blockReference))
			; ; ; (progn
				; ; ; (vla-Delete blockReference)
			; ; ; )
		; ; ; )
	; ; ; )
	
	
	;; we have to do selectiveTrimming AFTER our hack to convert alignmentTarget blockReferences to construction.
	(applySelectiveTrimming)
	;; warning: applySelectiveTrimming may modify some polylines (which is fine), but, furthermore, the modified version of the polylines may in fact be region objects, so we need to 
	;; be thinking in terms of regions and polylines below.
		
	
	
	;; reursively explode all block references.
	(if (not (= "no" doPhase1)) ;;this chunk of code takes a long time to execute, during testing, I only need to execute it once, and then I can work on iterating the remainder of the code -- that's why this chunk of code is separated.
		(progn 
			;; delete everything in modelspace
			(vlax-for entity modelSpace
				;;(vla-Delete entity) ;;strangely this did not work, giving "error: bad argument type: VLA-OBJECT nil"
				(vlax-invoke-method entity 'Delete )
			)

			;; insert a block reference to the starting block ("level1")
			(vla-InsertBlock modelSpace 
				(vlax-3D-point (list 0 0 0)) ;; insertion point
				nameOfStartingBlock ;; name
				1 ;;xScaleFactor
				1 ;;yScaleFactor
				1 ;;zScaleFactor
				0 ;;insertion angle
			)
			
			;; repeatedly: select all block references, explode, delete extraneous stuff 
			;; (constraints and dimensions, most importantly), 
			;; and repeat until no block references remain.
			(setq weNeedToLoop T)
			(setq i 0)
			(while weNeedToLoop
				(setq weNeedToLoop nil)
				
				(princ (strcat "Now performing round " (itoa i) " of block explosion." "\n"))
				;;explode all block references in modelSpace, flagging weNeedToLoop if any block references exist
				;;collect all block references into tempGroup::
				;;empty out the pre-existing tempGroup
				(vla-Delete tempGroup) ;;this may not be necessary
				(setq tempGroup (vla-Add groups tempGroupName))
				
				;;populate tempGroup with all the block references
				(vlax-for entity modelSpace
					(if (= "AcDbBlockReference" (vla-get-ObjectName entity))
						(progn
							(setq weNeedToLoop T)
							; (vla-Delete tempGroup) ;;this may not be necessary
							; (setq tempGroup (vla-Add groups tempGroupName))
							(vla-AppendItems tempGroup (gc:ObjectListToVariant (list entity)))
						)
					)
				)
				;;ought to clear the current selection here, just in case
				(command)(command)
				(command-s
					"._EXPLODE"
					"Group"
					tempGroupName
					""
				)

				;==========
				;;delete all dimensions and constraints
				(command-s 
					"._DELCONSTRAINT"
					"All"
					""
				)

				(setq i (+ i 1))
			)
			;===
		)
		;====
	)
	;========
	(if (= "no" doPhase2) (quit))
	;;At this point, all block references have been exploded.
	
	;; collect all polylines (AND REGIONS) to be rankwiseCombine'd later to create the wallRegion
	(setq wallPolylines (list))
	(vlax-for entity modelSpace
		(if 
			(and
				(or
					(= "AcDbPolyline" (vla-get-ObjectName entity))
					(= "AcDbCircle" (vla-get-ObjectName entity))
					(= "AcDbRegion" (vla-get-ObjectName entity))
				)
				(= "wallUnbroken" (vla-get-Layer entity))
			)
			(progn
				(setq wallPolylines
					(append
						wallPolylines
						(list entity)
					)
				)
			)
		)
	)
	

	
	
	
	
	(makeRequiredLayers)
	
	;;populate the door and window lists based on data loaded from file and object references
	;; from the current document.
	(loadData)
	(populateDoors)
	(populateWindows)
	(populateRooms)
	
	;; draw door and window icons
	(foreach item doors
		(progn
			(setq thisDoorIdentifier (car item))
			(setq thisDoor (cdr item))
			(setq thisDoorLevel (cdr (assoc "level" thisDoor)))
			(if (= 'STR (type thisDoorLevel)) (setq thisDoorLevel (read thisDoorLevel))) ;; thisDoorLevel is now either nil (if no 'level' property was defined in doors.json), or the result of evaluating the level property (as a string) if there was a level property.
			(if
				(or
					(not thisDoorLevel) ; if no level was defined for this entity
					(member thisDoorLevel levelFilter) ; or if a level was defined and the level matches the filter
				)
				(drawDoorIcon thisDoor)
			)
		)
	)
	

	(foreach item windows
		(progn
			(setq thisWindowIdentifier (car item))
			(setq thisWindow (cdr item))
			(setq thisWindowLevel (cdr (assoc "level" thisWindow)))
			(if (= 'STR (type thisWindowLevel)) (setq thisWindowLevel (read thisWindowLevel))) ;; thisWindowLevel is now either nil (if no 'level' property was defined in doors.json), or the result of evaluating the level property (as a string) if there was a level property.
			(if
				(or
					(not thisWindowLevel) ; if no level was defined for this entity
					(member thisWindowLevel levelFilter) ; or if a level was defined and the level matches the filter
				)
				(drawWindowIcon thisWindow)
			)
		)
	)
	
	(foreach item rooms
		(progn
			(setq thisRoomIdentifier (car item))
			(setq thisRoom (cdr item))
			(drawRoomAnnotation thisRoom)
		)
	)
	;=======

	;; The above sweeps through doors, windows, and rooms will have inserted a bunch of new block references -- 
	;; we need to do an attsync for each block definition for which a corresponding reference was just inserted in the 
	;; above sweeps.  Attsync fixes text mirroring problems, among other things.
	
	(if nil ;;it turns out that we can call attsync with an asterisk and cover all blocks, so no need to specify block names to attsync after all.
		(progn
			; ; ; (vlax-for blockDefinition blockDefinitions
					; ; ; (setq blockName (vla-get-Name blockDefinition))
				; ; ; (princ "blockName: ")(princ blockName)(princ "\n")
			; ; ; )
			
			;; collect all blocknames to which there is a currently a reference (I first thought of doing this with (vlax-for blockDefinition blockDefinitions ...), but that gives me the internal anonymous block definitions associated with all the dynamic block references. -- no good.
			(setq blockNames (list ))
			(vlax-for entity modelSpace
				(if 
					(and
						(= "AcDbBlockReference" (vla-get-ObjectName entity))
						(= :vlax-true (vla-get-HasAttributes entity) )
					)
					(progn
						(if (not (member (vla-get-EffectiveName entity) blockNames))
							(progn
								;(princ "(vla-get-EffectiveName entity): ")(princ (vla-get-EffectiveName entity))(princ "\n")
								;(princ "(vla-get-HasAttributes entity): ")(princ (vla-get-HasAttributes entity))(princ "\n")
								;(princ ".\n")
								(setq blockNames
									(append
										blockNames
										(list (vla-get-EffectiveName entity))
									)
								)
							)
						)
					)
				)
			)
			;;at this point, blockNames is a list of all the block names on which we need to run attsync
			(princ "(length blockNames): ")	(princ (length blockNames)) (princ "\n")
			(princ "blockNames: ")	(princ blockNames) (princ "\n")
			;;run attsync for each block name

			(foreach blockName blockNames
				(princ "blockName: ")(princ blockName)(princ "\n")
				(command)(command)
				(command "REGEN")
				(command)(command)
				(command-s
					"._ATTSYNC"
					"Name"
					blockName
				)		
			) 
		)
	)
	
	
	(if nil 
	(progn
	; (vl-cmdf "_.ATTSYNC" "Name" "*") ;; invoking "ATTSYNC" via (command-s ) did not work (caused access violation exception error), but using (vl-cmdf) works --go figure.
	
	; ;;Run TORIENT to align attribute references to modelspace orientation
	; (TOrientAllToZero)
	
	; ;;update all fields
	; (command)(command)
	; (vl-cmdf "._UPDATEFIELD" "All" "") ;; "UPDATEFIELD did not work when invoked via (command-s), but it did work via (vl-cmdf ) -- The finnickiness of invoking command-line commands is really starting to get irritating.
	; (progn (vl-cmdf "_.ATTSYNC" "Name" "*") (vl-cmdf "_.ATTSYNC" "Name" "*") (TOrientAllToZero)	(vl-cmdf "_.ATTSYNC" "Name" "*") (TOrientAllToZero) (vl-cmdf "._UPDATEFIELD" "All" ""))
	; (progn (vl-cmdf "_.ATTSYNC" "Name" "*") (vl-cmdf "_.ATTSYNC" "Name" "*") (TOrientAllToZero)	(vl-cmdf "_.ATTSYNC" "Name" "*")  (vl-cmdf "._UPDATEFIELD" "All" ""))
	;(progn (vl-cmdf "_.ATTSYNC" "Name" "*") (vl-cmdf "_.ATTSYNC" "Name" "*") (TOrientAllToZero)	  (vl-cmdf "._UPDATEFIELD" "All" ""))
	;(progn (vl-cmdf "_.ATTSYNC" "Name" "*") (TOrientAllToZero)	(vl-cmdf "._UPDATEFIELD" "All" ""))
	;(progn  (TOrientAllToZero)	(vl-cmdf "._UPDATEFIELD" "All" ""))
	; ; (progn 
		; ; (vl-cmdf "_.ATTSYNC" "Name" "*") 
		; ; (TOrientAllToZero) 
		; ; ;;(vl-cmdf "._UPDATEFIELD" "All" "")
	; ; )
	;;(vl-cmdf "_.ATTSYNC" "Name" "*") (TOrientAllToZero) ;;(vl-cmdf "._UPDATEFIELD" "All" "") 
	
	;;updateField seems to be causing 
	;;there is something going on in the above combination of commands (always appears immediately after running updatefield) that
	;; is causing th multiline attribute text to become strung together ona  single line.  I am not sure what it is, but if I avoid
	;; calling updatefield, the problem seems not to happen.  However, If I later on change a dynamic block proeprty value and then forcefully update
	;; fields (with updatefield) or even let an update happen naturally with REGEN., the multiline flattens out.
	;;I have sometimes but not always observed that a field will become static (non-field) text, but this observation may be a canard.
	;; Very weird!  Something about the field evaluation process strips newlines from the text, perhaps.
	
	;; Perhaps I should hard-convert all of the fields to text at the end of this script.  Yes, I think that is the thing to do.
	
	
	;; === EXPERIMENT WITH (StripMtext) ---
	;;		;; The following line of program code was in this script during the experiments.
	;;		(vl-cmdf "_.ATTSYNC" "Name" "*") 
	;;		
	;;  FIRST, I RAN (generateDeliverabelDrawing, but I had (StripMtext) and (TOrientAllToZero) commented out.
	;; THEN, I ran (setq myObject (vlax-ename->vla-object (car (nentsel)))), and I selected one of the attribute references
	;;  At this point, : 
	;;		(vla-get-HasExtensionDictionary myObject) returned :vlax-true
	;;		(vla-get-Count (vla-GetExtensionDictionary myObject)) returned 1
	;;		(keys (vla-GetExtensionDictionary myObject)) returned ("ACAD_FIELD")
	;;		(elist (vlax-vla-object->ename myObject)) returned 
	;;			(
	;;				(-1 . <Entity name: 7fff8126d30>)
	;;				(0 . "ATTRIB")
	;;				(5 . "C75A3")
	;;				(102 . "{ACAD_XDICTIONARY")
	;;				(360 . <Entity name: 7fff8126d40>)
	;;				(102 . "}")
	;;				(330 . <Entity name: 7fff8126d10>)
	;;				(100 . "AcDbEntity")
	;;				(67 . 0)
	;;				(410 . "Model")
	;;				(8 . "text")
	;;				(100 . "AcDbText")
	;;				(10 923.97 1454.51 0.0)
	;;				(40 . 11.52)
	;;				(1 . "ahoy\\PSILL HEIGHT: 2'-1\"\\PVERTICAL EXTENT: 3'-4\"\\PHORIZONTAL EXTENT: 4'-7\"\\PWALL THICKNESS: 0'-10\"")
	;;				(50 . 4.71239)
	;;				(41 . 1.0)
	;;				(51 . 0.0)
	;;				(7 . "Standard")
	;;				(71 . 0)
	;;				(72 . 1)
	;;				(11 918.21 863.35 0.0)
	;;				(210 0.0 0.0 1.0)
	;;				(100 . "AcDbAttribute")
	;;				(280 . 0)
	;;				(2 . "WINDOWANNOTATION")
	;;				(70 . 0)
	;;				(73 . 0)
	;;				(74 . 2)
	;;				(280 . 1)
	;;				(71 . 2)
	;;				(72 . 1)
	;;				(11 918.21 863.35 0.0)
	;;				(101 . "Embedded Object")
	;;				(10 918.21 863.35 0.0)
	;;				(40 . 11.52)
	;;				(41 . 0.0)
	;;				(46 . 0.0)
	;;				(71 . 5)
	;;				(72 . 5)
	;;				(1 . "window12\\PSILL HEIGHT: 2'-2 1/2\"\\PVERTICAL EXTENT: 4'-4\"\\PHORIZONTAL EXTENT: 9'-8 1/2\"\\PWALL THICKNESS: 0'-6 1/2\"")
	;;				(7 . "Standard")
	;;				(210 0.0 0.0 1.0)
	;;				(11 -1.83697e-016 -1.0 0.0)
	;;				(42 . 237.464)
	;;				(43 . 89.3573)
	;;				(50 . 4.71239)
	;;				(73 . 1)
	;;				(44 . 1.0)
	;;				(-3
	;;				  (
	;;					"AcadAnnotative"
	;;					(1000 . "AnnotativeData")
	;;					(1002 . "{")
	;;					(1070 . 1)
	;;					(1070 . 0)
	;;					(1002 . "}")
	;;				  )
	;;				  (
	;;					"AcDbBlockRepETag"
	;;					(1070 . 1)
	;;					(1071 . 5)
	;;					(1005 . "0")
	;;				  )
	;;				)
	;;			)
	;;
	;;
	;;		Then, I ran (StripMtext (ssget "_X") "D"), and the command window made it look like a bunch of fields had been processed.
	;;		At this point,
	;;		(vla-get-HasExtensionDictionary myObject) returned :vlax-false
	;; 		Strangely, I then run UPDATEFIELD, and 155 fields are still found (I'm not sure where these fields are exactly.).
	;;		If I select any one of the window_standard block references and run "UPDATEFIELD", I get "5 field(s) found. 5 field(s) updated."
	;;		It is as if the fields are still present in the block, but there do not appear to be any fields in the attribute reference (no grey
	;;		backgrounds, and yes, the "FIELDDISPLAY" sysvar is set to 1, so I would expect to see grey backgrounds if fields were present.)
	;;		This is less than satisfying, intellectually, but at last my present goal of preventing the deletion of newlines from the attribgute text
	;;		seems to be achieved.
	)
	)
	;====
	
	
	(vl-cmdf "_.ATTSYNC" "Name" "*") 
	(StripMtext (ssget "_X") "D") ;;this converts fields into static text.
	;;(TOrientAll (/ PI 2)) 
	(TOrientAll 0) 

	;;(quit)
	;; explode all "wallOpeningEdge" block references (which were just generated by inserting door and window icons) (this is the block that contains the trimming tool)
	(vla-Delete tempGroup) ;;this may not be necessary
	(setq tempGroup (vla-Add groups tempGroupName))
	
	;;populate tempGroup with all the block references
	(setq trimToolsSelectionSet (ssadd))
	(vlax-for entity modelSpace
		(if 
			(and
				(= "AcDbBlockReference" (vla-get-ObjectName entity))
				(= "wallOpeningEdge" (vla-get-EffectiveName entity))
			)
			(progn
				(vla-Delete tempGroup) ;;this may not be necessary
				(setq tempGroup (vla-Add groups tempGroupName))
				(vla-AppendItems tempGroup (gc:ObjectListToVariant (list entity)))
				(sssetfirst nil ) 
				(command)(command)
				(command-s
					"._EXPLODE"
					"Group"
					tempGroupName
					;""
				)
				(sssetfirst nil ) 
				(command)(command)
				(command-s 
					"._DELCONSTRAINT"
					"All"
					""
				)
			)
		)
	)

	; ; ; ; ;;(princ (strcat "FOUND " (itoa (vla-get-Count tempGroup)) " wallOpeningEdge block references." "\n"))
	; ; ; ; ;;ought to clear the current selection here, just in case


	; ; ; ; ; (command-s
		; ; ; ; ; "._EXPLODE"
		; ; ; ; ; "Group"
		; ; ; ; ; tempGroupName
		; ; ; ; ; ""
	; ; ; ; ; )
	
	; ; ; ; ; ; ;;THE above explode statement did not work, not sure why, therefore
	; ; ; ; ; ; ;; I am manually selecting.
	; ; ; ; ; ; (command-s) 
	; ; ; ; ; (setvar "PICKADD" 2)
	; ; ; ; ; (command-s
		; ; ; ; ; "._SELECT"
		; ; ; ; ; "Group"
		; ; ; ; ; tempGroupName
		; ; ; ; ; ""
	; ; ; ; ; )
	; ; ; ; ; (command "._EXPLODE")
	; ; ; ; (sssetfirst nil ) 
	; ; ; ; (command)(command)
	; ; ; ; (command-s
		; ; ; ; "._EXPLODE"
		; ; ; ; "Group"
		; ; ; ; tempGroupName
		; ; ; ; ""
	; ; ; ; )
	; ; ; ; (quit)
	; ; ; ; (princ "(getvar \"CMDACTIVE\"): ")(princ (getvar "CMDACTIVE"))(princ "\n")
	; ; ; ; (setq i 0)
	; ; ; ; (while (= (getvar "CMDACTIVE") 1)
		; ; ; ; (command "")
		; ; ; ; (setq i (+i 1))
	; ; ; ; )
	; ; ; ; (princ (strcat "Had to actively clear the command line using " (itoa i) " invocations of (command \"\")." "\n"))
	; ; ; ; (princ "(getvar \"CMDACTIVE\"): ")(princ (getvar "CMDACTIVE"))(princ "\n")
	; ; ; ; (command)(command)(command)
	; ; ; ; (sssetfirst nil ) ;;clear the current selection (I think)
	; ; ; ; (sssetfirst nil trimToolsSelectionSet)
	; ; ; ; (command "._EXPLODE")
	; ; ; ; ; (command-s
		; ; ; ; ; "._EXPLODE"
		; ; ; ; ; "Group"
		; ; ; ; ; tempGroupName
		; ; ; ; ; ;;""
	; ; ; ; ; )
	; ; ; ; (quit)
	; ; ; ; (command)
	; ; ; ; ;;the terminal empty string argument to command-s is quite confusing -- sometimes it seems to behave exactly as an Enter at the command
	; ; ; ; ;; line and sometimes it behaves like TWO enters -- weird.  Empirically, I have
	; ; ; ; ;; discovered that I can achieve my desired effect here by NOT having the 
	; ; ; ; ;; terminal empty string in the call to command-s above.
	; ; ; ; ;;I am hoping that this functions as an escape sent to the command line, to make sure there are not active selections nor active commands.
	; ; ; ; ;==========
	; ; ; ; ;;delete all dimensions and constraints
	; ; ; ; (command-s 
		; ; ; ; "._DELCONSTRAINT"
		; ; ; ; "All"
		; ; ; ; ;""
	; ; ; ; )
	; ; ; ; (quit)
	
	
	;;collect all walltrimming polylines
	;; and assign them a rank of 101 (which is subtractive, because it is odd) (I choose 101 because it is higher than the rank 0 and 1 wall polylines that I already have.
	(setq wallTrimmingPolylines (list))
	
	(vlax-for entity modelSpace
		(if 
			(and
				(= "AcDbPolyline" (vla-get-ObjectName entity))
				(= "trimTool" (vla-get-Layer entity))
			)
			
			(progn
				(setRank entity 101)
				(setq wallTrimmingPolylines
					(append
						wallTrimmingPolylines
						(list entity)
					)
				)
			)
		)
	)
	(princ (strcat "Found " (itoa (length wallTrimmingPolylines)) " wall trimming polylines." "\n"))
	
	(if nil (progn
		(setq interLevelTrimmingPolylines (list))
		(vlax-for entity modelSpace
			(if 
				(and
					(= "AcDbPolyline" (vla-get-ObjectName entity))
					(= "interLevelTrimming" (vla-get-Layer entity))
				)
				
				(progn
					;(setRank entity 14)
					(setq interLevelTrimmingPolylines
						(append
							interLevelTrimmingPolylines
							(list entity)
						)
					)
				)
			)
		)
		(princ (strcat "Found " (itoa (length interLevelTrimmingPolylines)) " interLevelTrimmingPolylines." "\n"))
	))
	
	
	;; cut openings in wall for doors and windows.
	
	;;set rank of interLevelTrimmingPolylines to 14
	;;(foreach item interLevelTrimmingPolylines	(setRank item 14))
	
	(setq wallRegion
		(rankwiseCombineRegions 
			(append
				wallPolylines
				wallTrimmingPolylines
			)
		)
	)
	
	
	

	
	
	(if nil ;; put a hatch on wallRegion, just for debugging so we can visualize the region.
		(progn
			;; put a hatch on wallRegion, just for debugging so we can visualize the region.
			(vla-Delete tempGroup) ;;this may not be necessary
			(setq tempGroup (vla-Add groups tempGroupName))
			(vla-AppendItems tempGroup (gc:ObjectListToVariant (list wallRegion)))
			(sssetfirst nil ) 
			(command)(command)
			(command-s
				"_-HATCH"
				;; we're now sitting at root of -HATCH menu
				
				"Select"
				"Group"
				tempGroupName
				""
				;; we're now back at root of -HATCH menu
				
				"Color"
				"Red"; "ByLayer" ;"Red"
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
		)
		;======
	)
	;============

	
	

	

	;;trim stairLand regions with wallRegion.
	;; collect all the stairLandPolygons (AND REGIONS)
	(setq stairLandPolygons (list))
	(vlax-for entity modelSpace
		(if 
			(and
				(member (vla-get-ObjectName entity) (list "AcDbPolyline" "AcDbCircle" "AcDbRegion"))
				(= "stairLands" (vla-get-Layer entity))
			)
			(progn
				(setq stairLandPolygons (cons entity stairLandPolygons))
			)
		)
	)
	(princ (strcat "Found " (itoa (length stairLandPolygons)) " stairLandPolygons." "\n"))
	(foreach stairLandPolygon stairLandPolygons
		;(princ "(vlax-erased-p stairLandPolygon): ")(princ (vlax-erased-p stairLandPolygon))(princ "\n")
		(setq rankOfStairLandPolygon (getRank stairLandPolygon))
		(setq layerOfStairLandPolygon (vla-get-Layer stairLandPolygon))
		;(princ "(vlax-erased-p wallRegion): ")(princ (vlax-erased-p wallRegion))(princ "\n")
		(setq copyOfWallRegion (vla-Copy wallRegion))
		(setq thisTrimmedRegion 
			(rankwiseCombineRegions
				(list
					(list 
						(list 0)
						stairLandPolygon
					)
					(list 
						(cons 1 (getRank wallRegion))
						copyOfWallRegion
					) 
				)
			)
		)
		(setRank thisTrimmedRegion rankOfStairLandPolygon)
		(vla-put-Layer thisTrimmedRegion layerOfStairLandPolygon)
		(if 
			(and
				(not (vlax-erased-p stairLandPolygon))
				(not (= (vla-get-ObjectID stairLandPolygon) (vla-get-ObjectID thisTrimmedRegion))) ;;I am not sure it would ver happen that entity and thisTrimmedRegion would ever point to the same object (maybe it could happen as a result of the way rankwiseCombineRegions works, not cloning the starting region in the combine), but if ti did happen, then we of course would not want to invoke entity's Delete method.  This check guards against this.
			)
			(progn
				(vla-Delete stairLandPolygon)
			)
		)
		(if 
			(and
				(not (vlax-erased-p copyOfWallRegion))
				(not (= (vla-get-ObjectID copyOfWallRegion) (vla-get-ObjectID thisTrimmedRegion))) ;;I am not sure it would ver happen that entity and thisTrimmedRegion would ever point to the same object (maybe it could happen as a result of the way rankwiseCombineRegions works, not cloning the starting region in the combine), but if ti did happen, then we of course would not want to invoke entity's Delete method.  This check guards against this.
			)
			(progn
				(vla-Delete copyOfWallRegion)
			)
		)
	)
	
	(if nil (progn  ;; trim the stairLands with interLevelTrimmingPolylines ;;update 2017/01/05: we are no longer doing this here.  trimmming is handled by (applySelectiveTrimming), invoiked above.
		;;set rank of interLevelTrimmingPolylines to 13 (subtractive) in preparation for combining with stairLAndPolygons
		(foreach item interLevelTrimmingPolylines	(setRank item 13))
		
		;; trim each stairLand polygon with the interLevelTrimmingPolylines
		;; note: we are not adding all the stairLands together into one big region; rather, we treat each stairLAndPolygon separately.
		(foreach thisStairLandPolygon stairLandPolygons
			(progn
				(setRank thisStairLandPolygon 10)
				(setq thisStairLandRegion
					(rankwiseCombineRegions 
						(append 
							(list thisStairLandPolygon) 
							(mapcar
								'(lambda (x) (list (list 10 1) x))
								interLevelTrimmingPolylines
							) ;;use the interLevelTrimmingPolylines but override the rank to (10 1)
						)
					)
				)
				;(vlax-dump-object thisStairLandRegion)
				(setq thisTrimmedStairlandPolylines (convertRegionToPolylines thisStairLandRegion))
				(foreach item thisTrimmedStairlandPolylines (vla-put-Layer item "stair"))
			)
		)
	))
	;==============
	
	;;convert any of the stairLandPolygons that is actually a region into a true polyline, and put it on the stair layer (all polygons or regions)
	;; collect all the stairLandPolygons (AND REGIONS) ;;have to do this again because the above trimming of stairLands may have changed the number of polygons.
	(setq stairLandPolygons (list))
	(vlax-for entity modelSpace
		(if 
			(and
				(member (vla-get-ObjectName entity) (list "AcDbPolyline" "AcDbCircle" "AcDbRegion"))
				(= "stairLands" (vla-get-Layer entity))
			)
			(progn
				(setq stairLandPolygons (cons entity stairLandPolygons))
			)
		)
	)
	(princ (strcat "Found " (itoa (length stairLandPolygons)) " stairLandPolygons." "\n"))
	(foreach stairLandPolygon stairLandPolygons
		;; move the stairLand polygon to the "stair" layer
		(vla-put-Layer stairLandPolygon "stair")
		(if 
			(= "AcDbRegion" (vla-get-ObjectName stairLandPolygon))
			(progn
				(setq result (convertRegionToPolylines stairLandPolygon))
				;;delete the original stairLAndPolygon (which was really a region)
				(if 
					(and
						(not (vlax-erased-p stairLandPolygon)) ;;check to make sure that stairLandPolygon has not been erased
						(not 
							(member 
								(vla-get-ObjectID stairLandPolygon) 
								;;(vla-get-ObjectID thisTrimmedRegion)
								(mapcar 'vla-get-ObjectID  result)
							)
						) ;;check to make sure that the object we are going to try to delete is not, for some strange reason, in the set of polylines that we just generated, which we definitely want to keep.  highly unlikely, but doesn't hurt to check before we delete.
					)
					(vla-Delete stairLandPolygon)
				)
			)
		)
	)
	
	
	;;convert wallRegion to polylines
	(setq finalWallPolylines
		(convertRegionToPolylines wallRegion)
	)
	(princ (strcat "(length finalWallPolylines): " (itoa (length finalWallPolylines)) "." "\n"))
	(foreach item finalWallPolylines
		(vla-put-Layer item "wall")
	)
	;============
	
	
	;;DELETE all leftover objects

	
	
	; (setq namesOfLayersWhoseContentsWeWantToKeep
		; (list ;; these are the layers we want to keep: 
			; "wall" 
			; "window" 
			; "door" 
			; "landing"
			; "stair"
			; ;;"stairLands"
			; "room_annotation" ;;this layer contains the roomAnnotation block references
			; ;;"text_roomAnnotation"  ;;nothing is directly on this layer in the root context, only within roomAnnotation block definition, and roomAnnotation block references are all on the "room" layer, which is already in this list, so we do not need to explicitly save the "text_roomAnnotation" layer.
			; ;;"text_windowAnnotation" ;;nothing is directly on this layer in the root context, only within window block definition, so we do not need to explicitly save it.
			; "0"
			; "stair_annotation"
			
		; )
	; )
	
	
	; ; ; (setq entitiesToDelete (list))
	; ; ; (setq i 0)
	; ; ; (vlax-for entity modelSpace
		; ; ; (setq weWantToKeepThisEntity nil)
		
		; ; ; (if 
			; ; ; (member (vla-get-Layer entity) namesOfLayersWhoseContentsWeWantToKeep)
			; ; ; (setq weWantToKeepThisEntity T)
		; ; ; )
		
		; ; ; (if (not weWantToKeepThisEntity)
			; ; ; (progn
				; ; ; ;;(vla-Delete entity)
				; ; ; (setq entitiesToDelete (cons entity entitiesToDelete))
				; ; ; (setq i (+ i 1))
			; ; ; )
		; ; ; )
	; ; ; )
	
	
	
	(setq namesOfLayersWhoseContentsWeWantToKeep
		(list ;; these are the layers we want to keep: 
			"wall" 
			"window" 
			"door" 
			"stair"
			"0"
			"stair_annotation"
		)
	)
	(setq entitiesToDelete (list))
	(vlax-for blockDefinition blockDefinitions
		(vlax-for entity blockDefinition
			(if 
				(or
					(not (member (vla-get-Layer entity) namesOfLayersWhoseContentsWeWantToKeep)) ;;if the entity is not on one of the layers whose contents we want to keep...
					(and ;;or if the entty is a block reference pointintg to the alignmentTarget block...
						(= "AcDbBlockReference" (vla-get-ObjectName entity))
						(= "alignmentTarget" (vla-get-EffectiveName entity))
					)
				)
				(progn
					(setq entitiesToDelete (cons entity entitiesToDelete))
				)
			)
			
			;;have to handle attributeReferences specially, because they do not show up like regular entities in a block definition.
			(if
				(and
					(= "AcDbBlockReference" (vla-get-ObjectName entity))
					(= :vlax-true (vla-get-HasAttributes entity))
				)
				(progn
					;;(princ "hasAttributes: ")(princ (if (= :vlax-true (vla-get-HasAttributes entity)) "True" "False"))(princ "\n")
					(foreach attributeReference (gc:VariantToLispData (vla-GetAttributes entity))
						(if 
							(not (member (vla-get-Layer attributeReference) namesOfLayersWhoseContentsWeWantToKeep))
							(progn
								(setq entitiesToDelete (cons attributeReference entitiesToDelete))
							)
						) 
					)
				)
			)	
		)
	)
	
	; ; ; (vlax-for entity modelSpace
			; ; ; (if 
				; ; ; (or
					; ; ; (not (member (vla-get-Layer entity) namesOfLayersWhoseContentsWeWantToKeep)) ;;if the entity is not on one of the layers whose contents we want to keep...
					; ; ; (and ;;or if the entty is a block reference pointintg to the alignmentTarget block...
						; ; ; (= "AcDbBlockReference" (vla-get-ObjectName entity))
						; ; ; (= "alignmentTarget" (vla-get-EffectiveName entity)) ;;oops -- all the block references to alignmentTarget were exploded long ago.  I will add a sweep up toward the top of generateDeliverableDrawing to ake all the block references to alignmentTarget bconstruction so that they will not be exploded.
					; ; ; )
					; ; ; (and ;;or if the entty is a block reference pointintg to an anonymous block definition (as indicated by Name begining with asterisk)
						; ; ; (= "AcDbBlockReference" (vla-get-ObjectName entity))
						; ; ; (= "*" (substr (vla-get-EffectiveName entity) 1 1))
					; ; ; )
				; ; ; )
				; ; ; (progn
					; ; ; (setq entitiesToDelete (cons entity entitiesToDelete))
				; ; ; )
			; ; ; )
			
			; ; ; ;;have to handle attributeReferences specially, because they do not show up like regular entities in a block definition.
			; ; ; (if
				; ; ; (= "AcDbBlockReference" (vla-get-ObjectName entity))
				; ; ; (progn
					; ; ; (foreach attributeReference (gc:VariantToLispData (vla-GetAttributes entity))
						; ; ; (if 
							; ; ; (not (member (vla-get-Layer attributeReference) namesOfLayersWhoseContentsWeWantToKeep))
							; ; ; (progn
								; ; ; (setq entitiesToDelete (cons attributeReference entitiesToDelete))
							; ; ; )
						; ; ; ) 
					; ; ; )
				; ; ; )
			; ; ; )	
	; ; ; )
	
	(foreach entity entitiesToDelete
		(if (not (vlax-erased-p entity))
			(progn
				(vla-Delete entity)
			)
		)
	)
	
	(princ "PURGING...\n")
	;;purge all, to get rid of extraneous layers and block definitions.
	(setvar "CLAYER" "0") ;; purge will not get rid of the layer that happens to be current, even if it is otherwise purgeable.
	(sssetfirst nil ) (command)(command)
	(command-s 
		"._-PURGE"
		"All" ;;types of objects to purge
		"*" ;;names to purge 
		"No" ;;verify each name to be purged?

	)
	;=========
	
	(if T (progn ;;second purge
		;;Strangely, whereas a single purge had always had the desired effect,
		;; today (2017/01/08) I noticed for the first time that a second purge was required to get rid of all the extra layers.
		;; I noticed this behavior right after I refactored the nesting of block references.  I suspect that my refactoring might have increased the nesting depth and that, 
		;; when the nesting depth gets too high, a second purge is required.  Very curious behavior.
		(princ "PURGING AGAIN...\n")
		;;purge all, to get rid of extraneous layers and block definitions.
		(setvar "CLAYER" "0") ;; purge will not get rid of the layer that happens to be current, even if it is otherwise purgeable.
		(sssetfirst nil ) (command)(command)
		(command-s 
			"._-PURGE"
			"All" ;;types of objects to purge
			"*" ;;names to purge 
			"No" ;;verify each name to be purged?

		)
	))
	;=========
	

	(deleteAllParameters)
	
	;;cleanup
	(if (not (vlax-erased-p tempGroup)) (vla-Delete tempGroup))
	(setvar "CMDECHO" originalCmdEcho)
	(setvar "PICKADD" originalPickAdd)
	(setvar "PICKFIRST" originalPickFirst)
	(setvar "PEDITACCEPT" originalPeditAccept)
	(setvar "PARAMETERCOPYMODE" originalParameterCopyMode)

	
	;;report duration of run time for this function.
	(setq endTime (getvar "date"))
	(setq functionRunDuration (- endTime startTime)) ;; functionRunDuration is in days.
	(princ 
		(strcat
			"generateDeliverableDrawing took "
			(rtos 
				(* 3600 24 functionRunDuration)
				2 
				2
			)
			" seconds to run."
			"\n"
		)
	)
	
	
	
	(princ)
)
;===========





;============= ACTIONS: ===============

;;mainly for debugging, I split the process up into two phases so I would not have to wait through both phases on every iteration.,
;; phase 1 is the nested explosion
;; phase 2 is the detection of door and window anchors and placement of door icons, window icons, and annotation text.
;;in production, both doPhase1 and doPhase2 should be "yes" (or nil - anything other than no)
(setq doPhase1 "yes")
(setq doPhase2 "yes")

;(generateDeliverableDrawing "level0_forRendering" (list 0))
;(generateDeliverableDrawing "level1_forRendering" (list 1))
(generateDeliverableDrawing "level2_forRendering" (list 2))
(princ)