; find all regions in the active space having three or four vertices (as are created by 
; doing hatch -> (convert to region using -hatchedit) -> <CONVTOMESH> -> <EXPLODE> -> <REGION> )
(load "variantArrayConversion.lsp")
(load "regionFunctions.lsp")


; (setq acadObj (vlax-get-acad-object))
; (setq doc (vla-get-ActiveDocument acadObj))
; (vla-AddItems 
    ; (vla-get-PickFirstSelectionSet doc)
    ; (gc:ObjectListToVariant 
        ; (list 
            ; (vlax-ename->vla-object (handent "3C2"))
        ; )
    ; )
; )


; (sssetfirst 
    ; nil
    ; (ssadd  
        ; (handent (vla-get-Handle (vlax-ename->vla-object (handent "3C2"))))
        ; (ssadd)
    ; )
; )

; (setq foo '(lambda (x / a b c) (alert x)))



; (apply
    ; foo
    ; (list 
        ; "hey there"
    ; )
; )

; (quit)

(command "._UNDO" "_Begin")


; (setq mesh (convertRegionToMesh (vlax-ename->vla-object (car (entsel)))))





(progn
    ; (progn ; variantArrayConversion.lsp
        ; ;;; copied on 2016/01/17 from http://www.theswamp.org/index.php?topic=31674.5;wap2 

        ; ;;;======================== VARIANTS & SAFEARRAYS ========================;;;

        ; ;; Variant -> LISP

        ; ;; gc:VariantToLispData
        ; ;; Converts a variant or a safearray into LISP data (list)
        ; ;;
        ; ;; Argument: var variant or safearray

        ; (defun gc:VariantToLispData (var)
          ; (cond
            ; ((= (type var) 'variant)
             ; (gc:VariantToLispData (vlax-variant-value var)))
            ; ((= (type var) 'safearray)
             ; (mapcar 'gc:VariantToLispData (vlax-safearray->list var))
            ; )
            ; (T var)
          ; )
        ; )

        ; ;; gc:2dVariantToPointList
        ; ;; Converts a variant of 2D coordinates into a 2d points list
        ; ;; LightweightPolyline: OCS coordinates
        ; ;;
        ; ;; Argument
        ; ;; var: a variant (array of doubles) as returned by vla-get-Coordinates

        ; (defun gc:2dVariantToPointList (var / foo)
          ; (defun foo (lst)
            ; (if lst
              ; (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
            ; )
          ; )
          ; (foo (vlax-safearray->list (vlax-variant-value var)))
        ; )

        ; ;; gc:3dVariantToPointList
        ; ;; Converts a variant of 3D coordinates into a 3d points list
        ; ;; 2d Polyline: OCS coordinates (Z = 0)
        ; ;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
        ; ;; PolygonMesh, Solid, Trace: WCS coordinates
        ; ;;
        ; ;; Argument
        ; ;; var: a variant (array of doubles) as returned by vla-get-Coordinates

        ; (defun gc:3dVariantToPointList (var / foo)
          ; (defun foo (lst)
            ; (if lst
              ; (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
            ; )
          ; )
          ; (foo (vlax-safearray->list (vlax-variant-value var)))
        ; )

        ; ;; gc:VariantsToDxfList
        ; ;; Returns an assoc list (DXF list type)
        ; ;;
        ; ;; Arguments
        ; ;; xtyp: variant (array of integers)
        ; ;; xval: varinat (array of variants)

        ; (defun gc:VariantsToDxfList (xtyp xval)
          ; (mapcar 'cons (gc:VariantToLispData xtyp) (gc:VariantToLispData xval))
        ; )

        ; ;; gc:GetXdata
        ; ;; Returns the object xadta list
        ; ;;
        ; ;; Arguments
        ; ;; obj: (vla-object) the object containing xdata
        ; ;; app: (string) the registred application name ("" for all)

        ; (defun gc:GetXdata (obj app / xtyp xval)
          ; (vla-GetXdata obj app 'xtyp 'xval)
          ; (gc:VariantsToDxfList xtyp xval)
        ; )

        ; ;; gc:GetXrecordData
        ; ;; Returns the xrecord object DXF data list
        ; ;;
        ; ;; Arguments
        ; ;; xrec: (vla-object) thet XRECORD object

        ; (defun gc:GetXrecordData (xrec / xtyp xval)
          ; (vla-GetXrecordData xrec 'xtyp 'xval)
          ; (gc:VariantsToDxfList xtyp xval)
        ; )

        ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; ;; LISP -> variant

        ; ;; gc:2dPointListToVariant (gile)
        ; ;; Return a variant of 2d coordinates
        ; ;;
        ; ;; Argument: a 2d points list -type (x y)-

        ; (defun gc:2dPointListToVariant (lst)
          ; (vlax-make-variant
            ; (vlax-safearray-fill
              ; (vlax-make-safearray
                ; vlax-VbDouble
                ; (cons 0 (1- (* 2 (length lst))))
              ; )
              ; (apply 'append lst)
            ; )
          ; )
        ; )

        ; ;; gc:3dPointListToVariant (gile)
        ; ;; Return a variant of 3d coordinates
        ; ;;
        ; ;; Argument: a 3d points list -type (x y z)-

        ; (defun gc:3dPointListToVariant (lst)
          ; (vlax-make-variant
            ; (vlax-safearray-fill
              ; (vlax-make-safearray
                ; vlax-VbDouble
                ; (cons 0 (1- (* 3 (length lst))))
              ; )
              ; (apply 'append lst)
            ; )
          ; )
        ; )

        ; ;; gc:ObjectListToVariant
        ; ;; returns a variant (array of objects)
        ; ;;
        ; ;; Argument
        ; ;; lst: a vla-object list

        ; (defun gc:ObjectListToVariant (lst)
          ; (vlax-make-variant
            ; (vlax-safearray-fill
              ; (vlax-make-safearray
                ; vlax-vbObject
                ; (cons 0 (1- (length lst)))
              ; )
              ; lst
            ; )
          ; )
        ; )

        ; ;; gc:DxfListToVariants
        ; ;; Defines 2 variables and bounds a variant to each
        ; ;;
        ; ;; Arguments
        ; ;; lst: a DXF list
        ; ;; typeSymbol: a quoted symbol (other than 'typeSymbol)
        ; ;; valueSymbol: a quoted symbol (other than 'valueSymbol)

        ; (defun gc:DxfListToVariants (lst typeSymbol valueSymbol)
          ; (set typeSymbol
               ; (vlax-make-variant
                 ; (vlax-safearray-fill
                   ; (vlax-make-safearray
                     ; vlax-vbInteger
                     ; (cons 0 (1- (length lst)))
                   ; )
                   ; (mapcar 'car lst)
                 ; )
               ; )
          ; )
          ; (set valueSymbol
               ; (vlax-make-variant
                 ; (vlax-safearray-fill
                   ; (vlax-make-safearray
                     ; vlax-vbVariant
                     ; (cons 0 (1- (length lst)))
                   ; )
                   ; (mapcar '(lambda (x)
                              ; (if (listp (setq x (cdr x)))
                                ; (vlax-3d-point x)
                                ; (vlax-make-variant x)
                              ; )
                            ; )
                           ; lst
                   ; )
                 ; )
               ; )
          ; )
        ; )


        ; ;; gc:SetXdata
        ; ;; Set xdatas to an object
        ; ;;
        ; ;; Arguments
        ; ;; obj: (vla-object) the object to set xdatas
        ; ;; lst: (liste DXF) the xdatas as:
        ; ;; '((1001 . "App_Name") (1002 . "{") (1000 . "string") (1070 . 1) (1002 . "}"))

        ; (defun gc:SetXdata (obj lst / xtyp xval)
          ; (gc:DxfListToVariants lst 'xtyp 'xval)
          ; (vla-SetXdata obj xtyp xval)
        ; )

        ; ;; gc:SetXrecordData
        ; ;; Set datas to an xrecord
        ; ;;
        ; ;; Arguments
        ; ;; xrec: (vla-object) the Xrecord object
        ; ;; lst : (liste DXF) the datas as:
        ; ;; '((1 . "string") (70 . 1) (10 1.0 2.0 0.0))

        ; (defun gc:SetXrecordData (xrec lst / xtyp xval)
          ; (gc:DxfListToVariants lst 'xtyp 'xval)
          ; (vla-SetXrecordData xrec xtyp xval)
        ; )    
            
    ; )
    ; (progn ;  "std-sleep.lsp"
        ; ;;sleeps for the specified number of seconds
            
            ; (defun STD-SLEEP (secs / endt)
            ; (setq endt (+ (getvar "DATE") (/ secs 86400.0)))
            ; (while (< (getvar "DATE") endt) T)) 
    ; )
    ; (progn ;  "polygonRanking.lsp"
        ; ;; Defines functions to get and set a property that I am inventing for objects (specifically polygons, and the regions generated from them) called "rank".
        ; ;; This 'rank' property will be stored as extended data.
        ; ;; The "rank" of polygons is used by my functions that convert polygons into regions.  The rank determines how regions combine.  When you have a set of regions,
        ; ;; the rank of each region specifies a way for the regions to be boolean-combined.  The 'rank' proerty lets us flag polygons (and then the regions generated by each of those polygons)
        ; ;; as being subtractive or additive.  rank is an integer.
        ; ;; Let's call this rank-aware combination of regions rankwiseCombine().
        ; ;; rankwiseCombineRegions() takes as an argument a set of regions, and produces as output a single region (not sure yet if the returned region should itself have a rank).
        ; ;; rankwiseCombineRegions() reads the rank property (if defined) of each region in the input set.  regions which do not have a specified rank property are assumed to be rank 0.
        ; ;; The returned region is constructed as a boolean combination of the input regions as follows:
        ; ;; Take the boolean sum of all rank 0 regions.
        ; ;; From this subtract all rank 1 regions.
        ; ;; To this, add all rank 2 regions.
        ; ;; From this, subtract all rank 3 regions.
        ; ;; etc. (continue the pattern until all regions in the set have been accounted for.)
        ; ;; return the result.
        ; ;; By default, a polygon will be regarded as having rank 0. 

        ; ;; 2017/01/04 - extending the ranking facility so that a rank is now a list of 0 or more integers, rather than a single integer.

        ; ; Registers the application name.

        ; ;; (setq thisApplicationName "polygonRanking")  
        ; ;; (regapp thisApplicationName)
        ; (regapp "polygonRanking")

        ; (defun getRank
            ; (
                ; arg
                ; /
                ; theEname
                ; theEntity
                ; returnValue
                ; thisApplicationName
                ; xData
                ; xDataGroupCodeForInteger
            ; )
            ; (setq thisApplicationName "polygonRanking")
            ; (setq xDataGroupCodeForInteger 1070)
            ; (COND 
                ; (
                    ; (= (type arg) 'ENAME)
                    ; (setq theEname arg)
                ; )
                ; (
                    ; (= (type arg) 'VLA-OBJECT)
                    ; (setq theEname 
                        ; (handent (vla-get-Handle arg))
                    ; )
                ; )
                ; (
                    ; T
                    ; (princ "error: getRank was passed an argument that was neither an ENAME nor a VLA-OBJECT.")
                ; )
                
            ; )
            
            ; (setq theEntity 
                ; (entget theEname (list thisApplicationName))
            ; )
            ; ;; "(assoc -3 theEntity)" evaluates to something like
            ; ;;		(-3 ("polygonRanking" (1070 . 10)))

            
            ; (if 
                ; (assoc -3 theEntity) ;; theEntity has some polyGonRanking xdata
                ; (progn
                    ; ; ; (setq returnValue 
                        ; ; ; (cdr 
                            ; ; ; (assoc xDataGroupCodeForInteger 
                                ; ; ; (cdr 
                                    ; ; ; (nth 0 
                                        ; ; ; (cdr
                                            ; ; ; (assoc -3 theEntity)
                                        ; ; ; )
                                    ; ; ; )
                                ; ; ; )
                            ; ; ; )
                        ; ; ; )
                    ; ; ; )
                    
                    ; (setq returnValue
                        ; (mapcar 'cdr 
                            ; (vl-remove-if-not '(lambda (x) (= xDataGroupCodeForInteger (car x))) 
                            ; ;(vl-remove-if-not '(lambda (x) (= 1070 (car x))) 
                                ; (cdr
                                    ; (cadr
                                        ; (assoc -3 theEntity)
                                    ; )
                                ; )
                            ; )
                        ; )
                    ; )
                ; )
                ; (progn
                    ; ;in this case, selectedEntity does not have any existing xData (at least none that we care about)
                    ; ;(princ "no exisiting xData found.\n")
                    ; ;;(setq returnValue nil)
                    ; (setq returnValue (list 0)) ;; any object that does not have an explicit rank assigned is considered to be rank (0).
                ; )
            ; )
            ; returnValue
        ; )
        ; ;==============


        ; (defun setRank
            ; (
                ; arg
                ; newRank
                ; /
                ; theEname
                ; theEntity
                ; returnValue
                ; newXData
                ; thisApplicationName
                ; xDataGroupCodeForInteger
            ; )
            ; (setq thisApplicationName "polygonRanking")
            ; (setq xDataGroupCodeForInteger 1070)
            
            ; (COND 
                ; (
                    ; (= (type arg) 'ENAME)
                    ; (setq theEname arg)
                ; )
                ; (
                    ; (= (type arg) 'VLA-OBJECT)
                    ; (setq theEname 
                        ; (handent (vla-get-Handle arg))
                    ; )
                ; )
                ; (
                    ; T
                    ; (princ "error: setRank was passed a first argument that was neither an ENAME nor a VLA-OBJECT.")
                ; )
            ; )
            
            ; ;; ensure that newRank is a list of integers (this will let us tolerate a single integer being passed as the newRank argument)
            ; (if (not (listp newRank)) (setq newRank (list newRank)))
            
            ; (setq newXData                       
                ; (list 
                    ; -3 
                    ; (append 
                        ; (list thisApplicationName)
                        ; (mapcar '(lambda (x) (cons xDataGroupCodeForInteger x)) newRank)
                        ; ; (list
                            ; ; (cons xDataGroupCodeForInteger  77)
                            ; ; (cons xDataGroupCodeForInteger  88)
                            ; ; (cons xDataGroupCodeForInteger  99)
                        ; ; )		
                    ; )
                ; )                               
            ; )  
            
            ; (setq theEntity 
                ; (entget theEname (list thisApplicationName))
            ; )
            ; (if (assoc -3 theEntity)
                ; (progn
                    ; ; in this case, theEntity already has some xdata, so we will replace the existing xData with
                    ; ; the new xData
                    
                    ; ;(princ (strcat "existing xData: "))(princ (assoc -3 theEntity))(princ "\n.\n")
                    
                    ; (setq theEntity
                      ; (subst 
                        ; newXData 			;replacement
                        ; (assoc -3 theEntity) 	;needle
                        ; theEntity 				;haystack
                      ; )
                    ; ) 
                ; )
                ; (progn
                    ; ;in this case, theEntity does not have any existing xData, so we will simply append the 
                    ; ; new xData
                    ; ;(princ "there is no existing xData")(princ "\n.\n")
                    ; (setq theEntity
                      ; (cons newXData theEntity)
                    ; ) 
                ; )
            ; )
            ; ; Write the newly modified entity to the database.
            ; (entmod theEntity)
            
            ; (setq returnValue newRank)
            ; returnValue
        ; )
            
        ; ;========================



        ; (defun C:getRankOfNestedEntity
            ; (

            ; /
                ; ename
                ; entity
                ; entityType
                ; x
            ; )
            
            ; (setq x (nentsel "Click on the entity whose rank you want to know.\n"))
            ; (setq ename (car x))
            ; (setq nestingDepth (length (last x)))
            ; (setq entity (entget ename))
            ; (setq entityType (cdr (assoc 0 entity)))
            
            ; (princ "\n")
            ; (princ 
                ; (strcat
                    ; "type: " entityType "."  "\t" "\n"
                    ; "nestingDepth: " (itoa nestingDepth) "."  "\t" "\n"
                    ; "rank: "
                ; )
            ; ) 
            ; (princ (getRank ename)) (princ "\n")
            ; (princ ".")
            ; (princ "\n")
            ; (princ)
        ; )
        ; ;========


        ; ;;These are some useful commands for fixing the rank of closet doors en mass.
        ; (defun C:setRankOfNestedEntity
            ; (

                ; /
                ; ename
                ; entity
                ; entityType
                ; x
                ; originalRank
                ; newRank
                ; defaultNewRank
            ; )
            
            ; (setq x (nentsel "Click on the polyline or circle whose rank you want to set.\n"))
            ; (setq ename (car x))
            ; (setq nestingDepth (length (last x)))
            ; (setq entity (entget ename))
            ; (setq entityType (cdr (assoc 0 entity)))
            ; (setq originalRank (getRank ename))
            
            ; (setq defaultNewRank originalRank)
            ; ; ; (setq newRank (getint (strcat "What rank would you like the new rank to be?  <" (itoa defaultNewRank) ">: ")))
            ; (setq newRank (getstring T (strcat "What rank would you like the new rank to be?  (enter a sequence of zero or more integers separated by spaces) <" (vl-princ-to-string defaultNewRank) ">: ")))
            
            
            ; ; (if (not newRank) (setq newRank defaultNewRank))
            ; (if (= 0 (strlen newRank)) 
                ; (progn
                    ; (setq newRank defaultNewRank)
                ; )
                ; (progn
                    ; (if (not (listp (read newRank))) (setq newRank (strcat "(" newRank ")")))
                    ; (setq newRank 
                        ; (read newRank)
                    ; )
                ; )
            ; )
            
            ; ;; at this point, newRank is a list whose elements are integers.
            
            
            ; (if 
                ; (or
                    ; (= "LWPOLYLINE" entityType)
                    ; (= "CIRCLE" entityType)
                ; )
                ; (progn
                    ; (setRank ename newRank)
                    ; (princ "Success! The rank of the selected ")(princ entityType)(princ " is now " )(princ (getRank ename))(princ ".  ")(princ "(it used to be ") (princ originalRank) (princ ").")(princ "\n")
                ; )

                ; (progn
                    ; (princ 
                        ; (strcat
                            ; "The selected object was a " entityType ", which is neither a LWPOLYLINE nor a CIRCLE, so we are ignoring it." "\n"
                        ; )line
                    ; )
                ; )
            ; )
            ; (princ)
        ; )
        ; ;=====

        ; ;; to get the rank of an element manaully:: (getrank (car (entsel)))
        ; ;; to set the rank of an element manaully:: (setrank (car (entsel)) (list 1 2 3 4))
    ; )
    
    ; (progn ; regionFunctions.lsp
        ; (vl-load-com)
        ; ; (load "variantArrayConversion.lsp")
        ; ; (load "std-sleep.lsp")
        ; ; (load "polygonRanking.lsp")



        ; (defun c:Example_AddRegion ()

            ; ;; This example creates a region from an arc and a line.
            ; (setq acadObj (vlax-get-acad-object))
            ; (setq doc (vla-get-ActiveDocument acadObj))
            ; (setq modelSpace (vla-get-ModelSpace doc))

            ; ;; Define the arc
            ; (setq centerPoint (vlax-3d-point 5 9 0)
                  ; radius 2
                  ; startAngle 0
                  ; endAngle 3.141592)
            ; (setq arc (vla-AddArc modelSpace centerPoint radius startAngle endAngle))
            
            ; ;; Define the line
            ; (setq line (vla-AddLine modelSpace (vla-get-StartPoint arc) (vla-get-EndPoint arc)))
            
            ; (setq pline 
                ; (vla-AddPolyline modelSpace 
                    ; (gc:3dPointListToVariant
                        ; (list
                            ; (list 2 2 0)
                            ; (list 8 2 0)
                            ; (list 8 6 0)
                            ; (list 2 6 0)
                            ; (list 2 2 0)
                        ; )
                    ; )
                ; )
            ; )

            ; ; (setq curves (vlax-make-safearray vlax-vbObject '(0 . 1)))
            ; ; (vlax-safearray-put-element curves 0 arc)
            ; ; (vlax-safearray-put-element curves 1 line)
            
            ; (setq region1BoundaryCurves  (list pline))
            ; (setq region2BoundaryCurves  (list arc line))
          
            ; ;; Create the regions
            ; (setq region1 
                ; (car
                    ; (gc:VariantToLispData
                        ; (vla-AddRegion modelSpace  
                            ; (gc:ObjectListToVariant 
                                ; region1BoundaryCurves
                            ; )
                        ; )
                    ; )
                ; )
            ; )
            
            ; (setq region2
                ; (car
                    ; (gc:VariantToLispData
                        ; (vla-AddRegion modelSpace  
                            ; (gc:ObjectListToVariant 
                                ; region2BoundaryCurves
                            ; )
                        ; )
                    ; )
                ; )
            ; )
            
            ; ;;delete the original boundary curves
            ; (foreach entity (append region1BoundaryCurves region2BoundaryCurves)
                ; (vla-Delete entity)
            ; )
            
            ; ;;modify region1 by subtracting region2 from it.
            ; ;(setq copyOfRegion2 (vla-Copy region2))
            
            ; ;(vla-Boolean region1 acSubtraction region2) ;;it appears that region2 is consumed in this process.
            ; (vla-Boolean region1 acUnion region2)
            
            ; (convertRegionToPolylines region1)
            
            

            
            
            ; ; (setq region2 (vla-AddRegion modelSpace  (gc:ObjectListToVariant (list arc line))))
            
            ; ; (foreach region (gc:VariantToLispData region1)
                ; ; (vlax-dump-object region)
            ; ; )
            
            ; ;(princ (handent (vla-get-Handle region1)))
            
            ; ;(vla-ZoomAll acadObj)
            ; (princ)
        ; )
        ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; ;;when you attempt to invoke the Explode method of a region that happens to be empty,
        ; ;;, autocad throws a "Automation Error. Not applicable" excpetion, when what
        ; ;; we really want is just an empty list.  This function (safelyExplodeRegion) 
        ; ;; returns a
        ; (defun safelyExplodeRegion
            ; (
                ; region
                ; /
                ; result
                ; returnValue
            ; )

            ; (setq result (vl-catch-all-apply 'vla-Explode (list region)))
            ; (if (vl-catch-all-error-p result)
                ; (progn
                    ; ; in this case, there was an exception, presumably due to having an empty region.
                    ; ;(princ "handled an exception when invoking a region's Explode method.\n")
                    ; (setq returnValue (list ))
                ; )
                ; (progn
                    ; (setq returnValue (gc:VariantToLispData result))
                ; )
            ; )
            
            ; returnValue
        ; )
        ; ;============

        ; ;;When you explode a region using the default explode method, 
        ; ;; you get the lines,arcs, etc. that form the boundary of the region... UNLESS
        ; ;; the region is disjoint, in which case you get a set of regions (apparently all non-disjoint).
        ; ;; This function acts identically to the built-in Region.Explode() method with the exception that,
        ; ;;, for a disjoint region, this function returns the boundary lines of all the sub-regions instead
        ; ;; of the subregions themselves.  This function goes the extra mile and explodes subregions, unlike the
        ; ;; built-in Region.Explode() method.
        ; (defun fullyExplodeRegion 
            ; (
                ; region
                ; /
                ; explosionProduct
                ; explosionProducts
                ; newExplosionProducts
                ; returnValue
                ; weNeedToLoop
                ; regionsToDelete
                ; result
            ; )
            
            ; (setq explosionProducts (safelyExplodeRegion region))
            
            
            ; ;; In all likelihood, when you explode a disjoint region, the set of resulting objects never contains a disjoint region. However, out of an abundance of caution,
            ; ;; I am allowing for the possibility that when you explode a disjoint region, you occasionally get further disjoint regions, requiring yet more exploding. 
            
            
            ; ;;the built-in Region.Explode() method does not delete the region.  
            ; ;; So, when we run this fullyExplodeRegion function, we expect to end up with the original region and the newly created lines/arcs/etc.
            ; ;; In the case of a disjoint region, the intermediate explosions will result in left-over sub-regions, unless we explicitly delete them, 
            ; ;; hence the regionsToDelete
            ; (setq regionsToDelete (list)) 
            ; (setq weNeedToLoop T)
            ; (while weNeedToLoop
                ; (setq newExplosionProducts (list ))
                ; (setq weNeedToLoop nil) ;; this will remain nil unless we encounter another region
                ; (foreach explosionProduct explosionProducts
                    ; (if (= "AcDbRegion" (vla-get-ObjectName explosionProduct)) ;;if this explosion product is a region... ,
                        ; (progn ;; ... then explode it and append the results to newExplosionPRoducts
                            ; (setq newExplosionProducts 
                                ; (append
                                    ; newExplosionProducts
                                    ; (safelyExplodeRegion explosionProduct)
                                ; )
                            ; )
                            ; (setq weNeedToLoop T) ;;request another go-round through this loop to handle any regions that might have resulted from exploding this region.
                            ; (setq regionsToDelete
                                ; (append 
                                    ; regionsToDelete
                                    ; (list explosionProduct)
                                ; )
                            ; )
                        ; )
                        ; (progn ;;  ... else just append explosionProduct to newExplosionProducts
                            ; (setq newExplosionProducts 
                                ; (append
                                    ; newExplosionProducts
                                    ; (list
                                        ; explosionProduct
                                    ; )
                                ; )
                            ; )				
                        ; )
                    ; )
                ; )

                ; (setq explosionProducts newExplosionProducts)
            ; )
            
            ; (foreach regionToDelete regionsToDelete
                ; (vla-Delete regionToDelete)
            ; )
            
            ; (setq returnValue explosionProducts)
            ; returnValue
        ; )
        ; ;;=======


        ; ;;takes a region as input
        ; ;;returns a list of polylines.
        ; ;; ideally, I would want this fuinction
        ; ;; to assign a rank to each polyline in such that way that 
        ; ;; (convertRegionToPolylines) and (convertPolylinesToRegions) could be inverses of one another,
        ; ;; but without help from Autocad's low-level API, it is difficult to figure out which boundary polygons are inside others,
        ; ;; and so I have not yet implemented such functionality.

        ; ;; 2017/01/05 TODO: refactor convertRegionToPolylines so that it does not rely on sending autocad commands to the console and so that it will work with a region in a space other than modelSpace (I am not sure if it is currently able to do this).
        ; (defun convertRegionToPolylines
            ; (
                ; region
                ; /
                ; acadObj
                ; doc
                ; modelSpace
                ; ;mySelectionSet
                ; explosionProducts
                ; explosionProduct
                ; originalPickFirst
                ; originalPickAdd
                ; i
                ; originalLayerOfRegion
                ; tempLayer
                ; tempLayerName
                ; layers
                ; newPolylinesSelectionSet
                ; newPolylines
                ; groups
                ; tempGroup
                ; tempGroupName
                ; originalPeditAccept
            ; )
            ; (setq acadObj (vlax-get-acad-object))
            ; (setq doc (vla-get-ActiveDocument acadObj))
            ; (setq modelSpace (vla-get-ModelSpace doc))
            ; (setq layers (vla-get-Layers doc))
            ; (setq groups (vla-get-Groups doc))

            
            ; ;; create a new temporary layer
            ; (setq tempLayerName 
                ; (strcat
                    ; "tempLayer_"
                    ; (rtos (fix (* (getvar "date") 3600 24 1000)))
                ; )
            ; )
            ; ; (setq tempGroupName 
                ; ; (strcat
                    ; ; "tempGroup_"
                    ; ; (rtos (fix (* (getvar "date") 3600 24 1000)))
                ; ; )
            ; ; ) ;;resutls in 'invalid group name' error.
            ; (setq tempGroupName "TEMPGROUP")
            ; (setq tempLayer (vla-Add layers tempLayerName))
            ; (setq tempGroup (vla-Add groups tempGroupName))
            
            
            ; ;;store the original layer of region for later
            ; (setq originalLayerOfRegion (vla-get-Layer region))
            
            ; ;; move region to tempLayer, so that when we explode the region into lines (arcs, etc.) and then join those lines 
            ; ;; into polylines, the set of polylines that we want to return is precisely the set of polylines on tempLayer.
            ; (vla-put-Layer region tempLayerName)
            
            ; (setq explosionProducts (fullyExplodeRegion region))

            ; ;;we may want to delete region at this point, or at least move it back to its original layer.
            ; (vla-put-Layer region originalLayerOfRegion)
            
            ; (setq newPolylines (list))
            ; (if (> (length explosionProducts) 0)   
                ; (progn
                    ; (setq mySelectionSet (ssadd))
                    ; (foreach explosionProduct explosionProducts
                        ; (setq mySelectionSet 
                            ; (ssadd  
                                ; (handent (vla-get-Handle explosionProduct))
                                ; mySelectionSet
                            ; )
                        ; )
                    ; )
                    ; (vla-AppendItems tempGroup (gc:ObjectListToVariant explosionProducts))   
                    
                    ; ;;at this point, mySelectionSet is a selection set containing all the lines, arcs, etc, that were created when
                    ; ;; we exploded the region.  Now we want to join them together.
                    
                    ; ;;at this point, tempGroup is a group containing all the lines, arcs, etc, that were created when
                    ; ;; we exploded the region.  Now we want to join them together.
                    
                    ; (setq originalPeditAccept (getvar "PEDITACCEPT"))	
                    ; (setq originalPickAdd (getvar "PICKADD"))
                    ; (setq originalPickFirst (getvar "PICKFIRST"))
                    ; (setvar "PEDITACCEPT" 0)
                    ; (setvar "PICKADD" 0)
                    ; (setvar "PICKFIRST" 1)


                    
                    
                    ; (command-s
                        ; "pedit"
                        ; "Multiple"
                        ; "Group"
                        ; tempGroupName
                        ; ""
                        ; ""
                        ; "Join"
                        ; ""
                        ; ""
                    ; )	
                    ; (setvar "PICKADD" originalPickAdd)
                    ; (setvar "PICKFIRST" originalPickFirst)
                    ; (setvar "PEDITACCEPT" originalPeditAccept)
                    ; ;; It was very surprising to me that the JOIN command did not work here.
                    ; ;; I could not get JOIN to take a selection of multiple objects and join them together.
                    ; ;; it is as if the JOIN command that runs when invoike from a script with the (command ) function
                    ; ;; is a different command from that which runs when you manaully type in "JOIN" on the autocad command line.
                    ; ;; Specifically, the JOIN command as invoked by the (Command ) function seems to lack 
                    ; ;; the capability to deal with a selection of multiple objects -- it seems to want to start with a single 
                    ; ;; selected object.  I could almost make this work, except that in this single-object mode, join
                    ; ;;will not handle mixtures of arcs and lines -- only one or the other.
                    ; ;;Also, it seems that Document.SendCommand() behaves exactly like what you manually type at the command 
                    ; ;; string -- I was able to make JOIN work as expected when using Document.SendCommand(), but this 
                    ; ;; was problematic because Dodument.SendCommand() seems to wait until after the whole lisp script is finished before
                    ; ;; sending the command -- this does not work for me because I need to programmatically manipulate the objects that 
                    ; ;; the JOIN command creates, so I need to be able to execute more script AFTER the Join command has completed.
                    
                    ; ;;Fortunately, the PEDIT command does appear to be identical whether invoked manually on the Command line or invoked
                    ; ;; via the (command ) function.  And Pedit can be made to do the joining that I want to do, although it is a bit more clunky.
                    
                    ; ;; newPolylinesSelectionSet will contain everything on tempLayer
                    
                    ; (setq newPolylinesSelectionSet
                        ; (ssget "_X" (list (cons 8 tempLayerName)))
                    ; )
                    
                    
                    
                    ; (setq i 0)
                    ; (while (< i (sslength newPolylinesSelectionSet))
                        ; (setq newPolylines
                            ; (append
                                ; newPolylines
                                ; (list (vlax-ename->vla-object (ssname newPolylinesSelectionSet i)))
                            ; )
                        ; )
                        ; (setq i (+ 1 i))
                    ; )
                    

                    
                    ; (foreach item newPolylines
                        ; (vla-put-Layer item originalLayerOfRegion )
                    ; )
                    
                    ; ;;(princ "(length newPolylines): ")(princ (length newPolylines)) (princ "\n")
                ; )
            ; )
            ; ;;delete the temp layer
            ; ;(vla-Delete (vla-Item layers tempLayerName))
            ; (vla-Delete tempLayer)
            
            ; ;;delete the temp group
            ; ;(vla-Delete (vla-Item groups tempGroupName))
            ; (vla-Delete tempGroup)
            
            
            ; ;(vla-Update acadObj)
            ; ;(vla-Update (vla-get-Application acadObj))
            
            ; ; ; (setq newlyAddedEntities (list))
            
            ; ; ; (setq currentEntity (handent handleOfCurrentEntity))
            ; ; ; ;;collect all the newly added entities
            ; ; ; (while (setq newlyAddedEntity (entnext currentEntity))
                ; ; ; (setq currentEntity newlyAddedEntity)
                ; ; ; (setq  newlyAddedEntities
                    ; ; ; (append
                        ; ; ; newlyAddedEntities
                        ; ; ; (list newlyAddedEntity)
                    ; ; ; )
                ; ; ; )
            ; ; ; )
            ; ;; This does not seem to be working.  IDEA:: create a temporary layer, and move the objects to be joined
            ; ;; to that layer before joining,  then after joining, the new polylines will be the only thing on that
            ; ;; new layer.
            
            ; ; ; (princ "(length newlyAddedEntities): ") (princ (length newlyAddedEntities)) (princ "\n")
            
            ; ;;at this point, newlyAddedEntities should be a list of the enames of all the objects added as a result of the above join
            ; ;; command
            
            ; ; ; (foreach newlyAddedEntity newlyAddedEntities
                ; ; ; (vlax-dump-object (vlax-ename->vla-object newlyAddedEntity))
            ; ; ; )
            ; ; ; newlyAddedEntities
            
            
            ; ;;(command "JOIN")
            
            
            ; ;;
            ; ;; I found that running (setvar "pickfirst"... ) and (setvar "pickadd" ...) here caused the above JOIN
            ; ;; command to run as if nothing was selected.  I suspect that vla-SendCommand is somehow asynchronous with other
            ; ;; commands.  (command "JOIN") was even worse -- I could never get it to run and see the currently selected
            ; ;; objects -- it always behaved as if no objects were selected.
            

            
            ; newPolylines
        ; )
        ; ;=================

        ; ;;takes a polyline (expected to be closed and not self-intersecting,)
        ; ;; and converts it into a region.  Also, assigns the rank of the polyline to
        ; ;; the region.  (copying the rank is the only reason to use this function instead of  
        ; ;; using the built-in ModelSpace.AddRegion() function directly.
        ; ;; We might alternately call this function 'rankwiseConvertPolylineToRegion'.
        ; (defun convertPolylineToRegion
            ; (
                ; polyline
                ; /
                ; acadObj
                ; doc
                ; modelSpace
                ; layers
                ; groups
                ; region		
                ; returnValue
                ; regions
                ; owningSpace
            ; )
            ; (setq acadObj (vlax-get-acad-object))
            ; (setq doc (vla-get-ActiveDocument acadObj))
            ; (setq modelSpace (vla-get-ModelSpace doc))
            ; (setq layers (vla-get-Layers doc))
            ; (setq groups (vla-get-Groups doc))
            
            
            ; (setq owningSpace (vla-ObjectIDToObject  doc (vla-get-OwnerID polyline)))
            
            ; (setq regions
                ; (gc:VariantToLispData
                    ; (vla-AddRegion owningSpace  
                        ; (gc:ObjectListToVariant 
                            ; (list polyline)
                        ; )
                    ; )
                ; )
            ; )
            
            ; (if (/= 1 (length regions)) ;;if regions does not contain exactly one element...
                ; (princ 
                    ; (strcat
                        ; "Warning from (convertPolylineToRegion): "
                        ; "The input polyline generated " (itoa (length regions)) " regions.  "
                        ; "We were expecting exactly 1."
                    ; )
                ; )
            ; )
            ; ;;perhaps the best way to handle the case of regions containing more than one element would be to boolean combine all elements of regions together into a single region.
            
            ; (setq region (car regions))
            
            ; (setRank region (getRank polyline))
            
            ; (setq returnValue region)
            ; returnValue
        ; )
        ; ;===========

        ; ;; rankwiseCombineRegions() takes as an argument a set of regions, and produces as output a single region (not sure yet if the returned region should itself have a rank).
        ; ;; Actually, the input list can contain a mixture of regions and polylines.. each polyline will be converted into a region using convertPolylineToRegion.
        ; ;; rankwiseCombineRegions() reads the rank property (if defined -- see polygonRanking.lsp, rank is a custom property that I invented and am implementing using extended Data attached to each region.) 
        ; ;; of each region in the input set.  regions which do not have a specified rank property are assumed to be rank 0.
        ; ;; The returned region is constructed as a boolean combination of the input regions as follows:
        ; ;; Take the boolean sum of all rank 0 regions.
        ; ;; From this subtract all rank 1 regions.
        ; ;; To this, add all rank 2 regions.
        ; ;; From this, subtract all rank 3 regions.
        ; ;; etc. (continue the pattern until all regions in the set have been accounted for.)

        ; ;;the argument is a list, each element of which is either a polyline or a region or a list of the form (rank object).  If an element is of the latter form, the rank of the object that is stored in the object's xdata will be ignored and the object's rank will be taken to be rank.  This provides a way to override the rank attached to an object.
        ; (defun rankwiseCombineRegions
            ; (
                ; regionsArg
                ; /
                ; acadObj
                ; doc
                ; modelSpace
                ; layers
                ; groups	
                ; returnRegionHasBeenInitialized	
                ; returnRegion
                ; returnValue
                ; ranks
                ; region
                ; thisRank
                ; theseRegions
                ; rank
                ; newRegions
                ; regions
                ; item
                ; rankedRegions
                ; newRankedRegions
            ; )
            
            ; (setq acadObj (vlax-get-acad-object))
            ; (setq doc (vla-get-ActiveDocument acadObj))
            ; (setq modelSpace (vla-get-ModelSpace doc))
            ; (setq layers (vla-get-Layers doc))
            ; (setq groups (vla-get-Groups doc))
            

            ; ;;we really ought to handle the case where regionsArg is empty.
            ; ;;(princ regionsArg)
            ; ;;(princ (mapcar '(lambda (x) (listp x)) regionsArg))
            
            ; (setq rankedRegions
                ; (mapcar
                    ; '(lambda (x / rank object) 
                        ; (if (listp x)
                            ; (progn
                                ; (setq object (cadr x))
                                ; (setq rank (car x))
                            ; )
                            ; (progn
                                ; (setq object x)
                                ; (setq rank (getRank x))
                            ; )
                        ; )
                        ; (list rank object)
                    ; )
                    ; regionsArg
                ; )
            ; )
            ; ;; at this point, rankedRegions is a list, each element of which is a list of the form {rank, object}, where object is a polyline (or circle) or region, and rank is the rank to be used for that object.

            ; ;; convert any polylines or circles into true regions
            ; (setq rankedRegions
                ; (mapcar
                    ; '(lambda 
                        ; (x / rank object) 
                        ; (setq rank (car x))
                        ; (setq object (cadr x))
                        ; (if 
                            ; (or
                                ; (= "AcDbPolyline" (vla-get-ObjectName object))
                                ; (= "AcDbCircle" (vla-get-ObjectName object))
                            ; )
                            ; (progn
                                ; (setq object (convertPolylineToRegion object))
                            ; )
                        ; )
                        ; (list rank object)
                    ; )
                    ; rankedRegions
                ; )
            ; )
            
            ; ;;remove any elements of rankedRegions which have a nil object (such objects could have been created above by convertPolylineToRegion)
            ; (setq rankedRegions
                ; (vl-remove-if
                    ; '(lambda (x) (not (cadr x)))
                    ; rankedRegions
                ; )
            ; )
            
            ; ;; at this point, rankedRegions is a list, each element of which is a list of the form {rank, region}, where region is a region, and rank is the rank to be used for that region.
            
            ; (if nil (progn ;;deprecated code from prior to the advent of ranks that are lists
                ; (setq regions regionsArg)
                ; ;; convert any polylines that are in regions into true regions.
                ; (setq newRegions (list))
                ; (foreach item regions 
                    ; (if 
                        ; (or
                            ; (= "AcDbPolyline" (vla-get-ObjectName item))
                            ; (= "AcDbCircle" (vla-get-ObjectName item))
                        ; )
                        ; (progn
                            ; (setq region (convertPolylineToRegion item))
                        ; )
                        ; (progn
                            ; (setq region item)
                        ; )
                    ; )
                    ; (if region ;; this 'if' here is to exclude a nil region that might have been generated by convertPolylineToRegion.
                        ; (setq newRegions
                            ; (append
                                ; newRegions
                                ; (list region)
                            ; )
                        ; )
                    ; )
                ; )
                
                ; (setq regions newRegions)
                
                    
                        ; ; 
                
                ; (setq talliedRegions (list)) ;; talliedRegions will be a list, each element is of the form (<rank> . <listOfRegions>), where <listOFRegions> contains all the regions of the specified rank.
                ; ;;construct talliedRegions, which we want to be sorted by ascending rank, so that when we run foreach, 
                ; ;; below, we will process the regions starting with rank 0.
                
                ; ;; populate ranks, a list of all ranks that appear in regions.
                ; (setq ranks (list ))
                ; (foreach region regions
                    ; (setq thisRank (getRank region))
                    ; (if (not (member thisRank ranks))
                        ; (setq ranks (cons thisRank ranks))
                    ; )
                ; )
                ; (setq ranks (vl-sort ranks '<)) ;; sort ranks ascending.
                
                ; (foreach rank ranks
                    ; (setq talliedRegions
                        ; (append
                            ; talliedRegions
                            ; (list (cons rank (list )))
                        ; )
                    ; )
                ; )
                ; ;;;(princ "\n")(princ talliedRegions)(princ "\n")(quit)
                

                ; ;;at this point talliedRegions has all the ranks in order, but all the lists of regions belonging to each rank are empty and need to be filled in...
                

                
                ; (foreach region regions
                    ; (setq thisRank (getRank region))
                    
                    ; ;;add thisRegion to the appropriate sub list of talliedRegions.
                    ; (setq talliedRegions
                        ; (subst 
                            ; ;; 1. replacement: 
                            ; (cons thisRank
                                ; (append
                                    ; (cdr (assoc thisRank talliedRegions))
                                    ; (list region)
                                ; )
                            ; )
                            
                            ; ;; 2.  needle: 
                            ; (assoc thisRank talliedRegions)
                            
                            ; ;; 3.  haystack:
                            ; talliedRegions 
                        ; )
                    ; )
                ; )
                
                ; ;; At this point, talliedRegions is fully formed and ready to use.
                ; ;;;(princ "\n")(princ talliedRegions)(princ "\n")(quit)
                
                    
                
            ; ))
            ; ;===========
            
            ; ;primeRanks will be a list of all the first elements of the ranks
            ; (setq primeRanks
                ; (LM:Unique
                    ; (mapcar 
                        ; '(lambda (x / rank) 
                            ; (setq rank (car x))
                            ; (car rank)
                        ; )
                        ; rankedRegions
                    ; )
                ; )
            ; )
            ; (setq primeRanks (vl-sort primeRanks '<)) ;; sort primeRanks ascending.
            ; ;;(princ "primeRanks: ")(princ primeRanks)(princ "\n")
            
            ; ;==========
            
            ; ;;sweep through primeRanks, and for each primeRank, deal with all the elements of rankedRegions that belong to that primeRank.
            ; (setq talliedRegions
                ; (mapcar
                    ; '(lambda (primeRank / rankedRegionsHavingTheSpecifiedPrimeRank subRankedRegions theseRegions)
                        ; ;; collect all the regions in rankedRegions that have primeRank as the first element of their rank.
                        ; (setq rankedRegionsHavingTheSpecifiedPrimeRank
                            ; (vl-remove-if-not
                                ; '(lambda (x / rank) (setq rank (car x)) (= primeRank (car rank)))
                                ; rankedRegions
                            ; )
                        ; )
                        
                        ; ;; theseRegions will be a list of regions.  theseRegions will consist of all the elements of subRankedRegions with a rank of length 1, and 
                        ; ;; one additional region which is the rankwiseCombine of the subRankedRegions whose rank has length > 1 (with the primeRank stripped).
                        
                        ; ;; scan through rankedRegionsHavingTheSpecifiedPrimeRank and accumulate two lists:
                        ; ;;		theseRegions: Any regions whose rank contained only one element (namely primeRank) will go into theseRegions
                        ; ;;		subRankedRegions: Any regions whose rank contains more than one element will have the first element stripped from their rank (to form subrank), and then the {subrank, region} pair 
                        ; ;;			will be appended to subRankedRegions.
                        ; ;;			subRankedRegions will be suitable for passing into rankwiseCombineRegions (yes, this very function (it's re-entrant))
                        ; (setq theseRegions (list ))
                        ; (setq subRankedRegions (list ))
                        ; (mapcar 
                            ; '(lambda (x / rank subrank region) 
                                ; (setq rank (car x)) ;;we already know that the first element of rank is primeRank, because of the filtering that we did above
                                ; (setq subrank (cdr rank)) ;;subrank is the rest of rank (i.e. we strip the first element). Subrank may very well be nil (this happens if the length of rank is 1.)
                                ; (setq region (cadr x))
                                ; (if (> (length subrank) 0) ; if the length of the subrank is greater than zero (i.e. if the rank had more than one element)
                                    ; (progn
                                        ; ;; append the {subrank, object} pair to subRankedRegions
                                        ; (setq subRankedRegions
                                            ; (append subRankedRegions
                                                ; (list 
                                                    ; (list subrank region)
                                                ; )
                                            ; )
                                        ; )
                                    ; )
                                    ; (progn
                                        ; ;; in this case, the length of subrank is zero (i.e. the rank contained only one element (namely, primeRank))
                                        ; ;; append the region to theseRegions
                                        ; (setq theseRegions
                                            ; (append theseRegions
                                                ; (list 
                                                    ; region
                                                ; )
                                            ; )
                                        ; )
                                    ; )
                                ; )
                            ; )
                            ; rankedRegionsHavingTheSpecifiedPrimeRank
                        ; )
                        ; ;==== 
                        
                        ; ;; apply rankwiseCombine to the subRnkedRegions (if there are any), and append the result to theseRegions
                        ; ;; Oops.  That is not generally correct, because if the subRanked regions are subtractive, a rankwiseCombine produces a null region (or, in practice, exceptions, because null regions are not handled very well)
                        ; ;;  The correct approach is to rankwise combine all the subranked regions together with theseRegions (using rank zero for each of theseRegions).
                        ; ;; In other words, if we have some regions with rank (3), some regions with rank (3 4), and some regions with rank (3 4 5),
                        ; ;; we want to treat the rank (3) as rank (3 0 0) and the rank (3 4) as rank (3 4 0).
                        ; (if (> (length subRankedRegions) 0) 
                            ; ;;generate consolidatedRegion, which will take the place of all theseRegions and all the subrankedregions in the final talliedList.
                            ; (progn
                                ; ;;(princ "rankwiseCombineRegions is recursing with primeRank ")(princ primeRank)(princ ".\n")
                                ; (setq consolidatedRegion
                                    ; (rankwiseCombineRegions  ;; THIS IS THE RE-ENTRY
                                        ; (append
                                                ; (mapcar '(lambda (x) (list (list 0) x)) theseRegions)
                                                ; subRankedRegions
                                        ; )
                                    ; )
                                    ; ;;as rankwiseCombineRegions calls itself recursively, we will eventually reach a level where (length subRankedRegions) will be zero.
                                    ; ;; This is guaranteed to eventually happen because we are stripping off the first element of the ranks on each iteration (i.e. reducing their length by 1).
                                    ; ;; Once we get to the iteration where (length subRankedRegions) is zero, the above 'IF' condition will evaluate to false, and so we will stop recursing.
                                ; )
                                ; (setq theseRegions (list consolidatedRegion))
                            ; )
                            ; (progn
                                ; ;;(princ "rankwiseCombineRegions is skipping recursion for primeRank ")(princ primeRank)(princ ".\n")
                            ; )
                        ; )
                        ; ;=======
                        
                        ; ; (list primeRank theseRegions)
                        ; ;;to my mind, the above makes more sense as an element of talliedRegions, but I will use the below to stay compatible with the rest of the code.
                        ; (cons primeRank theseRegions)
                    ; )
                    ; primeRanks
                ; )	
            ; )
            ; ;=========
            ; ;; Beyond this point, the code did not need to be modified at all during the expansion from the single-integer rank concept to the list-of-integers rank concept.
            ; ;; When we added the implementation of the list-of-integers rank concept, the construction of talliedRegions became a bit more complicated (requiring recursion, for instance), but 
            ; ;; the modified code produces the same sort of talliedRegions list as the original code, and so the rest of the program digests the talliedRegions list
            ; ;; just like it always has.
            ; ;;(princ "talliedRegions: ")(princ talliedRegions)(princ "\n")
            
            
            ; (setq returnRegionHasBeenInitialized nil )
            ; (foreach item talliedRegions
                ; (setq thisRank (car item))
                ; (setq theseRegions (cdr item))
                
                ; (if (= 0 (rem thisRank 2)) ;;if thisRank is even...
                    ; (progn ;; then we need to boolean add theseRegions
                        ; ;;initialise returnRegion if necessary
                        ; (if (not returnRegionHasBeenInitialized)
                            ; (progn
                                ; (setq returnRegion (car theseRegions))
                                ; (setq returnRegionHasBeenInitialized T)
                                ; ;; (setq theseRegions (cdr theseRegions))
                                ; ;;actually, the above discarding of the first member of theseRegions is not necessary --
                                ; ;; it doesn't hurt to boolean add the same region twice.
                                ; ;;ACTUALLY, Autocad complains when you try to union a region with itself, so we better trim the first element after all.
                                ; (setq theseRegions (cdr theseRegions))
                            ; )
                        ; )
                        ; (foreach  region theseRegions
                            ; (vla-Boolean returnRegion acUnion region)
                        ; )
                    ; )
                    ; (progn ;; else thisRank must be odd, in which case, we need to boolean subtract theseRegions
                        ; (if (not returnRegionHasBeenInitialized)
                            ; (progn
                                ; ;;in this case, we are trying to subtract regions from an empty region, which is perfectly valid, but does at least deserve a warning.
                                ; (princ 	
                                    ; (strcat 
                                        ; "Warning: (rankwiseCombineRegions) was asked to subtract " 
                                        ; (itoa (length theseRegions)) " rank " (itoa thisRank) 
                                        ; " region(s) from a non-existent/empty region.  "
                                        ; "This subtraction request has been ignored." "\n"
                                    ; )
                                ; )
                            ; )
                            ; (progn
                                ; ;;in this case, returnRegion has been initialized, so our task is to boolean subtract each of theseRegions from it.
                                ; (foreach  region theseRegions
                                    ; (vla-Boolean returnRegion acSubtraction region)
                                ; )	
                            ; )	
                        ; )
                    ; )	
                ; )
            ; )
            
            ; ;;at this point, returnRegion is the result that we were hoping for (note: returnRegion will be nil
            ; ;; if there were no additive (i.e. even) rank regions in regions.  Autocad does not seem to have the concept
            ; ;; of an empty region, which is what I really would prefere to return in that case, so nill will have to suffice.)
            
            ; ;;I haven't yet considered what is to become of the input regions.  
            ; ;; Ideally, I would want (rankwiseCombineRegions) to leave the input regions untouched.
            ; ;; The acad documentation seems to imply that Region.Boolean() does not
            ; ;; modify its argument, but that does not always appear to be the case -- specifically for subtrction operations, the region that is subtracted 
            ; ;; seems to be consumed (i.e. becomes deleted) in the process.
            ; ;; Perhaps I should
            ; ;; create a working copy of input regions and build the returnRegion from this working copy, then delete the workingcCopy,
            ; ;; in order to guard against the possibility that Region.Boolean() modifies its argument.
            ; (setq returnValue returnRegion)
            ; returnValue	
        ; )
        ; ;=============

        ; ;; Unique  -  Lee Mac
        ; ;; Returns a list with duplicate elements removed.
        ; (defun LM:Unique ( l )
            ; (if l (cons (car l) (LM:Unique (vl-remove (car l) (cdr l)))))
        ; )
        ; ;==============

        ; ;; takes every polyline on layer 0 and rankwiseCombine's all of them into a singkle region, which 
        ; ;; we move to layer "Layer1"
        ; (defun playground1
            ; (
            
                ; /
                ; acadObj
                ; doc
                ; modelSpace
                ; layers
                ; groups
                ; regionsToBeCombined
                ; region
                ; hatch
                ; tempGroupName
                ; tempGroup
                ; requiredLayers
                ; nameOfLayerOnWhichToPlaceResult
                ; entitiesToDelete
                ; entityToDelete
            ; )
            
            ; (setq acadObj (vlax-get-acad-object))
            ; (setq doc (vla-get-ActiveDocument acadObj))
            ; (setq modelSpace (vla-get-ModelSpace doc))
            ; (setq layers (vla-get-Layers doc))
            ; (setq groups (vla-get-Groups doc))
            ; (setq nameOfLayerOnWhichToPlaceResult "rankwiseCombination")
            
            
            ; (progn ;;create the required layers
                ; (setq requiredLayers 
                    ; (list
                        ; (list
                            ; (cons 	"name" 		"0")
                            ; (cons 	"color" 	"7")
                        ; )
                        ; (list	
                            ; (cons 	"name" 		"rankwiseCombination")
                            ; (cons 	"color" 	"Red")
                        ; )
                    ; )
                ; )
                ; ;======
                
                ; (foreach layer requiredLayers
                    ; (command-s
                        ; "-LAYER"
                        ; "make"
                        ; (cdr (assoc "name" layer))
                        ; "color"
                        ; (cdr (assoc "color" layer))
                        ; (cdr (assoc "name" layer))
                        ; ;;"" ;; equivalent to sending Enter keystroke.
                        ; "" ;; equivalent to sending Enter keystroke.
                    ; )
                ; )
            ; )
            ; ;=============
            
            ; ;;delete everything on LayerOnWhichToPlaceResult, to clean the slate.
            ; (setq entitiesToDelete (list ))
            ; (vlax-for entity modelSpace
                ; (if 
                    ; (= (vla-get-Layer entity) nameOfLayerOnWhichToPlaceResult)	
                    ; (progn
                        ; ;; append entity to entitiesToDelete
                        ; (setq entitiesToDelete
                            ; (append
                                ; entitiesToDelete
                                ; (list entity)
                            ; )
                        ; )
                    ; )
                ; )
            ; )
            ; (foreach entityToDelete entitiesToDelete
                ; (vla-Delete entityToDelete)
            ; )
            
            ; (setq regionsToBeCombined (list))
            
            ; (vlax-for entity modelSpace
                ; (if ;;if entity is a polyline or circle on layer '0' ...
                    ; (and
                        ; (or
                            ; (= "AcDbPolyline" (vla-get-ObjectName entity))
                            ; (= "AcDbCircle" (vla-get-ObjectName entity))
                        ; )
                        ; (= "0" (vla-get-Layer entity))	
                    ; )
                    ; ;; append entity to regionsToBeCombined
                    ; (setq regionsToBeCombined
                        ; (append
                            ; regionsToBeCombined
                            ; (list entity)
                        ; )
                    ; )
                ; )
            ; )
            ; ;=======
            
            ; (setq region (rankwiseCombineRegions regionsToBeCombined))
            
            ; (princ "region: ")(princ region) (princ "\n")
            ; ;;(vlax-dump-object region)
            ; (vla-put-Layer region nameOfLayerOnWhichToPlaceResult)
            

            
            ; ;; fill the region in with a solid color
            ; ; ; (setq hatch 
                ; ; ; (vla-AddHatch modelSpace
                    ; ; ; 1 ;;patternType
                    ; ; ; "SOLID" ;;patternName
                ; ; ; )
            ; ; ; ) ;;modelSpace.AddHatch() won't do what I need it to do because it does
            ; ;; not convert a Region into a hatch,  Instead, it requires you to build the hatch by adding inner and out loops.
            ; ;; However, the command-line 'HATCH' command will convert a region into a hatch.
            ; ;;we create a temp group to contain the region simply to facilitate 
            ; ;; invoking the 'HATCH' command.
            ; (setq tempGroupName "TEMPGROUP")
            ; (setq tempGroup (vla-Add groups tempGroupName))
            ; (vla-AppendItems tempGroup (gc:ObjectListToVariant (list region)))
            
            ; (command-s
                ; "_-HATCH"
                ; ;; we're now sitting at root of -HATCH menu
                
                ; "Select"
                ; "Group"
                ; tempGroupName
                ; ""
                ; ;; we're now back at root of -HATCH menu
                
                ; "Color"
                ; "ByLayer" ;"Red"
                ; ;; we're now back at root of -HATCH menu
                
                ; "Properties"
                ; "Solid"
                ; ;; we're now back at root of -HATCH menu
                
                    ; "Advanced"
                    ; ;; we're now  at the 'Advanced' submenu of -HATCH menu
                    ; "Associativity"
                    ; "Yes"
                    ; ;; we're now  back at the 'Advanced' submenu of -HATCH menu
                    ; "Style"
                    ; "Normal" ;;the default style is 'Outer' --this is not the best option for converting a region to a hatch because islands within islands are npot hatched when Style is 'Outer'.  However, when 'Style' is 'Normal', the region is converted into a hatch exactly as expected.  --This drove me bat-shit until I figured it out.
                    ; ""
                ; ;; we're now back at root of -HATCH menu
                
                
                ; "" ;; a final ENTER to execute the command.
            ; )
            
            ; (setq hatch 
                ; (vlax-ename->vla-object (entlast))
            ; )
            
            
            
            ; ;(vlax-dump-object hatch)
            
            ; (vla-put-Layer hatch nameOfLayerOnWhichToPlaceResult)
            
            ; (vla-Delete tempGroup)
            ; (princ)
        ; )
        ; ;=============

        ; ;;========= ACTIONS: =================
        ; ;;(convertRegionToPolylines (vlax-ename->vla-object (car (entsel))))
        ; ;(playground1)
        ; ; (rankwiseCombineRegions nil)
        ; ; (c:Example_AddRegion)    
    
    ; )
    
    



)


(vlax-for entity (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if
        (and 
            (= (vla-get-ObjectName entity) "AcDbHatch")
        )
        (progn
            (convertHatchToSolids entity
                '(lambda (result)
                    (setq solids result)
                    (command "._UNDO" "_End")
                    (princ "\n\n\nCOMPLETED\n")
                    (princ)
                )
            )
            ; (setq mesh (convertRegionToMesh region))
            ; (vla-Delete entity)
        )
    )
)

(princ "ahoy")(princ "\n")

(princ)



; (quit 0)

; (vlax-for entity (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
    ; (if
        ; (and 
            ; (= (vla-get-ObjectName entity) "AcDbRegion")
        ; )
        ; (progn
            ; (setq myRegion entity)
        ; )
    ; )
; )


; (princ "(vla-get-Handle myRegion): " ) (princ (vla-get-Handle myRegion)) (princ "\n")

; (setq mesh (convertRegionToMesh myRegion))
; (princ "ahoy")(princ "\n")
; (command "._UNDO" "_End")
; (princ)
; (quit 0)


; (command "SELECT" (handent (vla-get-Handle myRegion)) "")
; (command-s "SELECT" (handent (vla-get-Handle myRegion)) "" "MESHSMOOTH" "p" "")(princ)
; (command "SELECT" (handent (vla-get-Handle myRegion)) "" )

; (command "MESHSMOOTH" (handent (vla-get-Handle myRegion)) (strcat (chr 13) (chr 10)))(princ)

; (command "MESHSMOOTH" (handent (vla-get-Handle myRegion)) "")
; (command "._delay" 1000)
; (princ)

; (command "SELECT" (handent (vla-get-Handle myRegion)) "")



; (command "MESHSMOOTH")
; (command (handent (vla-get-Handle myRegion)))
; (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "\n")
; (setq theCommand 
    ; (strcat
        ; "(vla-SendCommand "
            ; "(vla-get-ActiveDocument (vlax-get-acad-object)) "
            ; "\""
                ; "MESHSMOOTH"
                ; "\\n" 
                ; "(handent "
                    ; "\\\"" (vla-get-Handle myRegion) "\\\""
                ; ")" 
                ; "\\n"
                ; "\\n"
            ; "\""
        ; ")"
    ; )
; )
; (princ "theCommand: ")(princ theCommand)(princ "\n")(princ)
; (command theCommand)


; (command-s "MESHSMOOTH" "0,0,0" "")(princ)

; (princ "ahoy")(princ "\n")
; (command "._UNDO" "_End")
; (princ)
; (quit 0)



; ; For each such region, convert it into a solid
; (setq polylines (list ))
; (vlax-for entity (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
    ; (if
        ; (and 
            ; (= (vla-get-ObjectName entity) "AcDbRegion")
        ; )
        ; (progn
            ; (setq polylines 
                ; (append 
                    ; polylines 
                    ; (convertRegionToPolylines entity)
                ; )
            ; )
            ; (vla-Delete entity)
        ; )
    ; )
; )

; (princ "we have ")(princ (length polylines))(princ " polylines.\n") 

; (setq solids (list ))
; (foreach polyline polylines
    ; ; we assume that the polyline is closed and is composed entirely of straight segments, and has exactly 3 vertices or has exactly 4 vertices.
    ; (princ "polyline ")(princ (vla-get-Handle polyline))(princ " has ")
    ; (princ (length (gc:2dVariantToPointList (vla-get-Coordinates polyline))))
    ; (princ " vertices.\n")
    ; ; convert to a solid
    
    
    ; (setq vertices 
        ; (mapcar 
            ; '(lambda (twoDimensionalPoint) 
                ; (list 
                    ; (nth 0 twoDimensionalPoint)
                    ; (nth 1 twoDimensionalPoint)
                    ; 0
                ; )
            ; )
            ; (gc:2dVariantToPointList (vla-get-Coordinates polyline))
        ; )
    ; )
    ; ; This needs to be improved to deal with the fact that the two dimensional points are given in terms of 
    ; ; a coordinate system that is not necessarily the same as the world coordinate system.
    ; ; see Document.Utility.TranslateCoordinates 
    
    ; ; (princ "polyline ")(princ (vla-get-Handle polyline))(princ " has ")
    ; ; (princ (length vertices))
    ; ; (princ " vertices.\n")
    
    
    
    ; (setq solid 
        ; (vla-AddSolid
            ; (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
            ; (vlax-3d-point (nth 0 vertices))
            ; (vlax-3d-point (nth 1 vertices))
            ; (if (> (length vertices) 3)
                ; (vlax-3d-point (nth 3 vertices))
                ; (vlax-3d-point (nth 2 vertices))
            ; )
            ; (vlax-3d-point (nth 2 vertices))
        ; )
    ; )
    ; (if solid
        ; (progn
            ; (vla-Delete polyline)
            ; (setq solids
                ; (append
                    ; solids
                    ; (list solid)
                ; )
            ; )
        ; )
    ; ) 
; )

; (princ "\ngenerated ")(princ (length solids))(princ " solids.\n")




; ; (vlax-dump-object (vlax-ename->vla-object (car (entsel))))

; ; (setq myPolyline  (vlax-ename->vla-object (car (entsel))))
; ; (setq coordinates 
    ; ; (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates myPolyline)))
; ; )

  ; ; (defun foo (lst)
    ; ; (if lst
      ; ; (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
    ; ; )
  ; ; )
; ; (setq coordinates (foo coordinates))

; ; (princ "\nwe have ")(princ (length coordinates))(princ " vertices.\n")

; ; (setq coordinates 
    ; ; (gc:2dVariantToPointList
        ; ; (vla-get-Coordinates myPolyline)
    ; ; )
; ; )

; ; (princ "\nwe have ")(princ (length coordinates))(princ " vertices.\n")

; (command "._UNDO" "_End")
; (princ)