; find all hatches in the active space and convert to solids.
; tweak MESHOPTIONS as desired to control the resolution of the interpolation of smooth curves into straight-line segments)

; caution: we are relying on an experimental promises system, which rquires a bit of care to wrap your brain around. (necessitated by meshsmooth failing to run properly when invoked by (command-s), which means that we have to use Document::SendCommand tpo invoke meshsmooth and then let the lisp script fall through the bottom before the meshsmooth actually occurs.).
(load "variantArrayConversion.lsp")
(load "regionFunctions.lsp")


(command "._UNDO" "_Begin")

(setq allSolids (list))
(setq numberOfHatchesConverted 0)
(vlax-for entity (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if
        (and 
            (= (vla-get-ObjectName entity) "AcDbHatch")
        )
        (progn
            (convertHatchToSolids entity
                '(lambda (result)
                    (setq numberOfHatchesConverted (+ numberOfHatchesConverted 1))
                    (setq solids result)
                    (setq allSolids 
                        (append 
                            allSolids
                            solids
                        )
                    )
                    (princ (strcat "converted a hatch into " (itoa (length solids)) " solids.\n"))
                )
            )
        )
    )
)


;; put cleanup activity into a promise:
(setq promiseId (GUID))
(vla-SendCommand 
    (vla-get-ActiveDocument (vlax-get-acad-object))
    (strcat
        ;;process the promise: 
        "(progn "
            "(princ \"now fulfilling promise " promiseId ", with \")  (princ (- (length promises) 1)) (princ \" promises remaining.\\n\")"
            "(setq promise (cdr (assoc \"" promiseId "\" promises))) "
            "(setq promises (vl-remove (cdr (assoc \"" promiseId "\" promises)) promises)) "
            "(apply "
                "(nth 1 promise) "
                "(nth 0 promise) "
            ") "
        ")"
        "\n"
    )
 )
(setq symbolsToSave
    '(
        ;;list symbols to save here:
        ; allSolids
        ; numberOfHatchesConverted
    )
)
(setq promise
    (list
        (mapcar 'eval symbolsToSave)
        (list
            'lambda 
            (append 
                symbolsToSave
                '( /
                    ;local variables go here:
                    myLocalVar
                )
            )
           
            '(progn ; wrapping in progn is necessary in order to get local vars to behave correctly.
                (setq myLocalVar "INTERNAL VALUE BETTER NOT MAKE IT OUT")
                (princ (strcat "converted " (itoa numberOfHatchesConverted) " hatches into " (itoa (length allSolids)) " solids.\n"))
                (command "._UNDO" "_End")
            )
            ;==========
            ; ;;
        )
    )
)
(setq promises (append promises (list (cons promiseId promise))))


(princ)

