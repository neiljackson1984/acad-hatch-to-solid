(setq weAreInAnAcadDocScript T) ;;this should be the first line of this acaddoc script
(progn 
	(defun LM:sfsp+ ( lst )
		(   (lambda ( str lst )
				(if (setq lst
						(vl-remove-if
						   '(lambda ( x )
								(or (vl-string-search (strcase x) (strcase str))
									(not (findfile x))
								)
							)
							lst
						)
					)
					(setenv "ACAD" (strcat str ";" (apply 'strcat (mapcar '(lambda ( x ) (strcat x ";")) lst))))
				)
			)
			(vl-string-right-trim ";" (getenv "ACAD"))
			(mapcar '(lambda ( x ) (vl-string-right-trim "\\" (vl-string-translate "/" "\\" x))) lst)
		)
	)


	;; thanks to https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings for splitStr
	; in AutoLisp usage (splitStr "get off of my cloud" " ") returns (get off of my cloud)
	(defun splitStr (src delim / word letter)

	  (setq wordlist (list))
	  (setq cnt 1)
	  (while (<= cnt (strlen src))

		(setq word "")

		(setq letter (substr src cnt 1))
		(while (and (/= letter delim) (<= cnt (strlen src)) ) ; endless loop if hits NUL
		  (setq word (strcat word letter))
		  (setq cnt (+ cnt 1))      
		  (setq letter (substr src cnt 1))
		) ; while

		(setq cnt (+ cnt 1))
		(setq wordlist (append wordlist (list word)))

	  )

	  wordlist

	)






	 

	 ;;This function is analagous to php's 'implode' function - it takes a list of strings and a delimiter,
	 ;; and returns a string that consists of all the strings in the list merged together, in order, delimited by the delimiter.
	 ;; implode is the inverse of splitStr (or, very nearly the inverse, if we do not allow the delimeter to appear within any of the elements).
	(defun implode ( delimiter listOfStrings /
		returnValue
		)
		(setq returnValue
			(apply 'strcat
				(mapcar 
					'(lambda (x) (strcat x delimiter))
					listOfStrings
				)
			)
		)
		;; the above will have left one extra trailing delimiter, that we will now remove.
		(setq returnValue (substr returnValue 1 (- (strlen returnValue) (strlen delimiter))))
		
		returnValue
	)
	 
	;; given a semicolon-delimited string mainString, this function will return a new semicolon-delimitedd string in which the first element is newElement, and if newElement 
	;; was already in mainString (case insensitive), it will not appear again in the result beyond the first element
	;; 'new' may be either a string or a list of strings.  if it is a list, all will be prepended to the mainString.
	(defun prependUniquelyToSemicolonDelimitedString (mainString new /
			elements
			delimiter
			returnValue
			newElements
		)
		(setq delimiter ";")
		(setq elements (splitStr mainString delimiter))
		(setq newElements
			(if (listp new) 
				new 
				(list new)
			)
		)
		
		
		;; remove the new element from the exisiting elements, if it already existed there
		(setq elements 
			(vl-remove-if
				'(lambda (x) 
					(member 
						(strcase x) 
						(mapcar 'strcase newElements)
					)
				) ;;returns non-nil iff. x is a member of newElements (case insensitive)
				elements
			)
		)
		
		(setq elements (append newElements elements))
		
		(setq returnValue (implode delimiter elements))
		returnValue
	)
	 

	(defun prependUniquelyToPreferencesFilesProperty (nameOfProperty new / 
		)
		(vlax-put-property 
			(vla-get-Files (vla-get-Preferences (vlax-get-acad-object))) 
			nameOfProperty
			(prependUniquelyToSemicolonDelimitedString
				(vlax-get-property 
					(vla-get-Files (vla-get-Preferences (vlax-get-acad-object)))
					nameOfProperty
				)
				new
			)
		)
	)

	(defun prependUniquelyToPrinterConfigPath       (new)   (prependUniquelyToPreferencesFilesProperty "PrinterConfigPath"      new)  )
	(defun prependUniquelyToPrinterDescPath         (new)   (prependUniquelyToPreferencesFilesProperty "PrinterDescPath"       new)  )
	(defun prependUniquelyToPrinterStyleSheetPath   (new)   (prependUniquelyToPreferencesFilesProperty "PrinterStyleSheetPath"  new)  )
	(defun prependUniquelyToSupportPath             (new)   (prependUniquelyToPreferencesFilesProperty "SupportPath"            new)  )
	 
	 
	 (defun getDirectoryOfActiveDocument ()
		(vl-filename-directory (vla-get-FullName (vla-get-ActiveDocument (vlax-get-acad-object))))
	)


	;; if you want to load another acaddoc.lsp script (in the parent directory of the current script, for instance),
	;; use the function (loadAScript ), which will set the directoryOfThisScript variable and then load
	;; the requested script.  This function pays attention to the variable weAreInAnAcadDocScript, which the user should
	;; should set at the beginning of each acaddoc.lsp file and unset at the end.  (the LoadAScript() function also unsets weAreInAnAcadDocScript
	;; immediately before loading the new script, and then sets it immediately afterward.
	;; As long as all loads within this acaddoc.lsp file are done via the loadAScript() function, the getDirectoryOfThisScript() function should work as expected (it might return nil, but if it returns a string, then you can be confident that that string is the directory of the current script).
	(defun getDirectoryOfThisScript ()
		(if directoryOfThisScript 
			directoryOfThisScript 
			(if weAreInAnAcadDocScript 
				(vl-filename-directory (findfile "acaddoc.lsp"))
				nil
			)
		)
	)

	(defun loadAScript (x /
		initialValueOfWeAreInAnAcadDocScript
		initialValueOfDirectoryOfThisScript
		)
		(if (not (findfile x)) (alert (strcat "warning from " (getDirectoryOfThisScript) "\\" "acaddoc.lsp: \n" "failed to find file " x)))
		(setq initialValueOfDirectoryOfThisScript directoryOfThisScript)
		(setq directoryOfThisScript (vl-filename-directory (findfile x)))
		(setq initialValueOfWeAreInAnAcadDocScript weAreInAnAcadDocScript)
		(setq weAreInAnAcadDocScript nil)
		(load x)
		(setq weAreInAnAcadDocScript initialValueOfWeAreInAnAcadDocScript)
		(setq directoryOfThisScript initialValueOfDirectoryOfThisScript)
		(princ)
	)
)
;;===========================================
;; DO NOT MODIFY ABOVE THIS LINE 
;; THE MEAT OF THIS SCRIPT GOES BELOW
;;===========================================

; (setq pathOfTheAutoloadFolder (strcat (getDirectoryOfThisScript) "\\braids\\evans_cad_standard\\scripts\\autoload"))
;; load each *.lsp file in the autoload folder.
(foreach filename (vl-directory-files pathOfTheAutoloadFolder "*.lsp" 1) ; the '1' causes the function to list files only (not folders).
    (loadAScript (strcat pathOfTheAutoloadFolder "\\" filename))
)



; ; PrinterConfigPath is where autocad looks for pc3 files (a pc3 file 
; ; encapsulates a printer driver and a particular choice (or choices) of 
; ; the printer driver specific settings. The printer driver that a pc3 
; ; encapsulates can be either an operating system printer (i.e. Windows 
; ; printers) or some other sorts of output drivers, of which "DWG to PDF" 
; ; is an example.) PrinterDescPath is where autocad looks for pmp files 
; ; (which define printer spatial calibration data (which is generally 
; ; irrelevant for pdf output) and custom page sizes (including custom 
; ; printable area margins)) PrinterStyleSheetPath is where autocad looks 
; ; for ctb and stb files. 



;; it appears that autocad descends into subdirectories to search for ctb files and pc3 files.


;; 2019/08/01: added the "." entry below as a hack to fix a problem that seems to have arisen in recent versions of autocad 
;; in which AcadPreferencesFiles::SupportPath replaces any "." entry in the underlying value with the resolved absolute path
;; of the current working directory.  This tendency of autocad means that, when we modify the support path setting (by reading 
;; the current value (via the AcadPreferencesFiles::SupportPath property), modifying it, and then writing the modified value 
;; back to the property), any "." entry that happened to 
;; exist in the original value of the SupportPath setting is replaced with a fully-qualified absolute path.
;; Forcefully injecting a "." entry into the value that we write to the SupportPath setting is a work-around to this problem.
(prependUniquelyToSupportPath              (list "." (findfile (strcat (getDirectoryOfThisScript) "\\" "braids\\as-built-tools\\" "scripts") ))   )

;; default autocad fonts folder (which is, by default, among the paths in the Support path): %programfiles%\autocad 2018\fonts


(setvar "PLOTOFFSET" 1)


;;===================================================
;;  DO NOT MODIFY BELOW THIS LINE
;;===================================================
(setq weAreInAnAcadDocScript nil)(princ) ;;this should be the last line of this script.
