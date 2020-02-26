;; Defines a user command that is a wrapper around dcaligned, which removes the user interaction required to 
;; place the dimension text in the model (instead placing it automatically at an arbitrary position),
;; and allows noun-verb command flow (i.e. select the object, then run the command).

;; apparently, constraints are not yet built into the high-level object model of autocad (see 
;; http://adndevblog.typepad.com/autocad/2013/01/a-simplified-net-api-for-accessing-autocad-parameters-and-constraints.html )
;; so what I was hoping to do is not feasible, the way I wanted to do it.
;; This means we cannot achieve noun-verb command flow.  However, I think we can still automate the
;; placement of dimension text on the model.

(vl-load-com)

(defun C:goodDim ; applies an 'aligned' dimension constraint to the currently selected object.
	( / theSelectionSet eNameOfSelectedObject x)
	(setq x (entsel))
	(setq selectionPoint (nth 1 x))
	(princ "you clicked " ) (princ selectionPoint) (princ ".  ") (princ "\n")
	
	(setq newParameterName "d17")
	(setq newParameterName "")
	(command-s "DCALIGNED" "Object" selectionPoint selectionPoint (strcat newParameterName "="))
	;(command-s "DCALIGNED" "Object" selectionPoint selectionPoint "")

	
	
	(princ)
)

(C:goodDim)