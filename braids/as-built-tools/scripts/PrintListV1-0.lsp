;; Print List  -  Lee Mac
;; Prints a supplied list to the command-line or to a given filename,
;; with nested lists displayed in a hierarchical format.
;; l - [lst] List to print
;; f - [str] Optional filename

(defun LM:princl ( l f / _print _princ d r )
    
    (defun _print ( l i )
        (if (and (= 'list (type l)) (vl-list-length l) (vl-some 'vl-consp l))
            (progn
                (_princ (strcat "\n" i "("))
                (foreach x l (_print x (strcat i "    ")))
                (_princ (strcat "\n" i ")"))
            )
            (_princ (strcat "\n" i (vl-prin1-to-string l)))
        )
    )

    (eval
        (list 'defun '_princ '( x )
            (if (and (= 'str (type f)) (setq d (open f "w")))
                (list 'princ 'x d)
               '(princ x)
            )
        )
    )

    (setq r (vl-catch-all-apply '_print (list l "")))
    (if (= 'file (type d))
        (progn
            (setq d (close d))
            (startapp "notepad" f)
        )
    )
    (if (vl-catch-all-error-p r)
        (prompt (vl-catch-all-error-message r))
        l
    )
)

(defun princl ( l ) (LM:princl l nil) (princ))
(defun princf ( l ) (LM:princl l (vl-filename-mktemp "list" (getvar 'dwgprefix) ".txt")) (princ))
(princ)