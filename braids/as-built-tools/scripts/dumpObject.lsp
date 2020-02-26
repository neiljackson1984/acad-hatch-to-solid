;;---------------------=={ Dump Object }==--------------------;;
;;                                                            ;;
;;  Lists the properties & methods of a supplied VLA-Object   ;;
;;  or VLA-Object equivalent of a supplied ename or DXF list. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  obj - VLA-Object, Entity Name, or Entity DXF List         ;;
;;------------------------------------------------------------;;

(defun c:dump  nil (LM:dump (car (entsel))))
(defun c:dumpn nil (LM:dump (car (nentsel))))

(defun LM:dump ( obj )
    (cond
        (   (or (= 'ename (type obj))
                (and (listp obj) (= 'ename (type (setq obj (cdr (assoc -1 obj))))))
            )
            (vlax-dump-object (vlax-ename->vla-object obj) t)
        )
        (   (= 'vla-object (type obj))
            (vlax-dump-object obj t)
        )
    )
    (princ)
)
(vl-load-com) (princ)
