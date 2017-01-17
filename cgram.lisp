(defpackage #:pcc.cgram (:use #:cl #:pcc.pass1))

(in-package #:pcc.cgram)

(defvar cftnod (make-p1nd))

(defun mkty (_t d sue)
  (_block 'TYPE nil nil _t d sue))

;* Traverse an unhandled expression tree bottom-up and call buildtree()
;* or equivalent as needed.

(defun eve(p)
  (let ((p1 (p1nd-n_left p))
	(p2 (p1nd-n_right p)))
    (case (p1nd-n_op p)
      ((NAME)
       (let ((sp (lookup (p1nd-n_sp p)
			 (if (attr_find (p1nd-n_ap p) 'ATTR_P1LABELS)
			     (make-stype :id 'SLBLNAME :mod '(STEMP))
			     (make-stype :id 'SNORMAL)))))
	 (when (member 'SINLINE (stype-mod (symtab-sflags sp)))
	   (inline_ref sp))
	 (let ((r (nametree sp)))
	   (error "Unfinished")))))))
	 
