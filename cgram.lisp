(in-package #:pcc)

;* Traverse an unhandled expression tree bottom-up and call buildtree()
;* or equivalent as needed.

(defun eve(p)
  (let ((p1 (node-left p))
	(p2 (node-right p)))
    (case (node-op p)
      ((NAME)
       (let ((sp (lookup (node-sp p)
			 (if (attr_find (node-ap p) 'ATTR_P1LABELS)
			     (make-sstype :id 'SLBLNAME :mod '(STEMP))
			     (make-sstype :id 'SNORMAL)))))
	 (when (member 'SINLINE (sstype-mod (symtab-sflags sp)))
	   (inline_ref sp))
	 
