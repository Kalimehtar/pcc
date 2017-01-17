(defpackage #:pcc.local
  (:use #:cl #:pcc.pass1)
  (:shadow
   #:node
   #:make-node
   #:node-n_op
   #:node-n_type
   #:node-n_qual
   #:node-n_su
   #:node-n_ap
   #:node-n_reg
   #:node-n_regw
   #:node-n_name
   #:node-n_df
   #:node-n_label
   #:node-n_val
   #:node-n_left
   #:node-n_slval
   #:node-n_right
   #:node-n_rval
   #:node-n_sp
   #:node-n_dcon
   #:nfree
   #:ccopy
   #:tfree))

(in-package #:pcc.local)

(deftype node () 'p1nd)
(defun node-n_op (n) (p1nd-n_op n))
(defun node-n_sp (n) (p1nd-n_sp n))
(defun (setf node-n_val) (val n) (setf (p1nd-n_val n) val))


(defun clocal (p)
  (declare (type node p))
  #+PCC_DEBUG
  (when (/= xdebug 0)
    (format "clocal: ~a~%" p)
    (fwalk p eprint 0))
  (let ((o (node-n_op p)))
    (case o
      ((NAME)
       (let ((q (node-n_sp p)))
	 (unless q (return-from clocal))
	 (case (symtab-sclass q)
	   ((PARAM AUTO)
	    ;; /* fake up a structure reference */
	    (let ((r (_block 'REG
			     nil nil
			     (make-stype :id 'STRTY :mod '(PTR)) 0 0)))
	      (slval r 0)
	      (setf (node-n_val r) 'FPREG)
	      (setf p (stref (_block 'STREF r p 0 0 0)))))
	   (error  "Unfinished")
	   ))))))
  

(defun cisreg (_t)
  "Return t if a variable of type _t is OK to put in register."
  (if (equalp _t (make-stype :id 'LDOUBLE)) nil t))

(defun ctype (type)
  "map types which are not defined on the local machine"
  (case (BTYPE type)
    ((LONGLONG) (MODTYPE type (make-stype :id 'LONG)))
    ((ULONGLONG) (MODTYPE type (make-stype :id 'ULONG))))
  type)


