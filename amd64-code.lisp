(defpackage #:pcc.amd64-code
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

(in-package #:pcc.amd64-code)

(defun make-node (&rest args) (apply #'make-p1nd args))
(defun node-n_op (n) (p1nd-n_op n))
(defun node-n_type (n) (p1nd-n_type n))
(defun (setf node-n_type) (val n) (setf (p1nd-n_type n) val))
(defun node-n_ap (n) (p1nd-n_ap n))
(defun (setf node-n_ap) (val n) (setf (p1nd-n_ap n) val))
(defun nfree (x) (p1nfree x))
;;; TODO: all node*

(defun bjobcode ()
  (setf (gethash 'INT astypnames) (tab ".long"))
  (setf (gethash 'UNSIGNED astypnames) (tab ".long"))
  (setf (gethash 'LONG astypnames) (tab ".quad"))
  (setf (gethash 'ULONG astypnames) (tab ".quad"))
  (let ((gp_offset (addname "gp_offset"))
	(fp_offset (addname "fp_offset"))
	(overflow_arg_area (addname "overflow_arg_area"))
	(reg_save_area (addname "reg_save_area"))
	(rp (bstruct nil 'SNAME nil))
	(p (_block 'NAME nil nil 'UNSIGNED 0 0)))
    (soumemb p gp_offset 0)
    (soumemb p fp_offset 0)
    (setf (node-n_type p) (make-stype :id 'VOID :mod '(PTR))
	  (node-n_ap p) nil)
    (soumemb p overflow_arg_area 0)
    (soumemb p reg_save_area 0)
    (nfree p)
    (let ((q (dclstruct rp))
	  (c (addname "__builtin_va_list")))
      (setf p (_block 'LB (bdty 'NAME c) (bcon 2) 'INT 0 0)))))
	  
	    
(defun fldty (p)
  "fix up type of field p"
  (declare (type (or null symtab) p)
	   (ignore p))
  nil)

