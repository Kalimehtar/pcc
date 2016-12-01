(defpackage #:pcc.amd64-code
  (:use #:cl #:pcc.pass1)
  (:shadow
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
   #:node-n_dcon))
   

(in-package #:pcc.amd64-code)

(defun make-node (&rest args) (apply #'make-p1nd args))
(defun node-n_op (n) (p1nd-n_op n))
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
	(rp (bstruct nil 'SNAME nil)))
    1))
