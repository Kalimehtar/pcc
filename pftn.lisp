(defpackage #:pcc.pftn
  (:use #:cl
	#:pcc.pass1)
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
   #:node-n_dcon)
  (:export
   #:symtabcnt))

(in-package #:pcc.pftn)

(defvar blevel 0)
(defvar reached nil) ;; Monk: boolean

(defvar symtabcnt) ;; statistics

(deftype node () 'p1nd)

;; Linked list stack while reading in structs.
(defstruct rstack
  rnext rsou rstr
  (rsym nil :type (or null symtab))
  (rb nil :type (or null symtab))
  (ap nil :type attr)
  flags)

(defvar rpole)

;; basic attributes for structs and enums

(defun seattr ()
  (attr_add (attr_new 'ATTR_ALIGNED  4) (attr_new 'ATTR_STRUCT 2)))

(defun defstr (sp class)
  "Struct/union/enum symtab construction."
  (setf (symtab-sclass sp) class)
  (case class
    ((STNAME) (setf (symtab-stype sp) 'STRTY))
    ((UNAME) (setf (symtab-stype sp) 'UNIONTY))
    ((ENAME) (setf (symtab-stype sp) 'ENUMTY))))
    
;; * Declare a struct/union/enum tag.
;; * If not found, create a new tag with UNDEF type.
(defun deftag (name class)
  (let ((sp (lookup name 'STAGNAME)))
    (cond
      ((null (symtab-sap sp))
       (defstr sp class))
      ((not (eq (symtab-sclass sp) class))
       (uerror "tag ~a redeclared" name)))
    sp))

;; * begining of structure or union declaration
;; * It's an error if this routine is called twice with the same struct.
(defun bstruct (name soru gp)
  (declare (type (or null string) name)
	   (type symbol soru)
	   (type node gp)
	   (ignorable gp))
  (let ((gap
	 #+GCC_COMPAT (if gp (gcc_attr_parse gp) nil)
	 #-GCC_COMPAT nil))
    (if name
	(let ((sp (deftag name soru)))
	  (unless (symtab-sap sp)
	    (setf (symtab-sap sp) (seattr)))
	  (let ((ap (attr_find (symtab-sap sp) 'ATTR_ALIGNED)))
	    (when (/= (iarg ap 0) 0)
	      (cond
		((< (symtab-slevel sp) blevel)
		 (setf sp (hide sp))
		 (defstr sp soru)
		 (setf (symtab-sap sp) (seattr)))
		(t (uerrror "~a redeclared" name))))
	    (setf gap (setf (symtab-sap sp) (attr_add (symtab-sap sp) gap)))))
	(progn
	  (setf gap (attr_add (seattr) gap))
	  (setf sp nil)))
    (let ((r (make-rstack :rsou soru :rsym sp :rb nil :ap gap :rnext rpole)))
      (setf rpole r)
      r)))

(defun getsymtab (name flags)
  (make-symtab :sname name
	       :stype 'UNDEF
	       :squal 0
	       :sclass 'SNULL
	       :sflags (make-stype :id flags)
	       :soffset 0
	       :slevel blevel)) 
