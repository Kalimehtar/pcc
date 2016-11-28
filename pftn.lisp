(defpackage #:pcc.pftn
  (:use #:cl
	#:pcc.pass1))

(in-package #:pcc.pftn)

(defvar blevel 0)
(defvar reached nil) ;; Monk: boolean

;(defun bstruct (name soru gp)
;  )

(defun getsymtab (name flags)
  (make-symtab :sname name
	       :stype 'UNDEF
	       :squal 0
	       :sclass 'SNULL
	       :sflags (make-sstype :id flags)
	       :soffset 0
	       :slevel blevel)) 
