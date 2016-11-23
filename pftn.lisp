(in-package #:pcc)

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
