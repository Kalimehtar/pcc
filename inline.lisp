(defpackage #:pcc.inline
  (:use #:cl #:pcc.pass1)
  (:import-from #:pcc.cgram #:cftnod))

(in-package #:pcc.inline)

(defvar isinlining 0)
(defvar inlstatcnt 0)
(defvar sdebug 0)

(defstruct ntds temp type ds attr)

;; ilink from ipole points to the next struct in the list of functions

(defstruct istat
  link
  (sp nil :type (or null symtab))
  (flags nil :type list)
  ;; CANINL       /* function is possible to inline */
  ;; WRITTEN      /* function is written out */
  ;; REFD         /* Referenced but not yet written out */
  nt ;  Array of arg temp type data
  nargs ; number of args in array :: Monk: delete?
  retval ; number of return temporary
  shead)

(defvar cifun)
(defvar nlabs)
(defvar ipole (mk-slist2))

(defun _SDEBUG (&rest x)
  (when sdebug (apply #'format t x)))

(defun refnode (sp)
  (declare (type symtab sp))
  (_SDEBUG "refnode(~a)~%"  (symtab-sname sp))
  (let ((ip (make-interpass :type 'IP_REF)))
    (setf (ip_name ip) sp)
    (inline_addarg ip)))

(defun inapcopy (ap)
  "Save attributes permanent."
  (declare (type attr ap))
  (let ((nap (attr_dup ap)))
    (when (attr-next ap)
      (setf (attr-next nap) (inapcopy (attr-next ap))))
    nap))

(defun intcopy (p)
  "Copy a tree onto the permanent heap to save for inline."
  (let ((q (copy-structure p))
	(o (coptype (node-n_op p)))) ;  XXX pass2 optype?
    (when (and (> nlabs 1)
	       (or (eq (node-n_op p) 'REG)
		   (eq (node-n_op p) 'OREG))
	       (eq (regno p) 'FPREG))
      (setf (istat-flags (SLIST_FIRST ipole))
	    (remove 'CANINL
		    (istat-flags (SLIST_FIRST ipole))))) ; no stack refs
    (when (node-n_ap q)
      (setf (node-n_ap q) (inapcopy (node-n_ap q))))
    (when (member (node-n_op q) '(NAME ICON XASM XARG))
      (setf (node-n_name q)
	    (if (node-n_name q) (copy-seq (node-n_name q)) "")))
    (when (eq o 'BITYPE)
      (setf (node-n_right q) (intcopy (node-n_right q))))
    (unless (eq o 'LTYPE)
      (setf (node-n_left q) (intcopy (node-n_left q))))
    q))

(let ((g (vector nil)))
  (defun inline_addarg (ip)
    (_SDEBUG "inline_addarg(~a)~%" ip)
    (DLIST_INSERT_BEFORE (istat-shead cifun) ip #'interpass-qelem)
    (case (interpass-type ip)
      ((IP_ASM) (setf (ip_asm ip) (copy-seq (ip_asm ip))))
      ((IP_DEFLAB) (incf nlabs))
      ((IP_NODE) (let ((q (ip_node ip)))
		   (setf (ip_node ip) (intcopy (ip_node ip)))
		   (tfree q)))
      ((IP_EPILOG)
       (when (aref (interpass_prolog-ip_labels ip) 0)
	 (uerror "no computed goto in inlined functions"))
       (setf (interpass_prolog-ip_labels ip) g)))
    (when cftnod (setf (istat-retval cifun) (regno cftnod)))))

(defun ialloc ()
  (incf inlstatcnt)
  (make-istat))

;;* Called when an inline function is found, to be sure that it will
;;* be written out.
;;* The function may not be defined when inline_ref() is called.
(defun inline_ref (sp)
  (_SDEBUG "inline_ref(\"~a\")~%" (symtab-sname sp))
  (when (eq (symtab-sclass sp) 'SNULL)
    (return-from inline_ref)) ; /* only inline, no references */
  (cond
    (isinlining
     (refnode sp))
    (t
     (SLIST_FOREACH w ipole istat-link
       (when (eq (istat-sp w) sp)
	 (pushnew 'REFD (istat-flags w))))
     ;; function not yet defined, print out when found
     (let ((w (ialloc)))
       (setf (istat-sp w) sp)
       (pushnew 'REFD (istat-flags w))
       (SLIST_INSERT_FIRST ipole w istat-link)
       (DLIST_INIT (istat-shead w) #'interpass-qelem)))))
