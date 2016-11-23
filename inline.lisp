(in-package #:pcc)

(defvar isinlining 0)
(defvar inlstatcnt 0)

(defun SDEBUG (&rest x)
  (when sdebug (apply #'format t x)))

;;* Called when an inline function is found, to be sure that it will
;;* be written out.
;;* The function may not be defined when inline_ref() is called.
(defun inline_ref (sp)
  (SDEBUG "inline_ref(\"~a\")~%" (symtab-sname sp))
  (when (eq (symtab-sclass sp) 'SNULL)
    (return-from inline-ref)) ; /* only inline, no references */
  (when isinlining
