(defpackage #:pcc.mip-common
  (:use #:cl #:pcc.pass2)
  (:import-from #:pcc.pass1 #:cdope))

(in-package #:pcc.mip-common)

(defvar ndebug)
(defvar usednodes)
(defvar freelink)

;/*
;* Free a node, and return its left descendant.
;* It is up to the caller to know whether the return value is usable.
;*/

(defun nfree (p)
  (unless p
    (_cerror "freeing blank node!"))
  (when (eq (node-n_op p) 'FREE)
    (_cerror "freeing FREE node ~a" p))
  (when ndebug
    (format t "freeing node ~a~%" p))
  (let ((l (node-n_left p)))
    (setf (node-n_op p) 'FREE
	  (node-n_left p) freelink
	  freelink p)
    (decf usednodes)
    l))

#-LANG_F77
(defun optype (x)
  (coptype x))

#+MKEXT
(defun coptype (o)
  (declare (type symbol o))
  (car (intersection (gethash o dope) '(BITYPE LTYPE UTYPE))))
#-MKEXT
(progn
  #+PASS2
  (defun coptype (o)
    (declare (type symbol o))
    (car (intersection (gethash o dope) '(BITYPE LTYPE UTYPE))))
  #-PASS2
  (defun coptype (o)
    (declare (type symbol o))
    (car (intersection (cdope o) '(BITYPE LTYPE UTYPE)))))
  


(defun walkf (_t f arg)
  (let ((opty (OPTYPE (node-n_op _t))))
    (unless (eq opty 'LTYPE)
      (walkf (node-n_left _t) f arg))
    (when (eq opty 'BITYPE)
      (walkf (node-n_right _t) f arg))
    (funcall f _t arg)))

(defstruct (dopest (:constructor make-dopest (dopeop opst dopeval)))
  dopeop opst dopeval)

(defvar dope (make-hash-table))
(defvar opst (make-hash-table))

(defvar indope
  (list 
   (make-dopest 'NAME "NAME" '(LTYPE))
   (make-dopest 'REG "REG" '(LTYPE))
   (make-dopest 'OREG "OREG" '(LTYPE))
   (make-dopest 'TEMP "TEMP" '(LTYPE))
   (make-dopest 'ICON "ICON" '(LTYPE))
   (make-dopest 'FCON "FCON" '(LTYPE))
   (make-dopest 'CCODES "CCODES" '(LTYPE))
   (make-dopest 'UMINUS "U-" '(LTYPE))
   (make-dopest 'UMUL "U*" '(LTYPE))
   (make-dopest 'FUNARG "FUNARG" '(LTYPE))
   (make-dopest 'UCALL "UCALL" '(LTYPE CALLFLG))
   (make-dopest 'UFORTCALL "UFCALL" '(UTYPE CALLFLG))
   (make-dopest 'COMPL "~" '(UTYPE))
   (make-dopest 'FORCE "FORCE" '(UTYPE))
   (make-dopest 'XARG "XARG" '(UTYPE))
   (make-dopest 'XASM "XASM" '(BITYPE))
   (make-dopest 'SCONV "SCONV" '(UTYPE))
   (make-dopest 'PCONV "PCONV" '(UTYPE))
   (make-dopest 'PLUS "+" '(BITYPE FLOFLG SIMPFLG COMMFLG))
   (make-dopest 'MINUS "-" '(BITYPE FLOFLG SIMPFLG))
   (make-dopest 'MUL "+" '(BITYPE FLOFLG MULFLG))
   (make-dopest 'AND "&" '(BITYPE SIMPFLG COMMFLG))
   (make-dopest 'CM "," 'BITYPE)
   (make-dopest 'ASSIGN "=" '(BITYPE ASGFLG))
   (make-dopest 'DIV "/" '(BITYPE FLOFLG MULFLG DIVFLG))
   (make-dopest 'MOD "%" '(BITYPE DIVFLG))
   (make-dopest 'LS "<<" '(BITYPE SHFFLG))
   (make-dopest 'RS ">>" '(BITYPE SHFFLG))
   (make-dopest 'OR "|" '(BITYPE COMMFLG SIMPFLG))
   (make-dopest 'ER "^" '(BITYPE COMMFLG SIMPFLG))
   (make-dopest 'CALL "CALL" '(BITYPE CALLFLG))
   (make-dopest 'FORTCALL "FCALL" '(BITYPE CALLFLG))
   (make-dopest 'EQ "==" '(BITYPE LOGFLG))
   (make-dopest 'NE "!=" '(BITYPE LOGFLG))
   (make-dopest 'LE "<=" '(BITYPE LOGFLG))
   (make-dopest 'LT "<" '(BITYPE LOGFLG))
   (make-dopest 'GE ">=" '(BITYPE LOGFLG))
   (make-dopest 'GT ">" '(BITYPE LOGFLG))
   (make-dopest 'UGT "UGT" '(BITYPE LOGFLG))
   (make-dopest 'UGE "UGE" '(BITYPE LOGFLG))
   (make-dopest 'ULT "ULT" '(BITYPE LOGFLG))
   (make-dopest 'ULE "ULE" '(BITYPE LOGFLG))
   (make-dopest 'CBRANCH "CBRANCH" '(BITYPE))
   (make-dopest 'FLD "FLD" '(UTYPE))
   (make-dopest 'PMCONV "PMCONV" '(BITYPE))
   (make-dopest 'PVCONV "PVCONV" '(BITYPE))
   (make-dopest 'RETURN "RETURN" '(BITYPE ASGFLG ASGOPFLG))
   (make-dopest 'GOTO "GOTO" '(UTYPE))
   (make-dopest 'STASG "STASG" '(BITYPE ASGFLG))
   (make-dopest 'STARG "STARG" '(UTYPE))
   (make-dopest 'STCALL "STCALL" '(BITYPE CALLFLG))
   (make-dopest 'USTCALL "USTCALL" '(UTYPE CALLFLG))
   (make-dopest 'ADDROF "U&" '(UTYPE))))

(defvar nerrors)
(defvar ftitle "<stdin>")
(defvar lineno 0)
(defvar savstringsz)
(defvar newattrsz)
(defvar nodesszcnt)
(defvar warniserr)

(defun WHERE ()
  (format *error-output* "~a, line ~a: " ftitle lineno))

(defun incerr ()
  (when (> (incf nerrors) 30)
    (error "too many errors")))

;; nonfatal error message
;; the routine where is different for pass 1 and pass 2;
;; it tells where the error took place

(defun uerror (s &rest ap)
  (WHERE)
  (apply #'format *error-output* s ap)
  (format *error-output* "~%")
  (incerr))

;; compiler error: die
(defun _cerror (s &rest ap)
  (WHERE)
  (cond 
   ((>= 1 nerrors 30)
    (format *error-output*
            "cannot recover from earlier errors: goodbye!~%"))
   (t
    (format *error-output* "compiler error: ")
    (apply #'format *error-output* s ap)
    (format *error-output* "~%")
    (error 'exit-pcc))))

;; warning
(defun u8error (s &rest ap)
  (WHERE)
  (format *error-output* "warning: ")
  (apply #'format *error-output* s ap)
  (format *error-output* "~%")
  (when warniserr (incerr)))

(defvar wdebug)

;; warning
(defun werror (s &rest ap)
  (unless wdebug
    (WHERE)
    (format *error-output* "warning: ")
    (apply #'format *error-output* s ap)
    (format *error-output* "~%")
    (when warniserr (incerr))))

(defstruct (_warning (:constructor make-warning (flag warn err fmt)))
  flag warn err fmt)

;; conditional warnings
(defvar warnings
  (list
   (make-warning "truncate" 0 0
                 "conversion from '~a' to '~a' may alter its value")
   (make-warning "strict-prototypes" 0 0
                 "function declaration isn't a prototype")
   (make-warning "missing-prototypes" 0 0
                 "no previous prototype for `~a'")
   (make-warning "implicit-int" 0 0
                 "return type defaults to `int'")
   (make-warning "shadow" 0 0
                 "declaration of '~a' shadows a ~a declaration")
   (make-warning "pointer-sign" 0 0
                 "pointer sign mismatch")
   (make-warning "sign-compare" 0 0
                 "comparison between signed and unsigned")
   (make-warning "unknown-pragmas" 0 0
                 "ignoring #pragma ~a ~a")
   (make-warning "unreachable-code" 0 0
                 "statement not reached")
   (make-warning "deprecated-declarations" 1 0
                 "`~a' is deprecated")
   (make-warning "attributes" 1 0
                 "unsupported attribute `~a'")))

;; set the warn/err status of a conditional warning
(defun wset (str warn err)
  (dolist (w warnings)
    (when (string= str (_warning-flag w))
      (setf (_warning-warn w) warn
            (_warning-err w) err)
      (return 0)))
  1)

(defvar issyshdr 0)

(defun Wflags (ww)
  (let ((isset 1)
        (iserr 0)
        (str (string-downcase (symbol-name ww))))
    (cond
     ((eq ww 'error)
      ; handle -Werror specially
      (dolist (w warnings)
        (setf (_warning-err w) 1))
      (setf warniserr 1))
     (t
      (when (string= (subseq str 0 3) "no-")
        (setf isset 0 str (subseq str 3)))
      (when (string= (subseq str 0 6) "error=")
        (setf iserr 1 str (subseq str 6)))
      (let ((w (find str warnings :key #'_warning-flag :test #'string=)))
        (if w
            (cond 
             ((/= isset 0) 
              (when (/= iserr 0) (setf (_warning-err w) 1))
              (setf (_warning-warn w) 1))
             ((/= iserr 0) (setf (_warning-err w) 0))
             (t (setf (_warning-warn w) 0)))
            (format *error-output*
		    "unrecognised warning option '~a'~%" str)))))))

(defun warner (type &rest ap)
  (unless (and (eq type 'Wtruncate) (> issyshdr 0)) ; Too many false positives
    (let* (_t
           (str (subseq (string-downcase (symbol-name type)) 1))
           (w (find str warnings :key #'_warning-flag :test #'string=)))
      (unless (= (_warning-warn w) 0) ; no warning
       (cond
        ((/= (_warning-err w) 0) (setf _t "error") (incerr))
        (t 
         (setf _t "warning")))
       (format *error-output* "~a:~a: ~a: " ftitle lineno _t)
       (apply #'format *error-output* (_warning-fmt w) ap)
       (format *error-output* "~%")))))

(defun talloc ()
  (incf usednodes)
  (cond
    (freelink
     (let ((p freelink))
       (setf freelink (node-n_left p))
       (unless (eq (node-n_op p) 'FREE)
	 (_cerror "node not FREE: ~a" p))
       (when ndebug
	 (format t "alloc node ~a from freelist~%" p))
       p))
    (t
     (let ((p (make-node :n_op 'FREE)))
       (when ndebug
	 (format t "alloc node ~a from memory~%" p))
       p))))

(defun tfree (p)
  (declare (type node p))
  (unless (eq (node-n_op p) 'FREE)
    (walkf p #'nfree 0)))

(defun mkdope ()
  (setf nerrors 0 warniserr 0)
  (dolist (q indope)
    (setf (gethash (dopest-dopeop q) dope) (dopest-dopeval q))
    (setf (gethash (dopest-dopeop q) opst) (dopest-opst q))))

(defun newstring (s)
  (copy-seq s))

(defun deunsign (x)
  "Make a type signed, if possible"
  (declare (type stype x))
  (make-stype
   :id (case (stype-id x)
	 ((UCHAR) 'CHAR)
	 ((USHORT) 'SHORT)
	 ((UNSIGNED) 'INT)
	 ((ULONG) 'LONG)
	 ((ULONGLONG) 'LONGLONG)
	 (t (stype-id x)))
   :mod (stype-mod x)))


;/*
;* Attribute functions.
;*/

(defun attr_new (type nelem)
  (declare (type symbol type)
	   (fixnum nelem))
  (make-attr :atype type
	     :aa (make-array (list nelem)
			     :initial-element 0)))

(defun attr_add (old new)
  "Add attribute list new before old and return new."
  (cond
    ((null new) old)
    (t (do ((ap new (attr-next ap)))
	   ((null (attr-next ap)) (setf (attr-next ap) old)))
       new)))
  
;; Search for attribute type in list ap.  Return entry or NULL.
(defun attr_find (ap type)
  (declare (type (or null attr) ap)
	   (type symbol type))
  (if (and ap (not (eq (attr-atype ap) type)))
      (attr_find (attr-next ap) type)
      ap))

(defun attr_dup (ap)
  "Duplicate an attribute, like strdup."
  (declare (type attr ap))
  (let ((ap1 (copy-structure ap)))
    (setf (attr-next ap1) nil)
    ap1))
  
