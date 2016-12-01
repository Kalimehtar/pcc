(defpackage #:pcc.gcc-compat (:use #:cl))

(in-package #:pcc.gcc-compat)

(defstruct (kw (:constructor make-kw (name ptr rv)))
  name ptr rv)

;; Do NOT change the order of these entries unless you know
;; what you're doing!
(defvar kw
  (vector
   (make-kw "__asm" nil 'C_ASM)   ; 0
   (make-kw "__signed" nil 0)    ; 1
   (make-kw "__inline" nil 0)    ; 2
   (make-kw "__const" nil 0)    ; 3
   (make-kw "__asm__" nil 'C_ASM)    ; 4
   (make-kw "__inline__" nil 0)    ; 5
   (make-kw "__thread" nil 0)    ; 6
   (make-kw "__FUNCTION__" nil 0)    ; 7
   (make-kw "__volatile" nil 0)    ; 8
   (make-kw "__volatile__" nil 0)    ; 9
   (make-kw "__restrict" nil -1)    ; 10
   (make-kw "__typeof__" nil 'C_TYPEOF)    ; 11
   (make-kw "typeof" nil 'C_TYPEOF)    ; 12
   (make-kw "__extension__" nil -1)    ; 13
   (make-kw "__signed__" nil 0)    ; 14
   (make-kw "__attribute__" nil 0)    ; 15
   (make-kw "__attribute" nil 0)    ; 16
   (make-kw "__real__" nil 0)    ; 17
   (make-kw "__imag__" nil 0)    ; 18
   (make-kw "__builtin_offsetof" nil 'PCC_OFFSETOF)    ; 19
   (make-kw "__PRETTY_FUNCTION__" nil 0)    ; 20
   (make-kw "__alignof__" nil 'C_ALIGNOF)    ; 21
   (make-kw "__typeof" nil 'C_TYPEOF)    ; 22
   (make-kw "__alignof" nil 'C_ALIGNOF)    ; 23
   (make-kw "__restrict__" nil -1)))    ; 24

;  /* allowed number of args */
(defconstant A_0ARG #x01)
(defconstant A_1ARG  #x02)
(defconstant A_2ARG  #x04)
(defconstant A_3ARG  #x08)
;  /* arg # is a name */
(defconstant A1_NAME #x10)
(defconstant A2_NAME #x20)
(defconstant A3_NAME #x40)
(defconstant A_MANY  #x80)
;  /* arg # is "string" */
(defconstant A1_STR  #x100)
(defconstant A2_STR  #x200)
(defconstant A3_STR  #x400)
  

(defstruct (atax (:constructor make-atax (typ name))) typ name)
(defvar atax
  (list
   (cons 'ATTR_NONE (make-atax 0 nil))
   (cons 'ATTR_COMPLEX (make-atax 0 nil))
   (cons 'xxxATTR_BASETYP (make-atax 0 nil))
   (cons 'ATTR_QUALTYP (make-atax 0 nil))
   (cons 'ATTR_STRUCT (make-atax 0 nil))
   (cons 'ATTR_ALIGNED (make-atax (logior A_0ARG A_1ARG) "aligned"))
   (cons 'ATTR_NORETURN (make-atax A_0ARG "noreturn"))
   (cons 'ATTR_P1LABELS (make-atax A_0ARG "p1labels"))
   (cons 'ATTR_SONAME (make-atax (logior A_1ARG A1_STR) "soname"))
   (cons 'GCC_ATYP_PACKED (make-atax (logior A_0ARG A_1ARG) "packed"))
   (cons 'GCC_ATYP_SECTION (make-atax (logior A_1ARG A1_STR) "section"))
   (cons 'GCC_ATYP_TRANSP_UNION (make-atax A_0ARG "transparent_union"))
   (cons 'GCC_ATYP_UNUSED (make-atax A_0ARG "unused"))
   (cons 'GCC_ATYP_DEPRECATED (make-atax A_0ARG "deprecated"))
   (cons 'GCC_ATYP_MAYALIAS (make-atax A_0ARG "may_alias"))
   (cons 'GCC_ATYP_MODE (make-atax (logior A_1ARG A1_NAME) "mode"))   
   (cons 'GCC_ATYP_FORMAT (make-atax (logior A_3ARG A1_NAME) "format"))
   (cons 'GCC_ATYP_NONNULL (make-atax A_MANY "nonnull"))
   (cons 'GCC_ATYP_SENTINEL (make-atax (logior A_0ARG A_1ARG) "sentinel"))
   (cons 'GCC_ATYP_WEAK (make-atax A_0ARG "weak"))
   (cons 'GCC_ATYP_FORMATARG (make-atax A_1ARG "format_arg"))
   (cons 'GCC_ATYP_GNU_INLINE (make-atax A_0ARG "gnu_inline"))
   (cons 'GCC_ATYP_MALLOC (make-atax A_0ARG "malloc"))
   (cons 'GCC_ATYP_NOTHROW (make-atax A_0ARG "nothrow"))
   (cons 'GCC_ATYP_CONST (make-atax A_0ARG "const"))
   (cons 'GCC_ATYP_PURE (make-atax A_0ARG "pure"))
   (cons 'GCC_ATYP_CONSTRUCTOR (make-atax A_0ARG "constructor"))
   (cons 'GCC_ATYP_DESTRUCTOR (make-atax A_0ARG "destructor"))
   (cons 'GCC_ATYP_VISIBILITY (make-atax (logior A_1ARG A1_STR) "visibility"))
   (cons 'GCC_ATYP_STDCALL (make-atax A_0ARG "stdcall"))
   (cons 'GCC_ATYP_CDECL (make-atax A_0ARG "cdecl"))
   (cons 'GCC_ATYP_WARN_UNUSED_RESULT (make-atax A_0ARG "warn_unused_result"))
   (cons 'GCC_ATYP_USED (make-atax A_0ARG "used"))
   (cons 'GCC_ATYP_NO_INSTR_FUN (make-atax A_0ARG "no_instrument_function"))
   (cons 'GCC_ATYP_NOINLINE (make-atax A_0ARG "noinline"))
   (cons 'GCC_ATYP_ALIAS (make-atax (logior A_1ARG A1_STR) "alias"))
   (cons 'GCC_ATYP_WEAKREF (make-atax (logior A_0ARG A_1ARG A1_STR) "weakref"))
   (cons 'GCC_ATYP_ALLOCSZ (make-atax (logior A_1ARG A_2ARG) "alloc_size"))
   (cons 'GCC_ATYP_ALW_INL (make-atax A_0ARG "always_inline"))
   (cons 'GCC_ATYP_TLSMODEL (make-atax (logior A_1ARG A1_STR) "tls_model"))
   (cons 'GCC_ATYP_ALIASWEAK (make-atax (logior A_1ARG A1_STR) "aliasweak"))
   (cons 'GCC_ATYP_RETURNS_TWICE (make-atax A_0ARG "returns_twice"))
   (cons 'GCC_ATYP_WARNING (make-atax (logior A_1ARG A1_STR) "warning"))
   (cons 'GCC_ATYP_NOCLONE (make-atax A_0ARG "noclone"))
   (cons 'GCC_ATYP_REGPARM (make-atax A_1ARG "regparm"))
   (cons 'GCC_ATYP_FASTCALL (make-atax A_0ARG "fastcall"))
   (cons 'GCC_ATYP_BOUNDED (make-atax (logior A_3ARG A_MANY A1_NAME) "bounded"))
   (cons 'GCC_ATYP_WEAKIMPORT (make-atax A_0ARG "weak_import"))))

(defun amatch (s at)
  (when (string= "__" (subseq s 0 2))
    (setf s (subseq s 2)))
  (let ((len (length s)))
    (when (string= "__" (subseq s (- len 2) len))
      (setf s (subseq s (- len 2)))))
  (dolist (i at)
    (let ((tmp (atax-name (cdr i))))
      (when (and tmp (string= tmp s))
	(return i))))
  nil)

(defun setaarg (str aa p)
  (cond
    ((/= str 0)
     (when (or (and (logand str (logior A1_STR A2_STR A3_STR))
		    (not (eq (node-op p) 'STRING)))
	       (and (logand str (logior A1_NAME A2_NAME A3_NAME))
		    (not (eq (node-op p) 'NAME))))
       (uerror "bad arg to attribute"))
     (if (eq (node-op p) 'STRING)
	 (setf (aarg-sarg aa) (node-name p)) ; saved in cgram.y
	 (setf (aarg-sarg aa) (node-sp p))))
    (t (setf (aarg-iarg aa) (icons (eve p))))))

(defun gcc_attribs (p)
  (let (name attr)
    (cond
      ((eq (node-op p) 'NAME) (setf name (node-sp p)))
      ((or (eq (node-op p) 'CALL) (eq (node-op p) 'UCALL))
       (setf name (node-sp (node-left p))))
      ((and (eq (node-op p) 'ICON) (= (node-type p) STRTY))
       (return nil))
      (t (_cerror "bad variable attribute")))
    (setf attr (amatch name atax))
    (unless attr
      (warner 'Wattributes name)
      (return nil))
    (assert (and (consp attr) (symbolp (car attr)) (ataxp (cdr attr))))
    (let ((narg 0))
      (when (eq (node-op p) 'CALL)
	(setf narg 1)
	(do ((q (node-right p))) ((not (eq (node-op q) 'CM)) nil)
	  (setf q (node-left q))
	  (incf narg)))
      (let ((cw (atax-typ (cdr attr))))
	(when (and (= (logand cw A_MANY) 0)
		   (or (> narg 3)
		       (= (logand cw (ash 1 narg)) 0)))
	  (uerror "wrong attribute arg count")
	  (return nil)))
      (let ((ap (attr_new (car attr) 3)) ; XXX should be narg
	    (q (node-right p)))
	(dotimes (i (- narg 3))
	  (let ((r q))
	    (setf q (node-left q))
	    (decf narg)
	    (tfree (node-right r))
	    (nfree r)))
	(when (= narg 3)
	  (let ((r q))
	    (setaarg (logang cw (logior A3_NAME A3_STR))
		     (aref (attr-aa ap) 2)
		     (node-right q))
	    (setf q (node-left q))
	    (nfree r)))	
	(when (>= narg 2)
	  (let ((r q))
	    (setaarg (logang cw (logior A2_NAME A2_STR))
		     (aref (attr-aa ap) 1)
		     (node-right q))
	    (setf q (node-left q))
	    (nfree r)))
	(when (>= narg 1)
	  (setaarg (logang cw (logior A1_NAME A1_STR))
		   (aref (attr-aa ap) 0)
		   q)
	  (setf (node-op p) 'UCALL))
	(case (car attr)
	  ((ATTR_ALIGNED)
	   (if (= narg 0)
	       (setf (aarg-iarg (aref (attr-aa ap) 0)) ALMAX)
	       (setf (aarg-iarg (aref (attr-aa ap) 0))
		     (* (aarg-iarg (aref (attr-aa ap) 0)) SZCHAR))))
	  ((GCC_ATYP_PACKED)
	   (if (= narg 0)
	       (setf (aarg-iarg (aref (attr-aa ap) 0)) 1) ; /* bitwise align */
	       (setf (aarg-iarg (aref (attr-aa ap) 0))
		     (* (aarg-iarg (aref (attr-aa ap) 0)) SZCHAR))))
	  ((GCC_ATYP_VISIBILITY)
	   (let ((c (aarg-sarg (aref (attr-aa ap) 0))))
	     (when (and (string/= c "default")
			(string/= c "hidden")
			(string/= c "internal")
			(string/= c "protected"))
	       (werror "unknown visibility ~a" c))))
	  ((GCC_ATYP_TLSMODEL)
	   (let ((c (aarg-sarg (aref (attr-aa ap) 0))))
	     (when (and (string/= c "global-dynamic")
			(string/= c "local-dynamic")
			(string/= c "initial-exec")
			(string/= c "local-exec"))
	       (werror "unknown tls model ~a" c)))))
	ap))))

(defun gcc_attr_parse (p)
  (cond
    ((null p) p)
    ((eq (node-op p) 'CM) (gcc_attribs p))
    (t (let ((b (gcc_attr_parse (node-left p)))
	     (c (gcc_attr_parse (node-right p))))
	 (if b (attr_add b c) c)))))


(defun addstr (n)
  (let ((p (_block 'NAME nil nil _FLOAT 0 0))
	(sp (make-symtab)))
    (setf (node-type p) (ctype ULONGLONG))))
    
;(defun gcc-init ()
  
