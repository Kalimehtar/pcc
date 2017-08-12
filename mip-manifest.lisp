(defpackage #:pcc.mip-manifest
  (:use #:cl #:pcc.mip-node #:pcc.macdef)
  (:export
   #:INCREF
   #:DECREF
   #:DECQAL
   #:BTYPE
   #:ISINTEGER
   #:SETOFF
   #:ISARY
   #:ISPTR
   #:ISFTN
   #:ISUNSIGNED
   #:lineno
   #:MODTYPE
   #:regno

   #:ATTR_MI

   #:stype
   #:stype-id
   #:stype-mod
   #:make-stype
   #:type-class
   #:stype>

   #:mflags
   #:Wflags

   #:make-interpass
   #:ip_node
   #:ip_name
   #:ip_asm
   #:interpass_prolog-ip_labels
   #:SLIST_FIRST
   #:SLIST_FOREACH
   #:SLIST_INSERT_FIRST
   #:SLIST_INSERT_LAST
   #:mk-slist2
   #:DLIST_INSERT_BEFORE
   #:DLIST_INIT
   #:interpass-qelem
   #:interpass-type

   #:make-interpass_prolog
   #:interpass_prolog-ip_lblnum
   #:UNDEF
   #:BOOL
   #:CHAR
   #:UCHAR
   #:SHORT
   #:USHORT
   #:INT
   #:UNSIGNED
   #:LONG
   #:ULONG
   #:LONGLONG
   #:ULONGLONG
   #:FLOAT
   #:DOUBLE
   #:LDOUBLE
   #:STRTY
   #:UNIONTY
   #:XTYPE
   #:VOID
   #:PTR
   #:FTN
   #:ARY
   #:CON
   #:VOL

   ;; mip-common
   #:newstring
   #:attr_new
   #:attr_add
   #:attr_find
   #:attr_dup
   #:uerror
   #:_cerror
   #:deunsign
   #:mkdope
   
   #:gflag
   #:kflag
   #:pflag
   #:sspflag
   #:xssa
   #:xtailcall
   #:xtemps
   #:xdeljumps
   #:xdce
   #:tfree
   #:walkf

   ;; from node
   #:iarg
   #:sarg
   #:varg
   #:amlist
   #:make-attr
   #:attr
   #:attr-next
   #:attr-atype
   #:attr-aa
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
   #:getlval
   #:setlval

   ;; from macdef
   #:MAXREGS
   #:SZPOINT
   #:SZBOOL
   #:SZCHAR
   #:SZSHORT
   #:SZINT
   #:SZLONG
   #:SZLONGLONG
   #:SZFLOAT
   #:SZDOUBLE
   #:SZLDOUBLE
   #:BOOL_TYPE
   #:szty
   #:TARGET_ENDIAN
   #:TARGET_LE
   #:TARGET_BE
   #:MAX_INT
   #:MAX_USHORT
   #:MAX_UCHAR
   
   #:ALCHAR
   #:ALBOOL
   #:ALSHORT
   #:ALINT
   #:ALLONG
   #:ALPOINT
   #:ALLONGLONG
   #:ALFLOAT
   #:ALDOUBLE
   #:ALLDOUBLE
   #:ALSTACK))
; * Type names, used in symbol table building.
; * The order of the integer types are important.
; * Signed types must have bit 0 unset, unsigned types set (used below).

(in-package #:pcc.mip-manifest)

(deftype type-class ()
  '(member UNDEF BOOL CHAR UCHAR SHORT USHORT INT UNSIGNED LONG
    ULONG LONGLONG ULONGLONG FLOAT DOUBLE LDOUBLE STRTY UNIONTY
    XTYPE ; Extended target-specific type
    VOID))

(defun regno (p) (node-n_rval p)) ; register number
(defun (setf regno) (val p) (setf (node-n_rval p) val))

#|

;/*
; * Various flags
; */
(defconstant NOLAB   -1)

;/*
; * Type modifiers.
; */
(defconstant PTR             #x20)
(defconstant FTN             #x40)
(defconstant ARY             #x60)
(defconstant CON             #x20)
(defconstant VOL             #x40)

;/*
; * Type packing constants
; */
(defconstant TMASK   #x060)
(defconstant TMASK1  #x180)
(defconstant TMASK2  #x1e0)
(defconstant BTMASK  #x1f)
(defconstant BTSHIFT 5)
(defconstant TSHIFT  2)

|#

;;; Monk: changed to lists of symbols

;; id -- base type
;; mod -- list of lists of modifiers
(defstruct stype
  (id nil :type symbol)
  (mod nil :type list))

(defun stype> (t1 t2)
  (and (null (stype-mod t2))
       (ecase (stype-id t1)
	 (UNDEF nil)
	 (BOOL (eq (stype-id t2) 'UNDEF))
	 (CHAR (member (stype-id t2) '(UNDEF BOOL)))
	 (UCHAR (member (stype-id t2) '(UNDEF BOOL CHAR)))
	 (SHORT (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR)))
	 (USHORT (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR SHORT)))
	 (INT (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR SHORT USHORT)))
	 (UNSIGNED (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR
					   SHORT USHORT INT)))
	 (LONG (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR
				       SHORT USHORT INT UNSIGNED)))
	 (ULONG (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR
					SHORT USHORT INT UNSIGNED LONG)))
	 (LONGLONG (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR
					   SHORT USHORT INT UNSIGNED
					   LONG ULONG)))
	 (ULONGLONG (member (stype-id t2) '(UNDEF BOOL CHAR UCHAR
					    SHORT USHORT INT UNSIGNED LONG
					    ULONG LONGLONG)))
	 (FLOAT (not (member (stype-id t2) '(FLOAT DOUBLE LDOUBLE STRTY
					     UNIONTY XTYPE VOID))))
	 (DOUBLE (not (member (stype-id t2) '(DOUBLE LDOUBLE STRTY UNIONTY
					      XTYPE VOID))))
	 (LDOUBLE (not (member (stype-id t2) '(LDOUBLE STRTY UNIONTY
					       XTYPE VOID))))
	 (STRTY (not (member (stype-id t2) '(STRTY UNIONTY XTYPE VOID))))
	 (UNIONTY (not (member (stype-id t2) '(UNIONTY XTYPE VOID))))
	 (XTYPE (not (member (stype-id t2) '(XTYPE VOID))))
	 (VOID (not (eq (stype-id t2) 'VOID))))))

(defun union-mod (l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) l1)
    (t (cons (union (car l1) (car l2)) (union-mod (cdr l1) (cdr l2))))))
;; set basic type of y to x
(defun MODTYPE (x y)
  (setf (stype-id x) (stype-id y)
	(stype-mod x) (union-mod (stype-mod x) (stype-mod y))))
(defun BTYPE (x) (stype-id x)) ; basic type of x
(defun ISLONGLONG (x)
  (and (null (stype-mod x))
       (member (stype-id x) '(LONGLONG ULONGLONG))))
(defun ISUNSIGNED (x)
  (and (null (stype-mod x))
       (member (stype-id x)
	       '(BOOL UCHAR USHORT UNSIGNED ULONG ULONGLONG))))
(defun UNSIGNABLE (x)
  (and (null (stype-mod x))
       (member (stype-id x)
	       '(CHAR SHORT INT LONG LONGLONG))))
(defun ISINTEGER (x)
  (and (null (stype-mod x))
       (member (stype-id x)
	       '(BOOL CHAR UCHAR SHORT USHORT INT UNSIGNED
		 LONG ULONG LONGLONG ULONGLONG))))
(defun ISPTR (x) (member 'PTR (car (stype-mod x))))

(defun ISFTN (x)
  (member 'FTN (car (stype-mod x)))) ; /* is x a function type? */
(defun ISARY (x)
  (member 'ARY (car (stype-mod x)))) ; /* is x an array type? */
(defun ISCON (x) (member 'CON (car (stype-mod x)))) ; /* is x const? */
(defun ISVOL (x) (member 'VOL (car (stype-mod x)))) ; /* is x volatile? */
(defun INCREF (x)
  (make-stype :id (stype-id x)
	      :mod (cons '(PTR) (stype-mod x))))
(defun INCQAL (x)
  (make-stype :id (stype-id x)
	      :mod (cons '() (stype-mod x))))
(defun DECREF (x)
  (make-stype :id (stype-id x)
	      :mod (cdr (stype-mod x))))
(defun DECQAL (x) (DECREF x))

(defmacro SETOFF (x y)
  `(when (/= (mod ,x ,y) 0)
     (setf ,x (* (1+ (truncate ,x ,y)) ,y))))

(defstruct slist forw)

(defmacro slist-head (n)
  `(defstruct (,n (:include slist))
     last))

(slist-head slist2)
(defun mk-slist2 ()
  (let ((n (vector nil)))
    (make-slist2 :forw n :last n)))

(defmacro SLIST_INIT (h &optional (last 'slist2-last))
  `(let ((n (vector nil)))
     (setf (slist-forw ,h) n
	   (,last ,h) n)))

(defun SLIST_FIRST (h) (aref (slist-forw h) 0))

(defun SLIST_ISEMPTY (h &optional (last 'slist2-last))
  (eq (slist-forw h) (funcall last h)))

(defmacro SLIST_FOREACH (v h f &body body)
  `(do ((,v (SLIST_FIRST ,h) (SLIST_FIRST (,f ,v)))) ((null ,v) nil)
    ,@body))

(defmacro SLIST_INSERT_FIRST (h e f &optional (last 'slist2-last))
  `(progn
     (when (eq (,last ,h) (slist-forw ,h))
       (setf (,last ,h) (slist-forw (,f ,e))))
     (setf (aref (slist-forw (,f ,e)) 0) (SLIST_FIRST ,h)
	   (aref (slist-forw ,h) 0) ,e)))

(defmacro SLIST_INSERT_LAST (h e f &optional (last 'slist2-last))
  `(setf (aref (slist-forw (,f ,e)) 0) nil
	 (aref (,last ,h) 0) ,e
	 (,last ,h) (slist-forw (,f ,e))))

(defstruct dlist forw back)

(defun DLIST_INIT (h f)
  (setf (dlist-forw (funcall f h)) h
	(dlist-back (funcall f h)) h))

(defun DLIST_INSERT_BEFORE (h e f)
  (setf (dlist-forw (funcall f e)) h
	(dlist-back (funcall f e)) (dlist-back (funcall f h))
	(dlist-forw (funcall f (dlist-back (funcall f e)))) e
	(dlist-back (funcall f h)) e))

(defstruct interpass
  (qelem (make-dlist) :type dlist)
  type lineno _un)
; node locc lbl name asm off)

(defmacro mkdef-interpass ()
  `(defstruct (interpass_prolog (:include interpass))
     ipp_name    ;         /* Function name */
     ipp_vis;            /* Function visibility */
     ipp_type;         /* Function type */
     ipp_autos;          /* Size on stack needed */
     ip_tmpnum;          /* # allocated temp nodes so far */
     ip_lblnum;          /* # used labels so far */
     ip_labels;         /* labels used in computed goto */  
     ,@TARGET_IPP_MEMBERS))

(mkdef-interpass)

;; Epilog/prolog takes following arguments (in order):
;; - type
;; - regs
;; - autos
;; - name
;; - type
;; - retlab


(defun ip_node (x) (interpass-_un x))
(defun (setf ip_node) (val x) (setf (interpass-_un x) val))
(defun ip_locc (x) (interpass-_un x))
(defun (setf ip_locc) (val x) (setf (interpass-_un x) val))
(defun ip_lbl (x) (interpass-_un x))
(defun (setf ip_lbl) (val x) (setf (interpass-_un x) val))
(defun ip_name (x) (interpass-_un x))
(defun (setf ip_name) (val x) (setf (interpass-_un x) val))
(defun ip_asm (x) (interpass-_un x))
(defun (setf ip_asm) (val x) (setf (interpass-_un x) val))
(defun ip_off (x) (interpass-_un x))
(defun (setf ip_off) (val x) (setf (interpass-_un x) val))

(defvar ATTR_MI (append '(:ATTR_NONE :GCC_ATYP_STDCALL :GCC_ATYP_CDECL)
			ATTR_MI_TARGET))
