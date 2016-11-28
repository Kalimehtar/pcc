(defpackage #:pcc.mip-manifest
  (:use #:cl #:pcc.mip-node)
  (:export
   #:stype
   #:stype-id
   #:stype-mod
   #:make-stype
   #:type-class
   #:gflag
   #:kflag
   #:pflag
   #:sspflag
   #:xssa
   #:xtailcall
   #:xtemps
   #:xdeljumps
   #:xdce

   ;; from node
   #:make-attr
   #:attr-next
   #:attr-atype
   #:attr-aa
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
   ))
; * Type names, used in symbol table building.
; * The order of the integer types are important.
; * Signed types must have bit 0 unset, unsigned types set (used below).

(in-package #:pcc.manifest)

(deftype type-class ()
  '(member UNDEF BOOL CHAR UCHAR SHORT USHORT INT UNSIGNED LONG
    ULONG LONGLONG ULONGLONG FLOAT DOUBLE  STRTY  UNIONTY
    XTYPE ; Extended target-specific type
    VOID))

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

(defstruct interpass
  type lineno node locc lbl name asm off)

