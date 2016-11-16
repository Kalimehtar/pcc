
; * Type names, used in symbol table building.
; * The order of the integer types are important.
; * Signed types must have bit 0 unset, unsigned types set (used below).

(in-package #:pcc)

(defconstant UNDEF           0) ;       /* free symbol table entry */
(defconstant BOOL            1) ;       /* function argument */
(defconstant _CHAR            2)
(defconstant UCHAR           3)
(defconstant SHORT           4)
(defconstant USHORT          5)
(defconstant INT             6)
(defconstant UNSIGNED        7)
(defconstant LONG            8)
(defconstant ULONG           9)
(defconstant LONGLONG        10)
(defconstant ULONGLONG       11)
(defconstant _FLOAT           12)
(defconstant DOUBLE          13)
(defconstant LDOUBLE         14)
(defconstant STRTY           15)
(defconstant UNIONTY         16)
(defconstant XTYPE           17) ;     /* Extended target-specific type */
; /* #define      MOETY           18 */   /* member of enum */
(defconstant VOID            19)

(defconstant MAXTYPES        19) ;      /* highest type+1 to be used by lang code */

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

(defmacro MODTYPE (x y) `(setf ,x (logior (logandc2 ,x BTMASK) ,y)))
(defun BTYPE (x) (logand x BTMASK))
(defun ISLONGLONG (x) (or (= x LONGLONG) (= x ULONGLONG)))
(defun ISUNSIGNED (x) (and (<= x ULONGLONG) (= (logand x 1) 1)))
(defun UNSIGNABLE (x) (and (<= _CHAR x ULONGLONG) (not (ISUNSIGNED x))))
(defun ISINTEGER (x) (<= BOOL x ULONGLONG))
(defun ISPTR (x) (= (logand x TMASK) PTR))
(defun ISFTN (x) (= (logand x TMASK) FTN)) ; /* is x a function type? */
(defun ISARY (x) (= (logand x TMASK) ARY)) ; /* is x an array type? */
(defun ISCON (x) (= (logand x CON) CON)) ; /* is x const? */
(defun ISVOL (x) (= (logand x VOL) VOL)) ; /* is x volatile? */
(defun INCREF (x) (logior (ash (logandc2 x BTMASK) TSHIFT) PTR (logand x BTMASK)))
(defun INCQAL (x) (logior (ash (logandc2 x BTMASK) TSHIFT) (logand x BTMASK)))
(defun DECREF (x) (logior (logandc2 (ash x (- TSHIFT)) BTMASK) (logand x BTMASK)))
(defun DECQAL (x) (logior (logandc2 (ash x (- TSHIFT)) BTMASK) (logand x BTMASK)))
