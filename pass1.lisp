(in-package #:pcc)

(defconstant STLS            #o00010) ;   /* Thread Local Support variable */
(defconstant SINSYS          #o00020) ;   /* Declared in system header */
(defconstant SSTMT           SINSYS) ;  /* Allocate symtab on statement stack */
(defconstant SNOCREAT        #o00040) ;   /* don't create a symbol in lookup() */
(defconstant STEMP           #o00100) ;   /* Allocate symtab from temp or perm mem */
(defconstant SDYNARRAY       #o00200) ;   /* symbol is dynamic array on stack */
(defconstant SINLINE         #o00400) ;   /* function is of type inline */
(defconstant SBLK            SINLINE) ; /* Allocate symtab from blk mem */
(defconstant STNODE          #o01000) ;   /* symbol shall be a temporary node */
(defconstant SBUILTIN        #o02000) ;   /* this is a builtin function */
(defconstant SASG            #o04000) ;   /* symbol is assigned to already */
(defconstant SLOCAL1         #o010000) 
(defconstant SLOCAL2         #o020000) 
(defconstant SLOCAL3         #o040000) 

(defconstant NOOFFSET        -10201)

(defconstant SIGNED          (+ MAXTYPES 1))
(defconstant FARG            (+ MAXTYPES 2))
(defconstant FIMAG           (+ MAXTYPES 3))
(defconstant IMAG            (+ MAXTYPES 4))
(defconstant LIMAG           (+ MAXTYPES 5))
(defconstant FCOMPLEX        (+ MAXTYPES 6))
(defconstant _COMPLEX         (+ MAXTYPES 7))
(defconstant LCOMPLEX        (+ MAXTYPES 8))
(defconstant ENUMTY          (+ MAXTYPES 9))


;/*
; * Dimension/prototype information.
; *      ddim > 0 holds the dimension of an array.
; *      ddim < 0 is a dynamic array and refers to a tempnode.
; *      ...unless:
; *              ddim == NOOFFSET, an array without dimenston, "[]"
; *              ddim == -1, dynamic array while building before defid.
; */
(defstruct dimfun ddim dfun)


;/*
; * Argument list member info when storing prototypes.
; */
(defstruct arglist type df sap)

(defvar TNULL (INCREF FARG)) ; pointer to FARG -- impossible type
(defvar TELLIPSIS (INCREF (INCREF FARG)))

;/*
;* Symbol table definition.
;*/

(defstruct symtab next soffset sclass slevel sflags sname stype squal sdf sap)

(defun ISSOU (ty) (or (= ty STRTY) (= ty UNIONTY)))


(defstruct node
  op type qual name df label
  ap left val slval right rval sp dcon)

(defmacro slval (p v) (setf (node-val p) v))
 
;typedef struct p1node {
;        int     n_op;
;        TWORD   n_type;
;        TWORD   n_qual;
;        union {
;                char *  _name;
;                union   dimfun *_df;
;        } n_5;
;        struct attr *n_ap;
;        union {
;                struct {
;                        union {
;                                struct p1node *_left;
;                                CONSZ _val;
;                        } n_l;
;                        union {
;                                struct p1node *_right;
;                                int _rval;
;                                struct symtab *_sp;
;                        } n_r;
;                } n_u;
;                struct {
;                        struct flt *_dcon;
;                        struct flt *_ccon;
;                };
;        } n_f;
;} P1ND;

