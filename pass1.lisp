(defpackage #:pcc.pass1
  (:use #:cl #:pcc.mip-manifest)
  (:export
   
   #:symtab
   #:make-symtab
   #:symtab-snext
   #:symtab-sname
   #:symtab-sap
   #:symtab-sclass
   #:symtab-stype
   #:symtab-slevel
   
   #:p1nd
   #:make-p1nd
   #:p1nd-n_op
   #:p1nd-n_type
   #:p1nd-n_qual
   #:p1nd-n_ap
   #:p1nd-n_name
   #:p1nd-n_df
   #:p1nd-n_val
   #:p1nd-n_left
   #:p1nd-n_right
   #:p1nd-n_rval
   #:p1nd-n_sp
   #:p1nd-n_dcon
   #:p1nd-n_ccon
   #:getlval
   #:setlval

   ;; init
   #:astypnames
   #:tab   ;; my own
   
   ;; scan
   #:pragma_allpacked
   
   ;; pftn
   #:getsymtab
   #:bstruct   
   #:blevel
   #:reached

   ;; symtabs
   #:lookup
   #:addname
   
   ;; gcc_compat
   #:gcc_attr_parse
   
   ;; from mip-manifest
   #:INCREF
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

   ;; mip-common
   #:newstring
   #:attr_new
   #:attr_add
   #:attr_find
   #:uerror
   
   ;; from mip-node
   #:iarg
   #:sarg
   #:varg
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
   ))

(in-package #:pcc.pass1)

(deftype storage-class ()
  '(member SNULL AUTO EXTERN STATIC REGISTER EXTDEF THLOCAL
    KEYWORD MOS PARAM STNAME MOU UNAME TYPEDEF ENAME MOE USTATIC))

(deftype flag-class ()
  '(member SNORMAL STAGNAME SLBLNAME SMOSNAME SSTRING NSTYPES SMASK))

(deftype flag-mod ()
  '(member
    STLS ; Thread Local Support variable
    SINSYS ; Declared in system header
    SSTMT ; Allocate symtab on statement stack
    SNOCREAT ; don't create a symbol in lookup()
    STEMP ;  Allocate symtab from temp or perm mem
    SDYNARRAY ; symbol is dynamic array on stack
    SINLINE ; function is of type inline
    SBLK ; Allocate symtab from blk mem
    STNODE ; symbol shall be a temporary node
    SBUILTIN ; this is a builtin function
    SASG ; symbol is assigned to already
    SLOCAL1 SLOCAL2 SLOCAL3))
    
; * Dimension/prototype information.
; *      ddim > 0 holds the dimension of an array.
; *      ddim < 0 is a dynamic array and refers to a tempnode.
; *      ...unless:
; *              ddim == NOOFFSET, an array without dimenston, "[]"
; *              ddim == -1, dynamic array while building before defid.
(defstruct dimfun ddim dfun)

;/*
; * Argument list member info when storing prototypes.
; */
(defstruct arglist type df sap)

(defvar TNULL
  (INCREF (make-stype :id 'FARG))) ; pointer to FARG -- impossible type
(defvar TELLIPSIS (INCREF (INCREF (make-stype :id 'FARG))))

;/*
;* Symbol table definition.
;*/

(defstruct symtab
  (snext nil :type (or null symtab)) ; link to other symbols in the same scope
  (soffset 0 :type fixnum)           ; offset or value
  (sclass 'SNULL :type symbol)            ; storage class
  (slevel 0 :type fixnum)           ; scope level
  (sflags (make-stype) :type stype)  ; flags, see below
  (sname "" :type string) ; Symbol name
  (stype 'UNDEF :type symbol) ; type
  (squal 0) ; qualifier
  sdf  ; ptr to the dimension/prototype array
  (sap nil :type list)) ; the base type attribute list

(defun ISSOU (ty) (member (stype-id ty) '(STRTY UNIONTY)))

(defstruct n_u n_l n_r)
(defstruct n_f (n_u (make-n_u) :type n_u) _dcon _ccon)
(defstruct p1nd 
  (n_op 'UNDEF :type symbol)
  n_type
  n_qual
  n_5
  n_ap
  (n_f (make-n_f) :type n_f))

(defun p1nd-n_name (n)
  (p1nd-n_5 n))

(defun (setf p1nd-n_name) (val n)
  (setf (p1nd-n_5 n) val))

(defun p1nd-n_df (n)
  (p1nd-n_5 n))

(defun (setf p1nd-n_df) (val n)
  (setf (p1nd-n_5 n) val))

(defun p1nd-n_left (n)
  (n_u-n_l (n_f-n_u (p1nd-n_f n))))

(defun (setf p1nd-n_left) (val n)
  (setf (n_u-n_l (n_f-n_u (p1nd-n_f n))) val))

(defun p1nd-n_val (n)
  (n_u-n_l (n_f-n_u (p1nd-n_f n))))

(defun (setf p1nd-n_val) (val n)
  (setf (n_u-n_l (n_f-n_u (p1nd-n_f n))) val))

(defun p1nd-n_right (n)
  (n_u-n_r (n_f-n_u (p1nd-n_f n))))

(defun (setf p1nd-n_right) (val n)
  (setf (n_u-n_r (n_f-n_u (p1nd-n_f n))) val))

(defun p1nd-n_rval (n)
  (n_u-n_r (n_f-n_u (p1nd-n_f n))))

(defun (setf p1nd-n_rval) (val n)
  (setf (n_u-n_r (n_f-n_u (p1nd-n_f n))) val))

(defun p1nd-n_sp (n)
  (n_u-n_r (n_f-n_u (p1nd-n_f n))))

(defun (setf p1nd-n_sp) (val n)
  (setf (n_u-n_r (n_f-n_u (p1nd-n_f n))) val))

(defun p1nd-n_dcon (n)
  (n_f-_dcon (p1nd-n_f n)))

(defun (setf p1nd-n_dcon) (val n)
  (setf (n_f-_dcon (p1nd-n_f n)) val))

(defun p1nd-n_ccon (n)
  (n_f-_ccon (p1nd-n_f n)))

(defun (setf p1nd-n_ccon) (val n)
  (setf (n_f-_ccon (p1nd-n_f n)) val))

(defun glval (p) (p1nd-n_val p))
(defmacro slval (p v) `(setf (p1nd-n_val ,p) ,v))
 
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

