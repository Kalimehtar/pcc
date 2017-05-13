(defpackage #:pcc.pass1
  (:use #:cl #:pcc.mip-manifest #:pcc.external)
  (:export

   #:NAME #:ICON #:FCON #:CALL #:UCALL #:UMUL
   #:<=ATTR_MAX
   
   #:symtab
   #:make-symtab
   #:symtab-snext
   #:symtab-sname
   #:symtab-sap
   #:symtab-sclass
   #:symtab-stype
   #:symtab-slevel
   #:symtab-sdf
   #:symtab-sflags
   #:symtab-squal
   #:symtab-soffset

   #:class-fieldp
   #:class-fldsiz
   
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
   #:slval
   #:glval

   #:FIMAG->FLOAT
   #:FLOAT->FIMAG
   #:ISITY
   #:ISCTY
   #:ISSOU
   #:amsize
   #:strattr
   #:coptype
   #:clogop
   #:casgop
   #:UNASG

   #:FLOAT_PLUS
   #:FLOAT_MINUS
   #:FLOAT_MUL
   #:FLOAT_DIV
   #:FLOAT_ISZERO
   #:FLOAT_NEG
   #:FLOAT_ZERO
   #:FLOAT_EQ
   #:FLOAT_NE
   #:FLOAT_GT
   #:FLOAT_GE
   #:FLOAT_LT
   #:FLOAT_LE
   

   ;; inline
   #:inline_ref
   #:isinlining
   
   ;; cgram
   #:mkty
   
   ;; stabs
   #:stabs_struct
   
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
   #:soumemb
   #:falloc
   #:dclstruct
   #:cftnsp
   #:tsize
   #:argoff
   #:autooff
   #:oalloc
   #:defid
   #:defid2
   #:strmemb

   ;; symtabs
   #:lookup
   #:addname
   #:hide
   
   ;; gcc_compat
   #:gcc_attr_parse

   ;; trees
   #:_block
   #:p1nfree
   #:bcon
   #:stref
   #:cdope
   #:plabel
   #:cqual
   #:tempnode
   #:nametree
   #:p1tcopy
   #:ccast

   ;; code
   #:fldty

   ;; from external
   #:NPERMREG
   
   ;; from mip-manifest
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
   #:stype
   #:stype-id
   #:stype-mod
   #:make-stype
   #:type-class
   #:stype>
   #:gflag
   #:kflag
   #:pflag
   #:sspflag
   #:xssa
   #:xtailcall
   #:xtemps
   #:xdeljumps
   #:xdce
   #:mflags
   #:MODTYPE
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
   #:regno
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

   #:SNULL
   #:AUTO
   #:EXTERN
   #:STATIC
   #:REGISTER
   #:EXTDEF
   #:THLOCAL
   #:KEYWORD
   #:MOS
   #:PARAM
   #:STNAME
   #:MOU
   #:UNAME
   #:TYPEDEF
   #:ENAME
   #:MOE
   #:USTATIC

   #:SNORMAL
   #:STAGNAME
   #:SLBLNAME
   #:SMOSNAME
   #:SSTRING
   #:NSTYPES
   #:SMASK
   
   #:STLS
   #:SINSYS ; Declared in system header
   #:SSTMT ; Allocate symtab on statement stack
   #:SNOCREAT ; don't create a symbol in lookup()
   #:STEMP ;  Allocate symtab from temp or perm mem
   #:SDYNARRAY ; symbol is dynamic array on stack
   #:SINLINE ; function is of type inline
   #:SBLK ; Allocate symtab from blk mem
   #:STNODE ; symbol shall be a temporary node
   #:SBUILTIN ; this is a builtin function
   #:SASG ; symbol is assigned to already
   #:SLOCAL1
   #:SLOCAL2
   #:SLOCAL3

   ;; mip-common
   #:newstring
   #:attr_new
   #:attr_add
   #:attr_find
   #:attr_dup
   #:uerror
   #:_cerror
   #:deunsign
   #:Wflags
   #:mkdope
   #:tfree
   
   ;; from mip-node
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

   ;; from local
   #:clocal
   #:cisreg

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
;;(defstruct dimfun ddim dfun)
;;   dimfun is a union, so in lisp it is simply (or integer arglist)

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
  (soffset 0 :type (or symbol fixnum))           ; offset or value
  ;; Monk :: sclass & FIELD | n <=> (FIELD . n)
  (sclass 'SNULL :type (or symbol cons))            ; storage class
  (slevel 0 :type fixnum)           ; scope level
  (sflags (make-stype) :type stype)  ; flags, see below
  (sname "" :type string) ; Symbol name
  (stype (make-stype :id 'UNDEF) :type stype) ; type
  (squal 0) ; qualifier
  (sdf nil :type (or integer symbol vector))  ; ptr to the dimension/prototype array
  (sap nil :type (or null attr))) ; the base type attribute list

(defun class-fieldp (x) (and (consp x) (eq (car x) 'FIELD)))
(defun class-fldsiz (x) (cdr x))

(defun ISSOU (ty) (member (stype-id ty) '(STRTY UNIONTY)))

(defstruct n_u
  (n_l nil :type (or null integer p1nd))
  (n_r nil :type (or null integer p1nd)))
(defstruct n_f (n_u (make-n_u) :type n_u) _dcon _ccon)
(defstruct p1nd 
  (n_op 'UNDEF :type symbol)
  (n_type nil :type (or null stype))
  n_qual
  n_5
  (n_ap nil :type (or null attr)) 
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
 
(defun ISCTY (x)
  (member x '(FCOMPLEX COMPLEX LCOMPLEX)))

(defun ISITY (x)
  (member x '(FIMAG IMAG LIMAG)))

(defun coptype (o)
  (declare (type symbol o))
  (car (intersection (cdope o) '(BITYPE LTYPE UTYPE))))

(defun clogop (o)
  (car (intersection (cdope o) '(LOGFLG))))

(defun casgop (o)
  (car (intersection (cdope o) '(ASGFLG))))

(defun FIMAG->FLOAT (x)
  (case x
    ((FIMAG) 'FLOAT)
    ((IMAG) 'DOUBLE)
    ((LIMAG) 'LDOUBLE)))

(defun FLOAT->FIMAG (x)
  (case x
    ((FLOAT) 'FIMAG)
    ((DOUBLE) 'IMAG)
    ((LDOUBLE) 'LIMAG)))

(defstruct flt fp sf _t)
(defvar FLOAT_ZERO (make-flt :fp 0.0))

#+NATIVE_FLOATING_POINT
(progn
  (defun FLOAT_PLUS (p1 p2)
    (incf (flt-fp (p1nd-n_dcon p1)) (flt-fp (p1nd-n_dcon p2))))
  (defun FLOAT_MINUS (p1 p2)
    (incf (flt-fp (p1nd-n_dcon p1)) (- (flt-fp (p1nd-n_dcon p2)))))
  (defun FLOAT_MUL (p1 p2)
    (setf (flt-fp (p1nd-n_dcon p1))
	  (* (flt-fp (p1nd-n_dcon p1))
	     (flt-fp (p1nd-n_dcon p2)))))
  (defun FLOAT_DIV (p1 p2)
    (setf (flt-fp (p1nd-n_dcon p1))
	  (/ (flt-fp (p1nd-n_dcon p1))
	     (flt-fp (p1nd-n_dcon p2)))))
  (defun FLOAT_ISZERO (p) (= (flt-fp p) 0.0))
  (defun FLOAT_EQ (d1 d2) (= (flt-fp d1) (flt-fp d2)))
  (defun FLOAT_NE (d1 d2) (/= (flt-fp d1) (flt-fp d2)))
  (defun FLOAT_GE (d1 d2) (>= (flt-fp d1) (flt-fp d2)))
  (defun FLOAT_GT (d1 d2) (> (flt-fp d1) (flt-fp d2)))
  (defun FLOAT_LE (d1 d2) (<= (flt-fp d1) (flt-fp d2)))
  (defun FLOAT_LT (d1 d2) (< (flt-fp d1) (flt-fp d2)))
  (defun FLOAT_NEG (p) (setf (flt-fp p) (- (flt-fp p)))))
#-NATIVE_FLOATING_POINT
(progn
  (defun FLOAT_PLUS (p1 p2)
    (soft_plus (flt-sf (p1nd-n_dcon p1)) (flt-sf (p1nd-n_dcon p2))))
  (defun FLOAT_MINUS (p1 p2)
    (soft_minsu (flt-sf (p1nd-n_dcon p1)) (flt-sf (p1nd-n_dcon p2))))
  (defun FLOAT_MUL (p1 p2)
    (soft_mul (flt-sf (p1nd-n_dcon p1)) (flt-sf (p1nd-n_dcon p2))))
  (defun FLOAT_DIV (p1 p2)
    (soft_div (flt-sf (p1nd-n_dcon p1)) (flt-sf (p1nd-n_dcon p2))))  
  (defun FLOAT_ISZERO (p) (soft_isz (flt-sf p)))
  (defun FLOAT_EQ (d1 d2) (soft_cmp (flt-sf d1) (flt-sf d2) 'EQ))
  (defun FLOAT_NE (d1 d2) (soft_cmp (flt-sf d1) (flt-sf d2) 'NE))
  (defun FLOAT_GE (d1 d2) (soft_cmp (flt-sf d1) (flt-sf d2) 'GE))
  (defun FLOAT_GT (d1 d2) (soft_cmp (flt-sf d1) (flt-sf d2) 'GT))
  (defun FLOAT_LE (d1 d2) (soft_cmp (flt-sf d1) (flt-sf d2) 'LE))
  (defun FLOAT_Lt (d1 d2) (soft_cmp (flt-sf d1) (flt-sf d2) 'LT))
  (defun FLOAT_NEG (p) (setf (flt-sf p) (soft_neg (flt-sf p)))))


#|
#ifdef notdef
* ATTR_BASETYP has the following layout:
* aa[0].iarg has size
* aa[1].iarg has alignment
#endif
* ATTR_QUALTYP has the following layout:
* aa[0].iarg has CON/VOL + FUN/ARY/PTR
* Not defined yet...
* aa[3].iarg is dimension for arrays (XXX future)
* aa[3].varg is function defs for functions.
|#

;; ATTR_STRUCT member list.
(defun amlist (x) (varg x 0))
(defun amsize (x) (iarg x 0))
(defun strattr (x) (attr_find x 'ATTR_STRUCT))
(defun (setf amlist) (val a) (setf (varg a 0) val))
(defun (setf amsize) (val a) (setf (iarg a 0) val))

(defun UNASG (x)
  (ecase x
    (PLUSEQ 'PLUS)
    (MINUSEQ 'MINUS)
    (DIVEQ 'DIV)
    (MODEQ 'MOD)
    (MULEQ 'MUL)
    (ANDEQ 'AND)
    (OREQ 'OR)
    (EREQ 'ER)
    (LSEQ 'LS)
    (RSEQ 'RS)))

(defvar <=ATTR_MAX (append ATTR_MI '(:ATTR_COMPLEX :xxxATTR_BASETYP
				     :ATTR_QUALTYP :ATTR_ALIGNED
				     :ATTR_NORETURN :ATTR_STRUCT)))
