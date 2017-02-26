(defpackage #:pcc.pass2
  (:use #:cl #:pcc.mip-manifest #:pcc.external)
  (:export

   #:ndebug
   #:wdebug

   ;; from external
   #:NPERMREG

   ;; from mip-manifest
   #:INCREF
   #:lineno
   #:MODTYPE

   #:stype
   #:stype-id
   #:stype-mod
   #:make-stype
   #:type-class
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
   #:dope
   #:mkdope
   #:deunsign
   
   #:gflag
   #:kflag
   #:pflag
   #:sspflag
   #:xssa
   #:xtailcall
   #:xtemps
   #:xdeljumps
   #:xdce
   #:Wflags
   #:walkf
   #:tfree

   ;; from node
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
   #:setlval))

(in-package #:pcc.pass2)

#|
;; cookies, used as arguments to codgen 
(defconstant FOREFF  #o01)              ; compute for effects only
(defconstant INAREG  #o02)              ; /* compute into a register */
(defconstant INBREG  #o04)              ;/* compute into a register */
(defconstant INCREG  #o010)             ;/* compute into a register */
(defconstant INDREG  #o020)             ;/* compute into a register */
(defconstant INREGS  (logior INAREG INBREG INCREG INDREG))
(defconstant FORCC   #o040             ;/* compute for condition codes only */
(defconstant QUIET   #o0100            ;/* tell geninsn() to not complain if fail */
(defconstant INTEMP  #o010000          ;/* compute into a temporary location */
(defconstant FORREW  #o040000          ;/* search the table for a rewrite rule */
(defconstant INEREG  #x10000         ;/* compute into a register, > 16 bits */
(defconstant INFREG  #x20000         ;/* compute into a register, > 16 bits */
(defconstant INGREG  #x40000         ;/* compute into a register, > 16 bits */

;/*
; * OP descriptors,
; * the ASG operator may be used on some of these
; */
(defconstant OPSIMP  #o010000)          ;/* +, -, &, |, ^ */
(defconstant OPCOMM  #o010002)          ;/* +, &, |, ^ */
(defconstant OPMUL   #o010004)          ;/* *, / */
(defconstant OPDIV   #o010006)          ;/* /, % */
(defconstant OPUNARY #o010010)          ;/* unary ops */
(defconstant OPLEAF  #o010012)          ;/* leaves */
(defconstant OPANY   #o010014)          ;/* any op... */
(defconstant OPLOG   #o010016)          ;/* logical ops */
(defconstant OPFLOAT #o010020)          ;/* +, -, *, or / (for floats) */
(defconstant OPSHFT  #o010022)          ;/* <<, >> */
(defconstant OPLTYPE #o010024)       leaf type nodes (e.g, NAME, ICON, etc.)

;/* shapes */
(defconstant SANY    #o01)             ; /* same as FOREFF */
(defconstant SAREG   #o02)             ; /* same as INAREG */
(defconstant SBREG   #o04)             ; /* same as INBREG */
(defconstant SCREG   #o010)            ; /* same as INCREG */
(defconstant SDREG   #o020)            ; /* same as INDREG */
(defconstant SCC     #o040)            ; /* same as FORCC */
(defconstant SNAME   #o0100)
(defconstant SCON    #o0200)
(defconstant SFLD    #o0400)
(defconstant SOREG   #o01000)
(defconstant STARNM  #o02000)
(defconstant STARREG #o04000)
(defconstant SWADD   #o040000)
(defconstant SPECIAL #o0100000)
(defconstant SZERO   SPECIAL)
(defconstant SONE    (logior SPECIAL 1))
(defconstant SMONE   (logior SPECIAL 2))
(defconstant SCCON   (logior SPECIAL 3))   ;  /* -256 <= constant < 256 */
(defconstant SSCON   (logior SPECIAL 4))   ;  /* -32768 <= constant < 32768 */
(defconstant SSOREG  (logior SPECIAL 5))   ;  /* non-indexed OREG */
(defconstant MAXSPECIAL      (logior SPECIAL 5))
(defconstant SEREG   #x10000)       ;  /* same as INEREG */
(defconstant SFREG   #x20000)       ;  /* same as INFREG */
(defconstant SGREG   #x40000)       ;  /* same as INGREG */

; /* These are used in rstatus[] in conjunction with SxREG */
(defconstant TEMPREG 01000)
(defconstant PERMREG 02000)

/* tshape() return values */
(defconstant SRNOPE  0)  ;               /* Cannot match any shape */
(defconstant SRDIR   1)  ;             /* Direct match */
(defconstant SROREG  2)  ;             /* Can convert into OREG */
(defconstant SRREG   3)  ;             /* Must put into REG */

|#

(defvar ndebug)
(defvar wdebug)
