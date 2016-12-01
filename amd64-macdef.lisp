(defpackage #:pcc.amd64-macdef (:use #:cl))

(in-package #:pcc.amd64-macdef)

(defconstant ARGINIT 128) ;  # bits above fp where arguments start 
(defconstant AUTOINIT 0)  ;  # bits below fp where automatics start

;;  Storage space requirements
(defconstant SZCHAR          8)
(defconstant SZBOOL          8)
(defconstant SZSHORT         16)
(defconstant SZINT           32)
(defconstant SZLONG          64)
(defmacro SZPOINT (tt) (declare (ignore tt)) 64)
(defconstant SZLONGLONG      64)
(defconstant SZFLOAT         32)
(defconstant SZDOUBLE        64)
(defconstant SZLDOUBLE       128)

;; Alignment constraints
(defconstant ALCHAR          8)
(defconstant ALBOOL          8)
(defconstant ALSHORT         16)
(defconstant ALINT           32)
(defconstant ALLONG          64)
(defconstant ALPOINT         64)
(defconstant ALLONGLONG      64)
(defconstant ALFLOAT         32)
(defconstant ALDOUBLE        64)
(defconstant ALLDOUBLE       128)
;/* #undef ALSTRUCT      amd64 struct alignment is member defined */
(defconstant ALSTACK         64)
(defconstant ALMAX           128)

;; Min/max values.
(defconstant MIN_CHAR        -128)
(defconstant MAX_CHAR        127)
(defconstant MAX_UCHAR       255)
(defconstant MIN_SHORT       -32768)
(defconstant MAX_SHORT       32767)
(defconstant MAX_USHORT      65535)
(defconstant MIN_INT         (1- (- #x7fffffff)))
(defconstant MAX_INT         #x7fffffff)
(defconstant MAX_UNSIGNED    #xffffffff)
(defconstant MIN_LONG        #x8000000000000000)
(defconstant MAX_LONG        #x7fffffffffffffff)
(defconstant MAX_ULONG       #xffffffffffffffff)
(defconstant MIN_LONGLONG    #x8000000000000000)
(defconstant MAX_LONGLONG    #x7fffffffffffffff)
(defconstant MAX_ULONGLONG   #xffffffffffffffff)

;  Default char is signed 
(defconstant  CHAR_UNSIGNED nil)
(defconstant BOOL_TYPE       'UCHAR) ;   /* what used to store _Bool */


;;;;; ............


(defconstant TARGET_VALIST t)
(defconstant TARGET_STDARGS t)
#|
#define TARGET_BUILTINS                                                 \
        { "__builtin_stdarg_start", amd64_builtin_stdarg_start,         \
                                                0, 2, 0, VOID },        \
        { "__builtin_va_start", amd64_builtin_stdarg_start,             \
                                                0, 2, 0, VOID },        \
        { "__builtin_va_arg", amd64_builtin_va_arg, BTNORVAL|BTNOPROTO, \
                                                        2, 0, 0 },      \
        { "__builtin_va_end", amd64_builtin_va_end, 0, 1, 0, VOID },    \
        { "__builtin_va_copy", amd64_builtin_va_copy, 0, 2, 0, VOID },



|#
