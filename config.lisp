(defpackage #:pcc-config
  (:use #:cl))

(in-package #:pcc-config)

(defconstant ELFABI 1)

; Define to 1 if printf supports C99 size specifiers
(defconstant HAVE_C99_FORMAT 1)

(defconstant HAVE_FFS 1)

; Define to 1 if you have the `getopt' function. 
(defconstant HAVE_GETOPT 1)

; Define to 1 if you have the <inttypes.h> header file. 
(defconstant HAVE_INTTYPES_H 1)

; Define to 1 if you have the <libgen.h> header file. */
(defconstant HAVE_LIBGEN_H 1)

