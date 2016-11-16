(in-package #:pcc)

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
   (make-kw "__typeof__", nil 'C_TYPEOF)    ; 11
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

(defun gcc-init ()
  