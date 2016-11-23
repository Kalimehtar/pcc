(in-package #:pcc)

(defun ctype (type)
  (case (BTYPE type)
    ((LONGLONG) (MODTYPE type LONG))
    ((ULONGLONG) (MODTYPE type ULONG)))
  type)
