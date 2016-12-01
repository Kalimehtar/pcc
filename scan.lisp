(defpackage #:pcc.scan
  (:use #:cl)
  (:import-from #:pcc.pass1 #:pragma_allpacked))

(in-package #:pcc.scan)

(defvar pragma_allpacked 0)
