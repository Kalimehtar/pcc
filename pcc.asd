(defsystem :pcc
  :serial t
  :components ((:file "package.lisp")
               (:file "mip-manifest.lisp")
               (:file "pass1.lisp")
               (:file "amd64-macdef.lisp")
               ))