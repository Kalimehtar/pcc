(defsystem :pcc
  :serial t
  :components ((:file "package.lisp")
               (:file "mip-manifest.lisp")
	       (:file "mip-node.lisp"
               (:file "pass1.lisp")
               (:file "amd64-macdef.lisp")
	       (:file "mip-common.lisp")
	       (:file "gcc-compat.lisp")
               ))
