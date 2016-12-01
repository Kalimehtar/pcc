(defsystem :pcc
  :serial t
  :components ((:file #:mip-node)
	       (:file #:mip-manifest)
               (:file #:pass1)
               (:file #:amd64-macdef)
	       (:file #:pass2)
	       (:file #:mip-common)
	       ;(:file #:gcc-compat)
	       (:file #:symtabs)
	       (:file #:scan)
	       (:file #:pftn)
	       (:file #:main)
	       (:file #:init)
	       (:file #:amd64-code)
               ))
