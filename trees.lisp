(in-package #:pcc)

(defun cdope (op)
  (or (gethash op dope)
      (case op
	((CLOP STRING QUALIFIER CLASS RB ELLIPSIS TYPE ALIGN FUNSPEC)
	 '(LTYPE))
	((DOT SZOF COMOP QUEST BIQUEST COLON LB TYMERGE STREF)
	 '(BITYPE))
	((XIMAG XREAL ATTRIB LABEL UPLUS)
	 '(UTYPE))
	((ANDAND OROR)
	 '(BITYPE LOGFLG))
	((NOT) '(UTYPE LOGFLG))
	((CAST) '(BITYPE ASGFLG ASGOPFLG))
	((PLUSEQ) '(BITYPE ASGFLG ASGOPFLG FLOFLG SIMPFLG COMMFLG))
	((MINUSEQ) '(BITYPE FLOFLG SIMPFLG ASGFLG ASGOPFLG))
	((MULEQ) '(BITYPE FLOFLG MULFLG ASGFLG ASGOPFLG))
	((OREQ EREQ ANDEQ)
	 '(BITYPE SIMPFLG COMMFLG ASGFLG ASGOPFLG))
	((DIVEQ)
	 '(BITYPE FLOFLG MULFLG DIVFLG ASGFLG ASGOPFLG))
	((MODEQ)
	 '(BITYPE DIVFLG ASGFLG ASGOPFLG))
	((LSEQ RSEQ)
	 '(BITYPE SHFFLG ASGFLG ASGOPFLG))
	((INCR DECR)
	 '(BITYPE ASGFLG))
	(t (_cerror "cdope missing op ~a" op)))))

(defun coptype (o)
  (intersection o '(BITYPE LTYPE UTYPE)))

(defun clogop (o)
  (intersection 0 '(LOGFLG)))

(defun casgop (o)
  (intersection 0 '(ASGFLG)))  

; node conventions:
;
;NAME:   rval>0 is stab index for external
;        rval<0 is -inlabel number
;        lval is offset in bits
;ICON:   lval has the value
;        rval has the STAB index, or - label number,
;                if a name whose address is in the constant
;        rval = NONAME means no name
;REG:    rval is reg. identification cookie

(defvar usdnodes 0)
(defvar frelink nil)

;* Free a node, and return its left descendant.
;* It is up to the caller to know whether the return value is usable.
(defun p1nfree (p)
  (unless p
    (_cerror "freeing blank node!"))
  (when (eq (node-op p) 'FREE)
    (_cerror "freeing FREE node ~a" p))
  (when ndebug
    (format t "freeing node ~a~%" p))
  (let ((l (node-left p)))
    (setf (node-op p) 'FREE
	  (node-left p) frelink
	  frelink p)
    (decf usdnodes)
    l))

(defun nfree (p) (p1nfree p))

(defun p1alloc ()
  (incf usdnodes)
  (cond
    (frelink
     (let ((p frelink))
       (setf frelink (node-left p))
       (unless (eq (node-op p) 'FREE)
	 (_cerror "node not FREE: ~a" p))
       (when ndebug
	 (format t "alloc p1node ~a from freelist~%" p))
       p))
    (t
     (let ((p (make-node :op 'FREE)))
       (when ndebug
	 (format t "alloc p1node ~a from memory~%" p))
       p))))

;; free the tree p
(defun p1tfree (p)
  (when (eq (node-op p) 'FREE)
    (_cerror "freeing FREE node"))
  (p1walkf p #'p1nfree nil))

(defun p1walkf (p f arg)
  (let ((opty (coptype (node-op p))))
    (unless (eq opty 'LTYPE) (p1walkf (node-left p) f arg))
    (when (eq opty 'BITYPE) (p1walkf (node-right p) f arg)))
  (apply f t arg))

(defun _block (o l r _t d ap)
  (let ((p (p1alloc)))
    (setf (node-rval p) 0
	  (node-op p) o
	  (node-left p) l
	  (node-right p) r
	  (node-type p) _t
	  (node-qual p) 0
	  (node-df p) d
	  (node-ap p) ap)
    p))
