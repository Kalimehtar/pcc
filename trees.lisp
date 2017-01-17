(defpackage #:pcc.trees
  (:use #:cl #:pcc.pass1 #:pcc.pass2)
  (:import-from #:pcc.pftn #:sztable))

(in-package #:pcc.trees)

(defvar usdnodes 0)
(defvar frelink nil)

(defun buildtree (o l r)
  (declare (type symbol o)
	   (type p1nd l r))
  #+PCC_DEBUG
  (when (=/ bdebug 0)
    (format t "buildtree(~a, ~a, ~a)~%" (copst o) l r)
    (when l (p1fwalk l eprint 0))
    (when r (p1fwalk r eprint 0)))
  (let ((opty (coptype o))
	q)
    ;; check for constants
    (when (member o '(ANDAND OROR NOT))
      (when (eq (p1nd-n_op l) 'FCON)
	(let ((p (bcon (not (FLOAT_ISZERO (p1nd-n_dcon l))))))
	  (p1nfree l)
	  (setf l p)))
      (when (and (not (eq o 'NOT)) (eq (p1nd-n_op l) 'FCON))
	(let ((p (bcon (not (FLOAT_ISZERO (p1nd-n_dcon r))))))
	  (p1nfree r)
	  (setf r p))))
    (cond
      ((and (eq opty 'UTYPE) (eq (p1nd-n_op l) 'ICON))
       (case o
	 ((NOT UMINUS COMPL)
	  (when (conval l o l) (return-from buildtree l)))))
      ((and (eq o 'NOT) (eq (p1nd-n_op l) 'FCON))
       (setf l (clocal (_block 'SCONV l nil 'INT 0 0))))
      ((and (eq o 'UMINUS) (eq (p1nd-n_op l) 'FCON))
       (FLOAT_NEG (p1nd-n_dcon l))
       (return-from buildtree l))
      ((and (eq o 'QUEST) (or (eq (p1nd-n_op l) 'ICON)
			      (and (eq (p1nd-n_op l) 'NAME)
				   (ISARY (p1nd-n_type l)))))
       (let ((c (glval l)))
	 (when (eq (p1nd-n_op l) 'NAME) (setf c 1))
	 (p1nfree l)
	 (cond
	   ((/= c 0)
	    (p1walkf (p1nd-n_right r) #'putjops 0)
	    (p1tfree (p1nd-n_right r))
	    (setf l (p1nd-n_left r)))
	   (t
	    (p1walkf (p1nd-n_left l) #'putjops 0)
	    (p1tfree (p1nd-n_left l))
	    (setf r (p1nd-n_right l))))
	 (p1nfree r)
	 (return-from buildtree l)))
      ((and (eq opty 'BITYPE)
	    (eq (p1nd-n_op l) 'ICON)
	    (eq (p1nd-n_op r) 'ICON))
       (case o
	 ((PLUS MINUS MUL DIV MOD)
	  ;; Do type propagation for simple types here.
	  ;; The constant value is correct anyway.
	  ;; Maybe this op shortcut should be removed?
	  (when (and (null (p1nd-n_sp l))
		     (null (p1nd-n_sp r))
		     (null (stype-mod (p1nd-n_type l)))
		     (null (stype-mod (p1nd-n_type r))))
	    (if (stype> (p1nd-n_type l) (p1nd-n_type r))
		(setf (p1nd-n_type r) (p1nd-n_type l))
		(setf (p1nd-n_type l) (p1nd-n_type r))))
	  (when (and (not (ISPTR (p1nd-n_type l)))
		     (not (ISPTR (p1nd-n_type r))))
	    (when (conval l o r)
	      (p1nfree r)
	      (return-from buildtree l))))
	 ((ULT UGT ULE UGE LT GT LE GE EQ NE
	       ANDAND OROR AND OR ER LS RS)
	  (when (and (not (ISPTR (p1nd-n_type l)))
		     (not (ISPTR (p1nd-n_type r))))
	    (when (conval l o r)
	      (p1nfree r)
	      (return-from buildtree l))))))
      ((and (eq opty 'BITYPE)
	    (member (p1nd-n_op l) '(FCON ICON))
	    (member (p1nd-n_op r) '(FCON ICON))
	    (member o '(PLUS MINUS MUL DIV EQ NE LE LT GE GT)))
       ;; at least one side is FCON
       (cond
	 #-CC_DIV_0
	 ((and (eq o 'DIV)
	       (or (and (eq (p1nd-n_op r) 'ICON) (= (glval r) 0))
		   (and (eq (p1nd-n_op r) 'FCON)
			(FLOAT_EQ (p1nd-n_dcon r) FLOAT_ZERO))))
	  nil)
	 (t
	  (cond
	    ((eq (p1nd-n_op l) 'ICON)
	     (unless (concast l (p1nd-n_type r))
	       (_cerror "fail cast const")))
	    ((eq (p1nd-n_op r) 'ICON)
	     (unless (concast r (p1nd-n_type l))
	       (_cerror "fail cast const"))))
	  (case o
	    ((PLUS MINUS MUL DIV)
	     (case o
	       (PLUS (FLOAT_PLUS l r))
	       (MINUS (FLOAT_MINUS l r))
	       (MUL (FLOAT_MUL l r))
	       (DIV (FLOAT_DIV l r)))
	     (p1nfree r)
	     (return-from buildtree l))
	    ((EQ NE LE LT GE GT)
	     (let ((n
		    (case o
		      (EQ (FLOAT_EQ (p1nd-n_dcon l) (p1nd-n_dcon r)))
		      (NE (FLOAT_NE (p1nd-n_dcon l) (p1nd-n_dcon r)))
		      (LE (FLOAT_LE (p1nd-n_dcon l) (p1nd-n_dcon r)))
		      (LT (FLOAT_LT (p1nd-n_dcon l) (p1nd-n_dcon r)))
		      (GE (FLOAT_GE (p1nd-n_dcon l) (p1nd-n_dcon r)))
		      (GT (FLOAT_GT (p1nd-n_dcon l) (p1nd-n_dcon r)))
		      (otherwise 0)))) ;  XXX flow analysis
	       (p1nfree r)
	       (p1nfree l)
	       (return-from buildtree (bcon n))))))))
      ((and (member 'ASGOPFLG (cdope o))
	    (not (member o '(RETURN CAST))))
       (when (and (eq (p1nd-n_op l) 'SCONV)
		  (eq (p1nd-n_op (p1nd-n_left l)) 'FLD))
	 (setf l (p1nfree l)))
       ;; Handle side effects by storing address in temporary q.
       ;; Side effect nodes always have an UMUL.
       (if (has_se l)
	   (let ((ll (p1nd-n_left l)))
	     (setf q (cstknode (p1nd-n_type ll) (p1nd-n_df ll) (p1nd-n_ap ll))
		   (p1nd-n_left l) (p1tcopy q)
		   q (buildtree 'ASSIGN q ll)))
	   (setf q (bcon 0))) ; No side effects
       
       ;; Modify the trees so that the compound op is rewritten.
       ;; avoid casting of LHS
       (when (and (member 'SIMPFLG (cdope o))
		  (ISINTEGER (p1nd-n_type l))
		  (not (eq (stype-id (p1nd-n_type l)) 'BOOL)))
	 (setf r (ccast r
			(p1nd-n_type l) (p1nd-n_qual l)
			(p1nd-n_df l) (p1nd-n_ap l))))
       (setf r (buildtree (UNASG o) (p1tcopy l) r)
	     r (buildtree 'ASSIGN l r)
	     l q
	     o 'COMOP))
      ((member o '(INCR DECR))
       (when (and (eq (p1nd-n_op l) 'SCONV)
		  (eq (p1nd-n_op (p1nd-n_left l)) 'FLD))
	 (setf l (p1nfree l)))
       ;; Rewrite to (t=d,d=d+1,t)
       (if (has_se l)
	   (let ((ll (p1nd-n_left l)))
	     (setf q (cstknode (p1nd-n_type ll) (p1nd-n_df ll) (p1nd-n_ap ll))
		   (p1nd-n_left l) (p1tcopy q)
		   q (buildtree 'ASSIGN q ll)))
	   (setf q (bcon 0))) ; No side effects
       
       ;; Boolean has special syntax.
       (setf r (if (eq (stype-id (p1nd-n_type l)) 'BOOL)
		   (rewincop l r (if (eq o 'INCR) 'ASSIGN 'EREQ))
		   (rewincop l r (if (eq o 'INCR) 'PLUSEQ 'MINUSEQ)))
	     l q
	     o 'COMOP))
      ((and (eq o 'ASSIGN)
	    (eq (p1nd-n_op l) 'SCONV)
	    (eq (p1nd-n_op (p1nd-n_left l)) 'FLD))
       (setf l (p1nfree l))))

    ;; its real; we must make a new node
    (let* ((p (_block o l r 'INT nil nil))
	   (actions (opact p)))
      (when (member 'PROML actions)
	(setf (p1nd-n_left p) (intprom (p1nd-n_left p))))
      (when (member 'LVAL actions) ;; check left descendent
	(when (notlval (p1nd-n_left p))
	  (uerror "lvalue required")
	  (p1nfree p)
	  (return-from buildtree l)))
      (cond
	((member 'NCVTR actions)
	 (setf (p1nd-n_left p) (pconvert (p1nd-n_left p))))
	((not (member 'NCVT actions))
	 (case opty
	   (BITYPE
	    (setf (p1nd-n_right p) (pconvert (p1nd-n_right p))
		  (p1nd-n_left p) (pconvert (p1nd-n_left p))))
	   (UTYPE
	    (setf (p1nd-n_left p) (pconvert (p1nd-n_left p)))))))
      (when (and (member 'PUN actions)
		 (not (eq o 'CAST)))
	(chkpun p))
      (when (intersection actions '(TYPL TYPR))
	(setf q (if (member 'TYPL actions) (p1nd-n_left p) (p1nd-n_right p))
	      (p1nd-n_type p) (p1nd-n_type q)
	      (p1nd-n_qual p) (p1nd-n_qual q)
	      (p1nd-n_df p) (p1nd-n_df q)
	      (p1nd-n_ap p) (p1nd-n_ap q)))
      (when (member 'CVTL actions)
	(setf p (convert p 'CVTL)))
      (error "Unfinished"))))
      
(defun putjops (p arg)
  (declare (ignore arg))
  (when (and (eq (p1nd-n_op p) 'COMOP)
	     (eq (p1nd-n_op (p1nd-n_left p)) 'GOTO))
    (plabel (+ (glval (p1nd-n_left (p1nd-n_left p))) 2))))	     

(defun concast (p _t)
  "Do an actual cast of a constant (if possible).
Routine assumes 2-complement.  p is cast to type t.
Returns 1 if handled, 0 otherwise."
  (declare (type p1nd p)
	   (type stype _t))
  (unless (member (p1nd-n_op p) '(ICON FCON)) ;  only constants
    (return-from concast nil))
  (when (and (eq (p1nd-n_op p) 'ICON) ; no addresses
	     (p1nd-n_sp p))
    (when (equalp _t (make-stype :id 'BOOL))
      (slval p 1)
      (setf (p1nd-n_type p) _t
	    (p1nd-n_sp p) nil)
      (return-from concast t))
    (return-from concast nil))
  (flet ((TYPMSK (y)
	   (logior (ash (1- (ash 1 (1- y))) 1) 1)))
    (cond
      ((eq (p1nd-n_op p) 'ICON)
       (let ((val (glval p))
	     (t-id (stype-id _t)))
	 (unless (stype-mod _t)
	   (case t-id
	     (BOOL
	      (when val (slval p 1)))
	     ((UNDEF CHAR UCHAR
		     SHORT USHORT INT UNSIGNED LONG ULONG
		     LONGLONG ULONGLONG)
	      (slval p (logand val (TYPMSK (sztable t-id))))
	      (unless (ISUNSIGNED _t)
		(when (/= 0 (logand val (ash 1 (1- (sztable t-id)))))
		  (slval p (logorc2 (glval p)
				    (TYPMSK (sztable t-id)))))))
	     ((FLOAT DOUBLE LDOUBLE)
	      (setf (p1nd-n_op p) 'FCON
		    (p1nd-n_dcon p) (fltallo))
	      (FLOAT_INT2FP (p1nd-n_dcon p) val (p1nd-n_type p)))))))
      (t ; /* p->n_op == FCON */
       (unless (stype-mod _t)
	 (case (stype-id _t)
	   (BOOL
	    (setf (p1nd-n_op p) 'ICON)
	    (slval p (FLOAT_NE (p1nd-n_dcon p) FLOAT_ZERO))
	    (setf (p1nd-n_sp p) nil))
	   ((UNDEF CHAR UCHAR
		   SHORT USHORT INT UNSIGNED LONG ULONG
		   LONGLONG ULONGLONG)
	    (setf (p1nd-n_op p) 'ICON)
	    (FLOAT_FP2INT (glval p) (p1nd-n_dcon p) _t)
	    (setf (p1nd-n_sp p) nil))
	   (t
	    (FLOAT_FP2FP (p1nd-n_dcon p) _t)))))))
  (setf (p1nd-n_type p) _t)
  1)

(defun conval (p o q)
  (declare (type p1nd p q) (type symbol o))
  (let ((tl (p1nd-n_type p))
	(tr (p1nd-n_type q))
	(val (glval q))
	u)
    ;; make both sides same type
    (if (and (null (stype-mod tl))
	     (null (stype-mod tr)))
	(let ((td
	       (case (stype-id tl)
		 ((UNDEF BOOL CHAR UCHAR SHORT USHORT INT)
		  (if (member (stype-id tr) '(UNDEF BOOL CHAR UCHAR
					      SHORT USHORT INT))
		      'INT
		      (stype-id tr)))
		 (UNSIGNED
		  (if (member (stype-id tr) '(UNDEF BOOL CHAR UCHAR
					      SHORT USHORT INT))
		      'UNSIGNED
		      (stype-id tr)))
		 (LONG
		  (if (member (stype-id tr) '(UNDEF BOOL CHAR UCHAR
					      SHORT USHORT INT UNSIGNED))
		      'LONG
		      (stype-id tr)))
		 (ULONG
		  (if (member (stype-id tr) '(UNDEF BOOL CHAR UCHAR
					      SHORT USHORT INT UNSIGNED LONG))
		      'ULONG
		      (stype-id tr)))
		 (LONGLONG
		  (if (member (stype-id tr) '(UNDEF BOOL CHAR UCHAR
					      SHORT USHORT INT UNSIGNED LONG
					      ULONG))
		      'LONGLONG
		      (stype-id tr)))
		 (ULONGLONG
		  (if (member (stype-id tr) '(UNDEF BOOL CHAR UCHAR
					      SHORT USHORT INT UNSIGNED LONG
					      LONGLONG ULONGLONG))
		      'ULONGLONG
		      (stype-id tr)))
		 (FLOAT
		  (if (member (stype-id tr) '(DOUBLE LDOUBLE STRTY UNIONTY
					      XTYPE VOID))
		      (stype-id tr)
		      'FLOAT))
		 (DOUBLE
		  (if (member (stype-id tr) '(LDOUBLE STRTY UNIONTY
					      XTYPE VOID))
		      (stype-id tr)
		      'DOUBLE))
		 (LDOUBLE
		  (if (member (stype-id tr) '(STRTY UNIONTY
					      XTYPE VOID))
		      (stype-id tr)
		      'LDOUBLE))
		 (STRTY
		  (if (member (stype-id tr) '(UNIONTY
					      XTYPE VOID))
		      (stype-id tr)
		      'STRTY))
		 (UNIONTY
		  (if (member (stype-id tr) '(XTYPE VOID))
		      (stype-id tr)
		      'UNIONTY))
		 (XTYPE
		  (if (eq (stype-id tr) 'VOID)
		      'VOID
		      'XTYPE))
		 (VOID 'VOID))))
	  (setf u (ISUNSIGNED td))
	  (unless (eq td (stype-id tl))
	    (setf p (makety p td 0 0 0)))
	  (unless (eq td (stype-id tr))
	    (setf q (makety q  td 0 0 0))))
	(setf u (or (ISUNSIGNED tl) (ISUNSIGNED tr))))
    (when u
      (setf o
	    (case o
	      (LE 'ULE)
	      (LT 'ULT)
	      (GE 'UGE)
	      (GT 'UGT)
	      (otherwise o))))
    (when (and (node-n_sp p) (node-n_sp q))
      (return-from conval 0))
    (when (and (node-n_sp q) (not (eq o 'PLUS)))
      (return-from conval 0))
    (when (and (node-n_sp p) (not (eq o 'PLUS)) (not (eq o 'MINUS)))
      (return-from conval 0))
    ;;; !!!! FIXME: Monk: C arith and Lisp arith may differ
    (let ((v1 (glval p))
	  (v2 (glval q)))
      (when (and (= v2 0) (member 'DIVFLG (cdope o)))
	(return-from conval 0)) ;; leave division by zero to runtime
      (case o
	(PLUS
	 (slval p (+ (glval p) val))
	 (when (null (node-n_sp p))
	   (setf (node-n_right p) (node-n_right q)
		 (node-n_left p) (node-n_left q))))
	(MINUS
	 (slval p (- (glval p) val)))
	(MUL
	 (slval p (* (glval p) val)))
	(DIV
	 (cond
	   (u
	    (setf v1 (mod v1 v2))
	    (slval p v1))
	   (t
	    (slval p (mod (glval p) val)))))
	(MOD
	 (cond
	   (u
	    (setf v1 (rem v1 v2))
	    (slval p v1))
	   (t
	    (slval p (rem (glval p) val)))))
	(AND
	 (slval p (logand (glval p) val)))
	(OR
	 (slval p (logior (glval p) val)))
	(ER
	 (slval p (logxor (glval p) val)))
	(LS
	 (slval p (ash (glval p) val)))
	(RS
	 (cond
	   (u
	    (setf v1 (ash v1 (- val)))
	    (slval p v1))
	   (t
	    (slval p (ash (glval p) (- val))))))
	(UMINUS
	 (slval p (- (glval p))))
	(COMPL
	 (slval p (lognot (glval p))))
	(NOT
	 (slval p (if (= (glval p) 0) 1 0))
	 (setf (p1nd-n_type p) (make-stype :id 'INT)))
	(LT
	 (slval p (if (< (glval p) val) 1 0)))
	(LE
	 (slval p (if (<= (glval p) val) 1 0)))
	(GT
	 (slval p (if (> (glval p) val) 1 0)))
	(GE
	 (slval p (if (>= (glval p) val) 1 0)))
	(ULT
	 (slval p (if (< v1 v2) 1 0)))
	(ULE
	 (slval p (if (<= v1 v2) 1 0)))
	(UGT
	 (slval p (if (> v1 v2) 1 0)))
	(UGE
	 (slval p (if (>= v1 v2) 1 0)))
	(EQ
	 (slval p (if (= (glval p) val) 1 0)))
	(NE
	 (slval p (if (/= (glval p) val) 1 0)))
	(ANDAND
	 (slval p (if (and (/= (glval p) 0)
			   (/= val 0))
		      1 0)))
	(OROR
	 (slval p (if (or (/= (glval p) 0)
			  (/= val 0))
		      1 0)))
	(t (return-from conval 0)))
      ;; Do the best in making everything type correct after calc
      (when (clogop o) (setf (p1nd-n_type p) (make-stype :id 'INT)))
      (when (and (null (p1nd-n_sp p))
		 (null (p1nd-n_sp q)))
	(slval p (valcast (glval p) (p1nd-n_type p))))
      1)))

;; * Ensure that v matches the type t; sign- or zero-extended
;; * as suitable to CONSZ.
;; * Only to be used for integer types.
(defun valcast (v _t)
  (when (member _t '(UNDEF BOOL FLOAT DOUBLE LDOUBLE STRTY UNIONTY
		     XTYPE VOID))
    (return-from valcast v)) ;  cannot cast
  (when (member _t '(LONGLONG ULONGLONG))
    (return-from valcast v)) ;  already largest
  (let ((sz (tsize _t nil nil)))
    (flet ((m (x)
	     (1+ (ash (1- (ash 1 (1- x))) 1))))
      (let ((r (logand v (m sz))))
	(when (and (ISUNSIGNED _t)
		   (logtest (ash 1 (1- sz)) r))
	  (setf r (logior (lognot (m sz)) r)))
	r))))
		     
(defun stref (p)
  (declare (type p1nd p))
  ;; make p->x
  ;; this is also used to reference automatic variables
  (let ((s (p1nd-n_sp (p1nd-n_right p))))
    (p1nfree (p1nd-n_right p))
    (let ((xap
    #+GCC_COMPAT (attr_find (p1nd-n_ap r) 'GCC_ATYP_PACKED)
    #-GCC_COMPAT nil)
	  (p (pconvert p)))
      (error "Unfinished"))))

(defun bcon (i)
  "make a constant node with value i"
  (xbcon i nil 'INT))

(defun xbcon (val sp type)
  (declare (type symtab sp)
	   (type symbol type))
  (let ((p (_block 'ICON nil nil type 0 0)))
    (slval p val)
    (setf (p1nd-n_sp p) sp)
    (clocal p)))

(defun pconvert (p)
  (declare (type p1nd p))
  ;; if p should be changed into a pointer, do so 
  (cond
    ((ISARY (p1nd-n_type p))
     (setf (p1nd-n_type p) (DECREF (p1nd-n_type p)))
     (incf (p1nd-n_df p))
     (buildtree '(ADDROF) p nil))
    ((ISFTN (p1nd-n_type p))
     (buildtree '(ADDROF) p nil))
    (t p)))

(defun makety (p _t q d ap)
  (declare (type p1nd p)
	   (type stype _t)
	   (type (or null attr) ap))
  (when (equalp _t (p1nd-n_type p))
    (setf (p1nd-n_df p) d
	  (p1nd-n_ap p) ap
	  (p1nd-n_qual p) q)
    (return-from makety p))
  (when (or (ISITY _t)
	    (ISCTY _t)
	    (ISITY (p1nd-n_type p))
	    (ISCTY (p1nd-n_type p)))
    (_cerror "makety"))
  (when (concast p _t)
    (return-from makety (clocal p)))
  (let ((p (_block (if (stype-mod _t) 'PCONV 'SCONV) p nil _t d ap)))
    (setf (p1nd-n_qual p) q)
    (clocal p)))

(defun _block (o l r _t d ap)
  (declare (type symbol o _t)
	   (type (or null attr) ap))
  (let ((p (p1alloc)))
    (setf (p1nd-n_rval p) 0
	  (p1nd-n_op p) o
	  (p1nd-n_left p) l
	  (p1nd-n_right p) r
	  (p1nd-n_type p) _t
	  (p1nd-n_qual p) 0
	  (p1nd-n_df p) d
	  (p1nd-n_ap p) ap)
    p))

;; the intent of this table is to examine the
;; operators, and to check them for
;; correctness.
;;
;; The table is searched for the op and the
;; modified type (where this is one of the
;; types INT (includes char and short), LONG,
;; DOUBLE (includes FLOAT), and POINTER
;;
;; The default action is to make the node type integer
;;
;; The actions taken include:
;;      PUN       check for puns
;;      CVTL      convert the left operand
;;      CVTR      convert the right operand
;;      TYPL      the type is determined by the left operand
;;      TYPR      the type is determined by the right operand
;;      TYMATCH   force type of left and right to match,by inserting conversions
;;      PTMATCH   like TYMATCH, but for pointers
;;      LVAL      left operand must be lval
;;      CVTO      convert the op
;;      NCVT      do not convert the operands
;;      OTHER     handled by code
;;      NCVTR     convert the left operand, not the right...


(defun opact (p)
  (let ((o (p1nd-n_op p))
	mt1 mt2 mt12)
    (case o
      (BITYPE
       (setf mt12 (setf mt2 (moditype (p1nd-n_type (p1nd-n_right p))))
	     mt1 (moditype (p1nd-n_type (p1nd-n_left p)))
	     mt12 (intersection mt12 mt1)))		   
      (UTYPE
       (setf mt1 (moditype (p1nd-n_type (p1nd-n_left p)))
	     mt12 (intersection mt12 mt1))))
    (flet ((err-ncvt ()
	     (uerror "operands of ~a have incompatible types" (copst o))
	     '(NCVT)))
      (case o
	((NAME ICON FCON CALL UCALL UMUL)
	 '(OTHER))
	(UMINUS (if (member 'MDBI mt1)
		    '(TYPL PROML)
		    (err-ncvt)))
	(COMPL (if (member 'MINT mt1)
		   '(TYPL PROML)
		   (err-ncvt)))
	((CM CBRANCH ANDAND OROR) nil)
	((MUL DIV) (if (member 'MDBI mt12)
		       '(TYMATCH)
		       (err-ncvt)))
	((MOD AND OR ER) (if (member 'MINT mt12)
			     '(TYMATCH)
			     (err-ncvt)))
	((LS RS) (if (member 'MINT mt12)
		     '(TYPL OTHER PROML)
		     (err-ncvt)))
	((EQ NE LT LE GT GE)
	 (cond
	   ((member 'MDBI mt12) '(TYMATCH CVTO))
	   ((member 'MPTR mt12) '(PTMATCH PUN CVTO))
	   ((member 'MPTI mt12) '(PTMATCH PUN))
	   (t (err-ncvt))))
	(QUEST '(TYPR OTHER))
	(COMOP '(TYPR))
	(STREF '(NCVTR OTHER))
	(FORCE '(TYPL))
	(COLON
	 (cond
	   ((member 'MDBI mt12) '(TYMATCH))
	   ((member 'MPTR mt12) '(TYPL PTMATCH PUN))
	   ((and (member 'MINT mt1) (member 'MPTR mt2))
	    '(TYPR PUN))
	   ((and (member 'MPTR mt1) (member 'MINT mt2))
	    '(TYPL PUN))
	   ((member 'MSTR mt12) '(NCVT TYPL OTHER))
	   (t (err-ncvt))))
	((ASSIGN RETURN)
	 (cond
	   ((member 'MSTR mt12) '(LVAL NCVT TYPL OTHER))
	   ((member 'MDBI mt12) '(TYPL LVAL TYMATCH))
	   ((member 'MPTR mt1) '(LVAL PTMATCH PUN))
	   ((member 'MPTI mt12) '(TYPL LVAL TYMATCH PUN))
	   (t (err-ncvt))))
	(CAST
	 (cond
	   ((member 'MDBI mt12) '(TYPL LVAL TYMATCH))
	   ((member 'MPTR mt1) '(LVAL PTMATCH PUN))
	   ((member 'MPTI mt12) '(TYPL LVAL TYMATCH PUN))
	   (t (err-ncvt))))
	(MINUS
	 (cond
	   ((member 'MPTR mt12) '(CVTO PTMATCH PUN))
	   ((member 'MPTR mt2) (err-ncvt))
	   ((member 'MDBI mt12) '(TYMATCH))
	   ((and (member 'MPTR mt1) (member 'MINT mt2))
	    '(TYPL CVTR))
	   ((and (member 'MINT mt1) (member 'MPTR mt2))
	    '(TYPR CVTL))
	   (t (err-ncvt))))
	(PLUS
	 (cond
	   ((member 'MDBI mt12) '(TYMATCH))
	   ((and (member 'MPTR mt1) (member 'MINT mt2))
	    '(TYPL CVTR))
	   ((and (member 'MINT mt1) (member 'MPTR mt2))
	    '(TYPR CVTL))
	   (t (err-ncvt))))))))

(defun moditype (ty)
  (if (stype-mod ty)
      '(MPTR MPTI)
      (case (stype-id ty)
	((STRTY UNIONTY) '(MSTR))
	((BOOL CHAR SHORT UCHAR USHORT UNSIGNED ULONG ULONGLONG INT
	       LONG LONGLONG)
	 '(MINT MDBI MPTI))
	((FLOAT DOUBLE LDOUBLE) '(MDBI))
	#-NO_COMPLEX
	((FCOMPLEX COMPLEX LCOMPLEX FIMAG IMAG LIMAG) '(MDBI))
	(otherwise '(MPTR MPTI)))))

(defvar tvaloff (if (> (+ MAXREGS NPERMREG) 100) (+ MAXREGS NPERMREG 100) 100))

;; Returns a TEMP node with temp number nr.
;; If nr == 0, return a node with a new number.
(defun tempnode (nr type df ap)
  (let ((r (_block 'TEMP nil nil type df ap)))
    (setf (regno r) (or nr tvaloff))
    (incf tvaloff (szty type))
    r))

(defun cstknode (_t df ap)
  "Create a node for either TEMP or on-stack storage"
  ;;  create a symtab entry suitable for this type
  (let ((sp (getsymtab "0hej" 'SSTMT)))
    (setf (symtab-stype sp) _t
	  (symtab-sdf sp) df
	  (symtab-sap sp) ap
	  (symtab-sclass sp) 'AUTO
	  (symtab-soffset sp) 'NOOFFSET)
    (oalloc sp 'autooff)
    (error "Unfinished")))

(defun has_se (p)
  "Return t if an assignment is found"
  (cond
    ((and (eq (p1nd-n_op p) 'COMOP)
	  (eq (p1nd-n_op (p1nd-n_left p)) 'GOTO))
     t)
    ((member 'ASSFLG (cdope (p1nd-n_op p))) t)
    ((eq (coptype (p1nd-n_op p)) 'LTYPE) nil)
    ((has_se (p1nd-n_left p)) t)
    ((eq (coptype (p1nd-n_op p)) 'BITYPE)
     (has_se (p1nd-n_right p)))
    (t nil)))

(defun send_passt (type &rest args)
  "Send something further on to the next pass"
  (when (and (null cftnsp) (not (eq type 'IP_ASM)))
    (when (eq type 'IP_NODE)
      (tfree (pop args)))
    (return-from send_passt))
;  (let ((sz (if (member type '(IP_PROLOG IP_EPILOG))
  (let ((ip (make-interpass :type type :lineno lineno)))
    (case type
      ((IP_NODE)
       (setf (ip_node ip) (pop args))))
    (error "Unfinished")
  ))

    
(defun cdope (op)
  (declare (type symbol op))
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

;* Free a node, and return its left descendant.
;* It is up to the caller to know whether the return value is usable.
(defun p1nfree (p)
  (unless p
    (_cerror "freeing blank node!"))
  (when (eq (p1nd-n_op p) 'FREE)
    (_cerror "freeing FREE node ~a" p))
  (when ndebug
    (format t "freeing node ~a~%" p))
  (let ((l (p1nd-n_left p)))
    (setf (p1nd-n_op p) 'FREE
	  (p1nd-n_left p) frelink
	  frelink p)
    (decf usdnodes)
    l))

(defun p1alloc ()
  (incf usdnodes)
  (cond
    (frelink
     (let ((p frelink))
       (setf frelink (p1nd-n_left p))
       (unless (eq (p1nd-n_op p) 'FREE)
	 (_cerror "node not FREE: ~a" p))
       (when ndebug
	 (format t "alloc p1node ~a from freelist~%" p))
       p))
    (t
     (let ((p (make-p1nd :n_op 'FREE)))
       (when ndebug
	 (format t "alloc p1node ~a from memory~%" p))
       p))))

;; free the tree p
(defun p1tfree (p)
  (when (eq (p1nd-n_op p) 'FREE)
    (_cerror "freeing FREE node"))
  (p1walkf p #'p1nfree nil))

(defun p1walkf (p f arg)
  (let ((opty (coptype (p1nd-n_op p))))
    (unless (eq opty 'LTYPE) (p1walkf (p1nd-n_left p) f arg))
    (when (eq opty 'BITYPE) (p1walkf (p1nd-n_right p) f arg)))
  (apply f t arg))

(defun plabel (label)
  "set PROG-seg label"
  (setf reached 1) ; /* Will this always be correct? */
  (send_passt 'IP_DEFLAB label))

(defun cqual (_t q)
  "Return CON/VOL/null, whichever are active for the current type."
  (loop
     (unless (ISARY _t)
       (return))
     (setf _t (DECREF _t)
	   q (DECQAL q)))
  (when (stype-mod _t)
    (intersection (stype-mod q) '(CON VOL))))
