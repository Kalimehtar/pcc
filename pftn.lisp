(defpackage #:pcc.pftn
  (:use #:cl
	#:pcc.pass1)
  (:shadow
   #:node
   #:make-node
   #:node-n_op
   #:node-n_type
   #:node-n_qual
   #:node-n_su
   #:node-n_ap
   #:node-n_reg
   #:node-n_regw
   #:node-n_name
   #:node-n_df
   #:node-n_label
   #:node-n_val
   #:node-n_left
   #:node-n_slval
   #:node-n_right
   #:node-n_rval
   #:node-n_sp
   #:node-n_dcon)
  (:export
   #:symtabcnt))

(in-package #:pcc.pftn)

;;#define NODE P1ND
(deftype node () 'p1nd)
(defun node-n_op (n) (p1nd-n_op n))
(defun (setf node-n_op) (val n) (setf (p1nd-n_op n) val))
(defun node-n_sp (n) (p1nd-n_sp n))
(defun (setf node-n_sp) (val n) (setf (p1nd-n_sp n) val))
(defun node-n_type (n) (p1nd-n_type n))
(defun (setf node-n_type) (val n) (setf (p1nd-n_type n) val))
(defun node-n_qual (n) (p1nd-n_qual n))
(defun (setf node-n_qual) (val n) (setf (p1nd-n_qual n) val))
(defun node-n_ap (n) (p1nd-n_ap n))
(defun (setf node-n_ap) (val n) (setf (p1nd-n_ap n) val))
(defun node-n_df (n) (p1nd-n_df n))
(defun (setf node-n_df) (val n) (setf (p1nd-n_df n) val))
	     
;;#define tfree p1tfree
;;#define nfree p1nfree
;;#define ccopy p1tcopy
;;#define flist p1flist
;;#define fwalk p1fwalk

(defvar cftnsp nil)

(defvar blevel 0)
(defvar reached nil) ;; Monk: boolean

(defvar symtabcnt) ;; statistics
(defvar arglistcnt 0)
(defvar dimfuncnt 0)
(defvar suedefcnt 0)

(defvar autooff 0 "the next unused automatic offset")
(defvar argoff 0 "the next unused argument offset")
(defvar maxautooff 0 "highest used automatic offset in function")

(deftype node () 'p1nd)

;; Linked list stack while reading in structs.
(defstruct rstack
  (rnext nil :type (or null rstack))
  (rsou nil :type symbol)
  (rstr 0 :type fixnum)
  (rsym nil :type (or null symtab))
  (rb nil :type (or null symtab))
  (ap nil :type attr)
  flags)

(defvar rpole)

;; basic attributes for structs and enums

(defun seattr ()
  (attr_add (attr_new 'ATTR_ALIGNED  4) (attr_new 'ATTR_STRUCT 2)))

(defun defstr (sp class)
  "Struct/union/enum symtab construction."
  (setf (symtab-sclass sp) class)
  (case class
    ((STNAME) (setf (symtab-stype sp) (make-stype :id 'STRTY)))
    ((UNAME) (setf (symtab-stype sp) (make-stype :id 'UNIONTY)))
    ((ENAME) (setf (symtab-stype sp) (make-stype :id 'ENUMTY)))))
    
;; * Declare a struct/union/enum tag.
;; * If not found, create a new tag with UNDEF type.
(defun deftag (name class)
  (let ((sp (lookup name 'STAGNAME)))
    (cond
      ((null (symtab-sap sp))
       (defstr sp class))
      ((not (eq (symtab-sclass sp) class))
       (uerror "tag ~a redeclared" name)))
    sp))

;; * begining of structure or union declaration
;; * It's an error if this routine is called twice with the same struct.
(defun bstruct (name soru gp)
  (declare (type (or null string) name)
	   (type symbol soru)
	   (type (or null node) gp)
	   (ignorable gp))
  (let ((gap
	 #+GCC_COMPAT (if gp (gcc_attr_parse gp) nil)
	 #-GCC_COMPAT nil))
    (let (sp)
      (cond
	(name
	 (setf sp (deftag name soru))
	 (unless (symtab-sap sp)
	   (setf (symtab-sap sp) (seattr)))
	 (let ((ap (attr_find (symtab-sap sp) 'ATTR_ALIGNED)))
	   (when (/= (iarg ap 0) 0)
	     (cond
	       ((< (symtab-slevel sp) blevel)
		(setf sp (hide sp))
		(defstr sp soru)
		(setf (symtab-sap sp) (seattr)))
	       (t (uerror "~a redeclared" name))))
	   (setf gap (setf (symtab-sap sp) (attr_add (symtab-sap sp) gap)))))
	(t
	 (setf gap (attr_add (seattr) gap))
	 (setf sp nil)))
      (let ((r (make-rstack :rsou soru :rsym sp :rb nil :ap gap :rnext rpole)))
	(setf rpole r)
	r))))

;; * Called after a struct is declared to restore the environment.
;; * - If ALSTRUCT is defined, this will be the struct alignment and the
;; *   struct size will be a multiple of ALSTRUCT, otherwise it will use
;; *   the alignment of the largest struct member.

(defun dclstruct (r)
  (declare (type rstack r))
  (let ((apb (attr_find (rstack-ap r) 'ATTR_ALIGNED))
	(aps (attr_find (rstack-ap r) 'ATTR_STRUCT)))
    (setf (amlist aps) (rstack-rb r))
    (let ((al (if (boundp 'ALSTRUCT)
		  (symbol-value 'ALSTRUCT)
		  ALCHAR)))
      ;; extract size and alignment, calculate offsets
      (do ((sp (rstack-rb r) (symtab-snext sp))) ((null sp) nil)
	(let* ((sa (talign (symtab-stype sp) (symtab-sap sp)))
	       (sz (if (class-fieldp (symtab-sclass sp))
		       (class-fldsiz (symtab-sclass sp))
		       (tsize (symtab-stype sp)
			      (symtab-sdf sp)
			      (symtab-sap sp)))))
	  (when (> sz (rstack-rstr rpole))
	    (setf (rstack-rstr rpole) sz)) ;; for use with unions
	  
	  ;; * set al, the alignment, to the lcm of the alignments
	  ;; * of the members.
	  (SETOFF al sa)))
      (SETOFF (rstack-rstr rpole) al)
      (setf (amsize aps) (rstack-rstr rpole)
	    (iarg apb 0) al)
      #+PCC_DEBUG
      (progn
	(when (/= ddebug 0)
	  (format t "dclstruct(~a): size=~a, align=~a~%",
		  (if (rstruct-rsym r)
		      (symtab-sname (rstruct-rsym r))
		      "??")
		  (amsize aps)
		  (iarg apb 0)))
	  
	(when (> ddebug 1)
	  (format t (tab "size ~a align ~a link ~a~%")
		  (amsize aps)
		  (iarg apb 0)
		  (amlist aps))
	  (do ((sp (amlist aps) (symtab-snaext sp))) ((null sp) nil)
	    (format t (tab "member ~a(~a)~%") (sname sp) sp))))
      #+STABS
      (when gflag
	(stabs_struct (rstack-rsym r) (rstack-ap r)));
      (setf rpole (rstack-rnext r))
      (let ((n (mkty (if (eq (rstack-rsou r) 'STNAME) 'STRTY 'UNIONTY)
		     0
		     (rstack-ap r))))
	
;      n = mkty(r->rsou == STNAME ? STRTY : UNIONTY, 0, r->ap);
;      n->n_sp = r->rsym;

;              n->n_qual |= 1; /* definition place XXX used by attributes */
;        return n;
	(error "Unfinished")
      ))))


;;  * update the offset pointed to by poff; return the
;;  * offset of a value of size _size, alignment _alignment
;;  * given that off is increasing
(defmacro upoff (size alignment poff)
  `(let ((off ,poff))
     (SETOFF off ,alignment)
     (when (< off 0)
       (_cerror "structure or stack overgrown")) ; wrapped
     (setf ,poff (+ off ,size))
     off))

(defun soumemb (n name class)
  "Add a new member to the current struct or union being declared."
  (declare (type (or null node) n)
	   (type string name)
	   (type (or cons symbol) class))
  ;; class = (FIELD . n) | symbol :: n = field size
  (unless rpole
    (_cerror "soumemb"))
  ;; check if tag name exists
  (let (lsp)
    (do ((sp (rstack-rb rpole) (symtab-snext sp))) ((null sp) nil)
      (setf lsp sp)
      (when (and (char/= (aref name 0) #\*) (string= (symtab-sname sp) name))
	(uerror "redeclaration of ~a" name)))
    (let ((sp (getsymtab name 'SMOSNAME)))
      (if (null (rstack-rb rpole))
	  (setf (rstack-rb rpole) sp)
	  (setf (symtab-snext lsp) sp))
      (setf (node-n_sp n) sp
	    (symtab-stype sp) (node-n_type n)
	    (symtab-squal sp) (node-n_qual n)
	    (symtab-slevel sp) blevel
	    (symtab-sap sp) (node-n_ap n)
	    (symtab-sdf sp) (node-n_df n))
      (cond
	((class-fieldp class)
	 (setf (symtab-sclass sp) class)
	 (when (eq (rstack-rsou rpole) 'UNAME)
	   (setf (rstack-rstr rpole) 0))
	 (falloc sp (class-fldsiz class) nil))
	((member (rstack-rsou rpole) '(STNAME UNAME))
	 (setf (symtab-sclass sp)
	       (if (eq (rstack-rsou rpole) 'STNAME)
		   'MOS
		   'MOU))
	 (when (eq (symtab-sclass sp) 'MOU)
	   (setf (rstack-rstr rpole) 0))
	 (let ((al (talign (symtab-stype sp) (symtab-sap sp)))
	       (tsz (tsize (symtab-stype sp) (symtab-sdf sp) (symtab-sap sp))))
	   (setf (symtab-soffset sp) (upoff tsz al (rstack-rstr rpole))))))
      ;;  6.7.2.1 clause 16:
      ;;  "...the last member of a structure with more than one
      ;;   named member may have incomplete array type;"
      (let ((incomp
	     (if (and (ISARY (symtab-stype sp))
		      (eq (symtab-sdf sp) 'NOOFFSET))
		 1
		 0)))
	(when (or (member 'LASTELM (rstack-flags rpole))
		  (and (eq (rstack-rb rpole) sp)
		       (= incomp 1)))
	  (uerror "incomplete array in struct"))
	(when (= incomp 1)
	  (pushnew 'LASTELM (rstack-flags rpole))))
      ;; 6.7.2.1 clause 2:
      ;; "...such a structure shall not be a member of a structure
      ;;  or an element of an array."
      (let ((type (symtab-stype sp)))
	(unless (and (eq (rstack-rsou rpole) 'STNAME)
		     (eq (BTYPE type) 'STRTY))
	  (return-from soumemb))
	(loop
	   (unless (ISARY type)
	     (return))
	   (setf type (DECREF type)))
	(when (ISPTR type)
	  (return-from soumemb)))
      (let ((lsp (strmemb (symtab-sap sp))))
	(when lsp
	  (loop
	     (unless (symtab-snext lsp)
	       (return))
	     (when (and (ISARY (symtab-stype lsp))
			(symtab-snext sp)
			(eq (symtab-sdf sp) 'NOOFFSET))
	       (uerror "incomplete struct in struct"))
	     (setf lsp (symtab-snext lsp))))))))

(defun talign (ty apl)
  "compute the alignment of an object with type ty, sizeoff index s"
  (declare (type stype ty)
	   (type (or null attr) apl))
  (loop
     (unless (stype-mod ty)
       (return))
     (case (car (stype-mod ty))
       ((PTR) (return-from talign 'ALPOINT))
       ((FTN) (_cerror "compiler takes alignment of function")))       
     (setf ty (DECREF ty)))
  
  ;; check for alignment attribute
  (let ((al (attr_find apl 'ATTR_ALIGNED)))
    (when al
      (let ((a (iarg al 0)))
	(when (or (null a) (= a 0))
	  (uerror "no alignment")
	  (setf a 'ALINT))
	(return-from talign a))))
  #-NO_COMPLEX
  (when (ISITY ty)
    (setf ty (make-stype :id (FIMAG->FLOAT (stype-id ty))
			 :mod (stype-mod ty))))

  (case (BTYPE (deunsign ty))
    #+GCC_COMPAT ((VOID) 'ALCHAR)
    ((BOOL) 'ALBOOL)
    ((CHAR) 'ALCHAR)
    ((SHORT) 'ALSHORT)
    ((INT) 'ALINT)
    ((LONG) 'ALLONG)
    ((LONGLONG) 'ALLONGLONG)
    ((FLOAT) 'ALFLOAT)
    ((DOUBLE) 'ALDOUBLE)
    ((LDOUBLE) 'ALLDOUBLE)
    (t (uerror "no alignment")
       'ALINT)))


(defun sztable (x)
  (ecase x
    ((UNDEF) 0)
    ((VOID) SZCHAR)
    ((BOOL) SZBOOL)
    ((CHAR) SZCHAR)
    ((SHORT) SZSHORT)
    ((INT) SZINT)
    ((LONG) SZLONG)
    ((LONGLONG) SZLONGLONG)
    ((FLOAT) SZFLOAT)
    ((DOUBLE) SZDOUBLE)
    ((LDOUBLE) SZLDOUBLE)))
  
;; compute the size associated with type ty,
;;  dimoff d, and sizoff s
;; BETTER NOT BE CALLED WHEN t, d, and s REFER TO A BIT FIELD...
(defun tsize (ty d apl)
  (declare (type stype ty)
	   (type list d)
	   (type (or null attr) apl))
  (let ((mult 1))
    (loop
       (unless (stype-mod ty)
	 (return))
       (case (car (stype-mod ty))
	 ((PTR) (return-from tsize (* mult (SZPOINT ty))))
	 ((FTN) (_cerror "cannot take size of function"))
	 ((ARY)
	  (when (eq (car d) 'NOOFSET) (return-from tsize 0))
	  (when (< (car d) 0) (_cerror "tsize: dynarray"))
	  (setf mult (* mult (car d))
		d (cdr d))))     
       (setf ty (DECREF ty)))
    #-NO_COMPLEX
    (when (ISITY ty)
      (setf ty (make-stype :id (FIMAG->FLOAT (stype-id ty))
			   :mod (stype-mod ty))))
    (* mult
       (case (BTYPE (deunsign ty))
	 ((UNDEF) 0)
	 ((VOID) SZCHAR)
	 ((BOOL) SZBOOL)
	 ((CHAR) SZCHAR)
	 ((SHORT) SZSHORT)
	 ((INT) SZINT)
	 ((LONG) SZLONG)
	 ((LONGLONG) SZLONGLONG)
	 ((FLOAT) SZFLOAT)
	 ((DOUBLE) SZDOUBLE)
	 ((LDOUBLE) SZLDOUBLE)
	 (t (cond
	      ((ISSOU ty)
	       (or (let ((ap (strattr apl)))
		     (when ap
		       (let ((ap2 (attr_find apl 'ATTR_ALIGNED)))
			 (when (and ap2
				    (iarg ap2 0)
				    (/= (iarg ap2 0) 0))
			   (amsize ap)))))
		   (progn
		     (uerror "unknown structure/union/enum")
		     SZINT)))
	      (t (uerror "unknown type")
		 SZINT)))))))

(defun oalloc (p poff)
  "allocate p with offset *poff, and update *poff"
  ;;  Only generate tempnodes if we are optimizing,
  ;; and only for integers, floats or pointers,
  ;; and not if the type on this level is volatile.
  (when (and (/= 0 xtemps)
	     (member (symtab-sclass p) '(AUTO REGISTER))
	     (or (stype> (make-stype :id 'STRTY) (symtab-stype p))
		 (ISPTR (symtab-stype p)))
	     
		   (p->stype < STRTY || ISPTR(p->stype)) &&
		               !(cqual(p->stype, p->squal) & VOL) && cisreg(p->stype))

(defun falloc (p w pty)
  (declare (type (or null symtab) p)
	   (type fixnum w)
	   (type (or null node) pty))
  (let* ((otype (if p (symtab-stype p) (node-n_type pty)))
	 (type otype))
    (declare (type (or null stype) otype))
    (when (eq (stype-id type) 'BOOL)
      (setf (stype-id type) BOOL_TYPE))
    (unless (ISINTEGER type)
      (uerror "illegal field type")
      (setf type (make-stype :id 'INT)))
    (let ((al (talign type nil))
	  (sz (tsize type nil nil)))
      (when (> w sz)
	(uerror "field too big")
	(setf w sz))
      (when (= w 0) ; /* align only */
	(SETOFF (rstack-rstr rpole) al)
	(when p
	  (uerror "zero size field"))
	(return-from falloc 0))
      (when (> (+ (mod (rstack-rstr rpole) al) w) sz)
	(SETOFF (rstack-rstr rpole) al))
      (unless p
	(incf (rstack-rstr rpole) w)  ;  /* we know it will fit */
	(return-from falloc 0))
      
      ;; establish the field
      
      (setf (symtab-soffset p) (rstack-rstr rpole)
	    (rstack-rstr rpole) (+ (rstack-rstr rpole) w)
	    (symtab-stype p) otype)
      (fldty p)
      0)))

(defun getsymtab (name flags)
  (make-symtab :sname name
	       :stype (make-stype :id 'UNDEF)
	       :squal 0
	       :sclass 'SNULL
	       :sflags (make-stype :id flags)
	       :soffset 0
	       :slevel blevel)) 


;; Fetch pointer to first member in a struct list.
(defun strmemb (ap)
  (setf ap (attr_find ap 'ATTR_STRUCT))
  (unless ap
    (_cerror "strmemb"))
  (amlist ap))

