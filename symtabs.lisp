(defpackage #:pcc.symtabs
  (:use #:cl #:pcc.pass1)
  (:shadow
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
   ;;   #:symtabscnt
   #:symtreecnt))

(in-package #:pcc.symtabs)

;(defstruct (tree (:copier nil)) bitno lr)

(defvar symtreecnt 0)

(defvar tmpsyms (make-hash-table))
(defvar numsyms (make-hash-table))
(defvar sympole (make-hash-table))
(defvar firstname)
(defvar nametabs 0)
(defvar strtabs 0)
(defvar namestrlen 0)

;;* Add a name to the name stack (if its non-existing),
;;* return its address.
;;* This is a simple patricia implementation.
(defmacro symtab_add (key first tabs stlen)
  `(block nil
     (let (m (len (length ,key)))
       (case ,tabs
	 ((0)
	  (setf ,first (newstring ,key))
	  (setf ,stlen (1+ len))
	  (incf ,tabs)
	  (return ,first))
	 ((1)
	  (setf m ,first))		      
	 (t
	  (setf m (gethash ,key ,first))))
       (cond
	 ((and m (string= m key))
	  m)
	 (t (let ((new (newstring ,key)))
	      (when (= ,tabs 1)
		(setf ,first (make-hash-table :test 'equal))
		(setf (gethash m ,first) m))
	      (incf ,stlen (1+ len))
	      (setf (gethash new ,first) new)
	      new))))))

(defun addname (key)
  (symtab_add key firstname nametabs namestrlen))

;; Inserts a symbol into the symbol tree.
;; Returns a struct symtab.
(defun lookup (key stype)
  (declare (type string key)
	   (type stype stype))
  (let* ((type (stype-id stype))
	 (uselvl (and (> blevel 0) (not (eq type 'SSTRING))))
	 w)
    (when (> blevel 0)
      (do ((sym (gethash type tmpsyms) (symtab-snext sym))) ((null sym) nil)
	(when (string= (symtab-sname sym) key)
	  (return sym))))
    (case (gethash type numsyms 0)
      ((0)
       (when (member 'SNOCREAT (stype-mod stype))
	 (return-from lookup))
       (when uselvl
	 (let ((sym (getsymtab key type)))
	   (setf (symtab-snext sym) (gethash type tmpsyms)
		 (gethash type tmpsyms) sym)
	   (return-from lookup sym)))
       (setf (gethash type sympole) (getsymtab key stype))
       (incf (gethash type numsyms))
       (return-from lookup (gethash type sympole)))
      ((1)
       (setf w (gethash type sympole)))
      (t
       (setf w (gethash key (gethash type sympole)))))
    (cond
      (w w)
      ((member 'SNOCREAT (stype-mod stype)) nil)
      (uselvl
       ;; Insert into the linked list, if feasible
       (let ((sym (getsymtab key type)))
	 (setf (symtab-snext sym) (gethash type tmpsyms)
	       (gethash type tmpsyms) sym)
	 sym))
      (t
       ;;* Need a new node.
       (incf symtreecnt)
       (let ((sym (getsymtab key type)))
	 (if (= (incf (gethash type numsyms 0)) 1)
	     (setf (gethash type sympole) sym)
	     (let ((w (make-hash-table)))
	       (setf (gethash key w) sym
		     (gethash type sympole) w)))
	 sym)))))
	       
	   
(defun hide (sym)
  (let* ((typ (stype-id (symtab-sflags sym)))
	 (new (getsymtab (symtab-sname sym)
			 (make-stype :id typ :mod '(STEMP)))))
    (setf (symtab-snext new) (gethash typ tmpsyms)
	  (gethash typ tmpsyms) new)
    #+PCC_DEBUG
    (when pdebug
      (format t (tab "~a hidden at level ~a (~a -> ~a)~%")
	      sym->sname blevel sym new))
    new))
			
	 
      

