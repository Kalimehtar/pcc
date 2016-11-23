(in-package #:pcc)

;(defstruct (tree (:copier nil)) bitno lr)

(defstruct sstype id mod)

(defvar symtreecnt 0)

(defvar tmpsyms (make-hash-table))
(defvar numsyms (make-hash-table))
(defvar sympole (make-hash-table))
(defvar firstname)
(defvar nametabs 0)
(defvar namestrlen 0)

(defun addname (key)
  (symtab_add key firstname nametabs namestrlen))



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
	      new)))))
	 
       
	     
     


;; Inserts a symbol into the symbol tree.
;; Returns a struct symtab.
(defun lookup (key stype)
  (let* ((type (sstype-id stype))
	 (uselvl (and (> blevel 0) (not (eq type 'SSTRING))))
	 w svbit)
    (when (> blevel 0)
      (do ((sym (gethash type tmpsyms) (symtab-next sym))) ((null sym) nil)
	(when (string= (symtab-sname sym) key)
	  (return sym))))
    (case (gethash type numsyms 0)
      ((0)
       (when (member 'SNOCREAT (sstype-mod stype))
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
       (setf w (gethash type sympole)
	     svbit 0)) ; /* XXX why? */
      (t
       (setf w (gethash key (gethash type sympole)))))
    (cond
      (w w)
      ((member 'SNOCREAT (sstype-mod stype)) nil)
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
	       
	   
       
	 
      

