(defpackage #:pcc.mip-node
  (:use #:cl)
  (:export
   #:make-attr
   #:attr-next
   #:attr-atype
   #:attr-aa
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
   #:node-n_dcon
   #:getlval
   #:setlval
   ))
  

(in-package #:pcc.node)

(deftype aarg () t)
#|
union aarg {
        int iarg;
        char *sarg;
        void *varg;
};
|#

(defstruct attr
  (next nil :type (or null attr))
  (atype 0 :type fixnum)
  (aa #() :type (vector aarg)))

;; sz dropped. aa has vecttor size inside
#|
struct attr {
        struct attr *next;
        unsigned int atype:12, sz:2;
        union aarg aa[];
};
|#

(defstruct n_u n_l n_r)
(defstruct n_f (n_u (make-n_u) :type n_u) _dcon)

(defstruct node
  (n_op 'UNDEF :type symbol)
  n_3
  (n_type 0 :type fixnum)
  (n_qual 0 :type fixnum)
  (n_su 0 :type fixnum)
  n_5
  n_ap
  (n_f (make-n_f) :type n_f))

(defun node-n_reg (n)
  (node-n_3 n))

(defun (setf node-n_reg) (val n)
  (setf (node-n_3 n) val))

(defun node-n_regw (n)
  (node-n_3 n))

(defun (setf node-n_regw) (val n)
  (setf (node-n_3 n) val))

(defun node-n_name (n)
  (node-n_5 n))

(defun (setf node-n_name) (val n)
  (setf (node-n_5 n) val))

(defun node-n_df (n)
  (node-n_5 n))

(defun (setf node-n_df) (val n)
  (setf (node-n_5 n) val))

(defun node-n_label (n)
  (node-n_5 n))

(defun (setf node-n_label) (val n)
  (setf (node-n_5 n) val))

(defun node-n_left (n)
  (n_u-n_l (n_f-n_u (node-n_f n))))

(defun (setf node-n_left) (val n)
  (setf (n_u-n_l (n_f-n_u (node-n_f n))) val))

(defun node-n_val (n)
  (n_u-n_l (n_f-n_u (node-n_f n))))

(defun (setf node-n_val) (val n)
  (setf (n_u-n_l (n_f-n_u (node-n_f n))) val))

(defun node-n_slval (n)
  (n_u-n_l (n_f-n_u (node-n_f n))))

(defun (setf node-n_slval) (val n)
  (setf (n_u-n_l (n_f-n_u (node-n_f n))) val))

(defun node-n_right (n)
  (n_u-n_r (n_f-n_u (node-n_f n))))

(defun (setf node-n_right) (val n)
  (setf (n_u-n_r (n_f-n_u (node-n_f n))) val))

(defun node-n_rval (n)
  (n_u-n_r (n_f-n_u (node-n_f n))))

(defun (setf node-n_rval) (val n)
  (setf (n_u-n_r (n_f-n_u (node-n_f n))) val))

(defun node-n_sp (n)
  (n_u-n_r (n_f-n_u (node-n_f n))))

(defun (setf node-n_sp) (val n)
  (setf (n_u-n_r (n_f-n_u (node-n_f n))) val))

(defun node-n_dcon (n)
  (n_f-_dcon (node-n_f n)))

(defun (setf node-n_dcon) (val n)
  (setf (n_f-_dcon (node-n_f n)) val))

(defun getlval (p)
  (n_u-n_l (n_f-n_u (node-n_f p))))

(defmacro setlval (p v)
  `(setf (n_u-n_l (n_f-n_u (node-n_f ,p))) ,v))
