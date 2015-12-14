
(in-package #:mykorz)

;; parent relation : (list (child . parent))
; child : Coordinate
; parent : Coordinate

(defclass coordinate ()
  ((parent :initarg :parent :accessor get-parent)))

(defvar *any* (make-instance 'coordinate :parent nil))

;(defmacro child (relation)
;  `(car ,relation))

;(defmacro parent (relation)
;  `(cdr ,relation))

;(defmacro make-relation (child parent)
;  `(cons ,child ,parent))

;(defun get-parent (coord)
;  (parent (assoc coord *parent-relation*)))

(defun parent-p (coord1 coord2)
  (and coord1
       (or (coord= coord1 coord2)
	   (parent-p (get-parent coord1) coord2))))
(defun coord< (coord1 coord2)
  (and (not (coord= coord1 coord2))
       (parent-p coord2 coord1)))

(defun make-coord (&optional (parent *any*))
  (make-instance `coordinate :parent parent))

(defclass primitive-coordinate (coordinate)
  ((parent :initform *any*)
   (value :initarg :value :accessor get-value)))

(defmethod print-object ((coord primitive-coordinate)
			 stream)
  (format stream "<coord ~s>" (get-value coord)))


(defmethod coord= (c1 c2)
  (eq c1 c2))

(defmethod coord= ((c1 primitive-coordinate)
		   (c2 primitive-coordinate))
  (equal (get-value c1) (get-value c2)))

(defvar *string* (make-instance 'primitive-coordinate
				:value ""))
(defvar *number* (make-instance 'primitive-coordinate
				:value 0))
(defvar *bool* (make-instance 'primitive-coordinate
			      :value *true*))

(defun string-coord (str)
  (make-instance 'primitive-coordinate :parent *string*
		 :value str))
(defun number-coord (num)
  (make-instance 'primitive-coordinate :parent *number*
		 :value num))
(defun bool-coord (bool)
  (make-instance 'primitive-coordinate :parent *number*
		 :value bool))
(defun make-bool-coord (cl-bool)
  (bool-coord (if cl-bool *true* *false*)))
