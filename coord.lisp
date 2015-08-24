
(in-package #:mykorz)

;; parent relation : (list (child . parent))
; child : Coordinate
; parent : Coordinate

(defclass coordinate ()
  ((parent :initarg :parent :accessor get-parent)))

(defvar *any* (make-instance 'coordinate :parent cl:nil))

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
       (or (eq coord1 coord2)
	   (parent-p (get-parent coord1) coord2))))

(defun make-coord (&optional (parent *any*))
  (make-instance `coordinate :parent parent))

(defclass primitive-coordinate (coordinate)
  ((parent :initform *any*)
   (value :initarg :value :accessor get-value)))

(defmethod print-object ((coord primitive-coordinate)
			 stream)
  (format stream "<coord ~s>" (get-value coord)))


;; next
(defmethod coord= (c1 c2)
  (eq c1 c2))

(defmethod coord= ((c1 primitive-coordinate)
		   (c2 primitive-coordinate))
  (equal (get-value c1) (get-value c2)))

(defclass string-coordinate (primitive-coordinate)())
(defvar *string* (make-instance 'string-coordinate
				:value ""))

(defun string-coord (str)
  (make-instance 'string-coordinate :parent *string*
		 :value str))

;;not use yet
(defclass number-coordinate (primitive-coordinate)()) 
(defclass bool-coordinate (primitive-coordinate) ())
  

