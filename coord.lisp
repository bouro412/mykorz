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
  (or (eq coord1 coord2)
      (parent-p (get-parent coord1) coord2)))

(defun make-coord (&optional (parent *any*))
  (make-instance `coordinate :parent parent))
  
