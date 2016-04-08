(in-package #:mykorz)

(defparameter *slot-space* (make-hash-table))
(defun init-slot-space ()
  (clrhash *slot-space*))
(defun ss->list ()
  (iter (for (selector 4s) in-hashtable *slot-space*)
	(appending (get-slots 4s))))

(defconstant *true* 'true)
(defconstant *false* 'false)
(defparameter *dimension-priority* (make-hash-table))
(defvar *proceed-info* (cons nil nil))

(defun get-dim-pri (dimension)
  (gethash dimension *dimension-priority* 0))

(defsetf get-dim-pri (dim) (value)
  `(setf (gethash ,dim *dimension-priority*) ,value))

(defun sort-dimension (dim-list)
  (sort dim-list #'> :key #'get-dim-pri))
