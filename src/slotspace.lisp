(in-package #:mykorz)

(defparameter *slot-space* (make-hash-table))
(defun init-slot-space ()
  (clrhash *slot-space*))
(defun ss->list ()
  (iter (for (selector 4s) in-hashtable *slot-space*)
	(appending (get-slots 4s))))

(defconstant *true* 'true)
(defconstant *false* 'false)







