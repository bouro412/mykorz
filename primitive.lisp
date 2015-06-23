(in-package mykorz)

;;; define primitive slot

(defmacro define-primitive (name params &body body)
  `(add-slot 
    (make-slot :context (make-contexts nil)
	       :selector ',name
	       :params (make-params ',params)
	       :content 
	       (lambda (args ctxt)
		 (declare (ignore ctxt))
		 (destructuring-bind ,params args
		   ,@body)))))

(defun set-primitive-slot ()
  (setf *slot-space* nil)
  (define-primitive print (a)
    (print a))
  (define-primitive newcoord (&optional (val *any*))
    (make-coord val)))


