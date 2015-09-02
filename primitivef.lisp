(in-package mykorz)

;;; define primitive slot

(defmacro define-primitive (name params (ctxt-var)  &body body)
  `(add-slot 
    (make-slot :context (make-contexts cl:nil)
	       :selector ',name
	       :params (make-params ',params ,(empty-env)
				    ,(empty-context))
	       :content 
	       (lambda (args ,ctxt-var)
 		 (destructuring-bind ,params args
		   ,@body)))))

(defun set-primitive-slot () 
  (setf *slot-space* cl:nil)
  (define-primitive print (a) (ctxt)
    (format cl:t "~a~%" (get-value a)))
  (define-primitive newcoord () (ctxt)
    (make-coord *any*))
  (define-primitive newcoord (parent) (ctxt)
    (make-coord parent))
  (define-primitive copy (coord) (ctxt)
    (let* ((newcoord (make-coord (get-parent coord)))
	   (slots (search-slots 
		   (lambda (slot)
		     (member coord 
			     (get-context (get-guard slot))
			     :key (lambda (dc) 
				    (get-coord dc))
			     :test #'equal)))))
      (mapc (lambda (old-slot)
	      (add-slot
	       (let ((g (get-guard old-slot)))
		 (make-slot 
		  :selector (get-selector g)
		  :params (get-params g)
		  :context 
		  (list-to-context
		   (mapcar (lambda (dc)
			     (if (equal (get-coord dc) coord)
				 (make-dim-and-coord
				  (get-dim dc)
				  newcoord)
				 dc)) 
			   (context-to-list (get-context g))))
		  :content
		  (if (slot-boundp old-slot 'init-exp)
		      (let ((v (eval-exp (get-exp old-slot)
				 (empty-env) ctxt)))
			(lambda (args ctxt)
			  (declare (ignore args ctxt))
			  v))
		      (get-content old-slot))))))
	    slots)
      newcoord))
  (define-primitive = (c1 c2) (ctxt)
    (if (equal c1 c2) *true* *false*))
)
