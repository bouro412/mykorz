(in-package mykorz)

;;; define primitive slot

(defmacro define-primitive-f (context name params (ctxt-var)  &body body)
  `(add-slot
    (make-slot :context (make-contexts (list ,@context))
	       :selector ',name
	       :params (make-params ',params ,(empty-env)
				    ,(empty-context))
	       :content 
	       (lambda (args ,ctxt-var)
 		 (destructuring-bind ,params args
		   ,@body)))))

(defmacro define-primitive (context name val)
  `(add-slot 
    (make-slot :context (make-contexts (list ,@context))
	       :selector ',name
	       :params nil
	       :content (lambda (args ctxt)
			  (declare (ignore args ctxt))
			  ,val))))

(defun get-rcvr (ctxt)
  (get-context-val-by-dim-var 'rcvr ctxt))

(defun set-primitive-slot ()
  (init-slot-space)
  (define-primitive-f () print (a) (ctxt)
    (format t "~a~%" (get-value a))
    a)
  (define-primitive-f () princ (a) (ctxt)
    (format t "~a" (get-value a))
    a)
  (define-primitive-f () fresh-line () (ctxt)
    (format t "~%")
    (bool-coord *false*))
  (define-primitive-f () eq (a b) (ctxt)
    (make-bool-coord (eq a b)))
  (define-primitive-f () error (message) (ctxt)
    (error (get-value message)))
  (define-primitive-f () newcoord () (ctxt)
    (make-coord *any*))
  (define-primitive-f () newcoord (parent) (ctxt)
    (make-coord parent))
  (define-primitive-f () copy (coord) (ctxt)
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
  (define-primitive-f (:rcvr *number*) = (a) (ctxt)
    (make-bool-coord (= (get-value (get-rcvr ctxt))
			(get-value a))))
  (define-primitive-f (:rcvr *number*) + (a) (ctxt)
    (number-coord
     (+ (get-value (get-rcvr ctxt))
	(get-value a))))
  (define-primitive-f (:rcvr *number*) - (a) (ctxt)
    (number-coord
     (- (get-value (get-rcvr ctxt))
	(get-value a))))
  (define-primitive-f (:rcvr *number*) * (a) (ctxt)
    (number-coord
     (* (get-value (get-rcvr ctxt))
	(get-value a))))
  (define-primitive-f (:rcvr *number*) / (a) (ctxt)
    (number-coord
     (float (/ (get-value (get-rcvr ctxt))
	(get-value a)))))
  (define-primitive-f (:rcvr *number*) mod (a) (ctxt)
    (number-coord
     (mod (get-value (get-rcvr ctxt))
	(get-value a))))
  (define-primitive-f (:rcvr *number*) < (a) (ctxt)
    (make-bool-coord (< (get-value (get-rcvr ctxt)) 
			(get-value a))))
  (define-primitive-f (:rcvr *number*) > (a) (ctxt)
    (make-bool-coord (> (get-value (get-rcvr ctxt)) 
			(get-value a))))
  (define-primitive-f (:rcvr *number*) <= (a) (ctxt)
    (make-bool-coord
     (<= (get-value (get-rcvr ctxt)) (get-value a))))

  (define-primitive-f (:rcvr *number*) >= (a) (ctxt)
    (make-bool-coord
     (>= (get-value (get-rcvr ctxt)) (get-value a))))

  (define-primitive-f (:rcvr *string*) length () (ctxt)
    (number-coord 
     (length (get-value (get-rcvr ctxt)))))
  (define-primitive-f (:rcvr *string*) elt (a) (ctxt)
    (string-coord
     (string (elt (get-value (get-rcvr ctxt)) (get-value a)))))
  (define-primitive-f (:rcvr *string*) cat (str) (ctxt)
    (string-coord
     (concatenate 'string (get-value (get-rcvr ctxt))
		  (get-value str))))
  )
