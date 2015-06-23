(in-package #:mykorz)

(defun get-slot (slotcall)
  (let ((slots (get-matched-slots slotcall)))
    (dolist (s slots)
      (dolist (s_dash slots)
	(if (and (not (equal s s_dash))
		 (slot<= s s_dash))
	    (setf slots (remove s_dash slots :test #'equal)))))))

(defun get-var (context selector)
  (let* ((slotcall (make-call context selector nil))
	 (slots (get-slot slotcall)))
    (cond ((< 1 (length slots))
	   (error "Ambiguous."))
	  ((= 1 (length slots))
	   (get-content (car slots)))
	  (t (error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
	     (get-context slotcall)
	     (get-selector slotcall)
	     (get-args slotcall))))))

(defun get-method (context selector args)
  (let* ((slotcall (make-call context selector args))
	 (slots (get-slot slotcall)))
    (cond ((< 1 (length slots))
	   (error "Ambiguous."))
	  ((= 1 (length slots))
	   (get-content (car slots)))
	  (t (error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
	     (get-context slotcall)
	     (get-selector slotcall)
	     (get-args slotcall))))))
  

(defun get-matched-slots (slotcall)
  (search-slots (lambda (slot)
		  (and (match-context slot slotcall)
		       (match-selector slot slotcall)
		       (match-params slot slotcall)))))

(defun match-context (slot slotcall)
  (every (lambda (dc)
	   (anaphora:aand
	    (search-context (get-dim dc)
			    (get-context slotcall))
	    (parent-p (get-coord anaphora:it) 
		    (get-coord dc))))
	 (context-to-list (get-context (get-guard slot)))))

(defun match-selector (slot slotcall)
  (eq (get-selector slotcall)
      (get-selector (get-guard slot))))

(defun match-params (slot slotcall)
  (let ((params (get-params (get-guard slot)))
	(args (get-args slotcall)))
    (and (= (params-num params) (args-num args))
	 (every #'argmatch (args-to-list args)
		(params-to-list  params)))))

(defun argmatch (arg param)
  (or (equal (param-type param) *any*)
      (parent-p arg (param-type param))))

(defun slot<= (slot1 slot2)
  (let ((sg1 (get-guard slot1))
	(sg2 (get-guard slot2)))
    (and (context< (get-context sg1) (get-context sg2))
	 (eq (get-selector sg1) (get-selector sg2))
	 (params< (get-params sg1) (get-params sg2)))))

(defun context< (ctx1 ctx2)
  (or (and (> (context-size ctx1) (context-size ctx2))
	   (every (lambda (context)
		    (search-context (get-dim context) ctx1))
		  (context-to-list ctx2)))
      (and (= (context-size ctx1) (context-size ctx2))
	   (every (lambda (context)
		    (anaphora:aand 
		     (search-context (get-dim context) ctx2)
		     (parent-p (get-coord context)
			       (get-coord anaphora:it))))
		  (context-to-list ctx1)))))
  
(defun params< (params1 params2)
;  (or (equal params1 params2)
      (and (= (params-num params1) (params-num params2))
	   (every (lambda (x y)
		    (parent-p (param-type x) (param-type y)))
		  (params-to-list params1)
		  (params-to-list params2))))


