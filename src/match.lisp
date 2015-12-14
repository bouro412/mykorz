(in-package #:mykorz)

(defun get-slot (slotcall)
  (sort (get-matched-slots slotcall) #'new-slot<=))
    

(defun get-var (context selector)
  (let* ((slotcall (make-call context selector nil))
	 (slots (get-slot slotcall)))
    (if slots 
	(get-content (car slots))
	(error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
	       (get-context slotcall)
	       (get-selector slotcall)
	       (get-args slotcall)))))
#|
    (cond ((< 1 (length slots))
	   (error "Ambiguous."))
	  ((= 1 (length slots))
	   (get-content (car slots)))
	  (t (error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
	     (get-context slotcall)
	     (get-selector slotcall)
	     (get-args slotcall))))))
|#

(defun get-method (context selector args)
  (let* ((slotcall (make-call context selector args))
	 (slots (get-slot slotcall)))
    (or slots 
	(error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
	       (get-context slotcall)
	       (get-selector slotcall)
	       (get-args slotcall)))))
#|
    (cond ((< 1 (length slots))
	   (error "Ambiguous."))
	  ((= 1 (length slots))
	   (get-content (car slots)))
	  (t (error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
	     (get-context slotcall)
	     (get-selector slotcall)
	     (get-args slotcall))))))
|#

(defun get-matched-slots (slotcall)
  (search-4s-slots (get-selector slotcall)
		   (lambda (slot)
		     (and (match-context slot slotcall)
			  (match-params slot slotcall)))))

(defun match-context (slot slotcall)
  (every (lambda (dc)
	   (anaphora:aand
	    (search-context (get-dim dc)
			    (get-context slotcall))
	    (parent-p (get-coord anaphora:it)
		      (get-coord dc))))
	 (context-to-list (get-context (get-guard slot)))))
"""
slot(ctxt: ((RCVR . #<coordinate :parent #<coordinate :parent #<coordinate :parent NIL>>>)), selector: CDR, params: NIL)
        #<slot-call :ctxt ((RCVR
                            . #<coordinate :parent #<coordinate :parent #<coordinate :parent NIL>>>)) :selector CAR :args NIL>)
"""

(defun match-selector (slot slotcall)
  (eq (make-keyword (get-selector slotcall))
      (make-keyword (get-selector (get-guard slot)))))

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
(defun new-slot<= (slot1 slot2)
  (cond ((new-slot<=% slot1 slot2) t)
	((new-slot<=% slot2 slot1) nil)
	(t (error "Ambiguous."))))

(defun new-slot<=% (slot1 slot2)
  (or (slot<= slot1 slot2)
      (let* ((sg1 (get-guard slot1))
	     (sg2 (get-guard slot2))
	     (dim-list (union (dimension-list sg1)
			      (dimension-list sg2)))
	     (dim-pri (remove-if-not 
		       (lambda (x) (member x dim-list))
		       *dimention-priority*)))
	(or (some (lambda (dim)
		    (let ((coord1 (get-context-by-dim 
				   dim (get-context sg1)))
			  (coord2 (get-context-by-dim 
				   dim (get-context sg2))))
		      (coord< coord1 coord2)))
		  dim-pri)
	    (some (lambda (p1 p2)
		    (coord< (param-type p1)
			    (param-type p2)))
		  (params-to-list (get-params sg1)) 
		  (params-to-list (get-params sg2)))))))

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


