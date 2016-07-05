(in-package #:mykorz)

(defun korz-test (src)
  (init-env)
  (eval-top-exp src (empty-env) (make-contexts nil)))

(defmacro korz-tests (&rest src)
  `(progn
     (init-env)
     ,@(mapcar (lambda (src)
		 `(eval-top-exp ',src (empty-env) (empty-context)))
	       src)))

(defun korz-repl ()
  (init-env)
  (loop (princ "> ")
	(print (eval-top-exp (read) 
			     (empty-env) 
			     (make-contexts nil)))
	(fresh-line)))

(defmacro run-korz (&rest src)
  `(progn 
     ,@(mapcar (lambda (src)
		 `(eval-top-exp ',src (empty-env) 
				(empty-context)))
	       src)))

(defun eval-top-exp (exp env ctxt)
  (cond ((progn-exp-p exp) (eval-exps (progn-exp exp)
				      env ctxt))
	((method-exp-p exp)
	 (method-exp (method-context exp)
		     (method-id exp)
		     (method-params exp)
		     (method-body exp)
		     env ctxt))
	((var-exp-p exp)
	 (var-exp (var-context exp)
		  (var-id exp)
		  (var-value exp)
		  env ctxt))
	((def-exp-p exp)
	 (def-exp (def-context exp)
	          (def-id exp)
	          (def-value exp)
		  env ctxt))
#|
	;predicate
	((predicate-exp-p exp)
	 (predicate-exp (pre-context exp)
			(pre-id exp)
			(pre-params exp)
			(pre-body exp)
			env ctxt))
|#
	((if-exp-p exp)
	 (if-exp (if-test exp)
		 (if-then exp)
		 (if-else exp)
		 env ctxt))
	((let-exp-p exp)
	 (let-exp (let-var exp)
		  (let-body exp)
		  env ctxt))
	((set-exp-p exp)
	 (set-exp (set-place exp)
		  (set-value exp)
		  env ctxt))
	;; set-dimension-priority
	((sdp-exp-p exp)
	 (sdp-exp (sdp-dimension exp)
		  (sdp-value exp)
		  env ctxt))
	((call-exp-p exp)
	 (call-exp (call-exp-function exp)
		   (call-exp-args exp)
		   (call-exp-context exp)
		   env ctxt))
	((id-exp-p exp) (id-exp exp env ctxt))
	(t (immediate-exp exp))))

;;eval-exp
;;exp = S expression
;;(exp env ctxt) -> coordinate
;; *-exp-p : exp -> bool
;; *-exp : ([exp] env ctxt) -> coordinate
(defun eval-exp (exp env ctxt)
  (cond ((or (def-exp-p exp)
	     (var-exp-p exp)
	     (method-exp-p exp))
	 (error "slot can't be difined in top level."))
	((progn-exp-p exp) (eval-exps (progn-exp exp)
				      env ctxt))
	((if-exp-p exp)
	 (if-exp (if-test exp)
		 (if-then exp)
		 (if-else exp)
		 env ctxt))
	((let-exp-p exp)
	 (let-exp (let-var exp)
		  (let-body exp)
		  env ctxt))
	((set-exp-p exp)
	 (set-exp (set-place exp)
		  (set-value exp)
		  env ctxt))

	
	((proceed-exp-p exp)
	 (proceed-exp (proceed-args exp)
		      (proceed-context exp)
		      env ctxt))
	((call-exp-p exp)
	 (call-exp (call-exp-function exp)
		   (call-exp-args exp)
		   (call-exp-context exp)
		   env ctxt))
	((id-exp-p exp) (id-exp exp env ctxt))
	(t (immediate-exp exp))))

;;eval-exps : ([exp] env ctxt) -> coordinate

(defun eval-exps (exp env ctxt)
  (if (rest-exp-p exp)
      (progn (eval-exp (first-exp exp) env ctxt)
	     (eval-exps (rest-exp exp) env ctxt))
      (eval-exp (first-exp exp) env ctxt)))
