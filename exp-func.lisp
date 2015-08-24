(in-package mykorz)

(defun exp-to-list (exp) exp)

;; immediate-exp
(defun immediate-exp (exp)
  (cond ((and 
	  (not (typep exp 'double-float))
	  (or (typep exp 'fixnum)
	      (typep exp 'float)))
	 exp)
	((eq exp *true*)
	 exp)
	((eq exp *false*)
	 exp)
	((stringp exp) (string-coord exp))
	(cl:t (error "not exist such a value: ~a~%" exp))))

;; eval-exps
(defun rest-exp-p (exp) (not (null (cdr exp))))

;; id-exp
(defun id-exp-p (exp)
  (and (symbolp exp)
       (not (keywordp exp))
       (not (eq exp *true*))
       (not (eq exp *false*))))

(defun id-exp (sym env ctxt)
  (cond ((has-binding-p env sym) (apply-env env sym))
	((has-dim-p sym ctxt)
	 (get-context-val-by-dim-var sym ctxt))
	(cl:t (error "variable ~a is unbound." sym))))

;; call-exp
(defun call-exp-p (exp) (consp exp))
(defun call-exp-function (exp) (first-exp exp))
(defun call-exp-args (exp) (exp->args-exp-list exp))
(defun call-exp-context (exp) (exp->context-exp-list exp))
(defun call-exp (f-exp args-exp ctxt-exp env ctxt)
  (let* ((args (list-to-args
		(mapcar #'(lambda (e)
			    (eval-exp e env ctxt))
			args-exp)))
	 (newctxt (create-new-context ctxt-exp env ctxt))
	 (fun (if (id-exp-p f-exp)
		  (get-method newctxt f-exp args)
		  (eval-exp f-exp ctxt env))))
    (funcall fun args newctxt)))

;; progn-exp
(defun progn-exp-p (exp)
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :progn)))

(defun progn-exp (exp) (rest-exp exp))

;; method-exp
(defun method-exp-p (exp)
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :method)))
(defun method-context (exp) (second-exp exp))
(defun method-id (exp) (third-exp exp))
(defun method-params (exp) (fourth-exp exp))
(defun method-body (exp) (nth-rest-exp 4 exp))
(defun make-params (params env ctxt)
  (list-to-params
   (mapcar (lambda (param)
	     (if (paramp param)
		 (make-param (param-symbol param)
			     (eval-exp (param-type param)
				       env ctxt))
		 (make-param param))) 
	   (params-to-list params))))

(defun method-exp (ctxt-exp id-exp params-exp body-exp env ctxt)
  (let ((guard-ctxt (create-new-context ctxt-exp env ctxt))
	(selector (if (id-exp-p id-exp) 
		       id-exp
		      (error "method name must be symbol.")))
	(params (make-params params-exp env ctxt)))
    (add-slot
     (make-slot :context guard-ctxt
		:selector selector
		:params params
		:content (lambda (args ctxt)
			   (let ((env (extend-envs 
				       (params-symbol params)
				       args
				       env)))
			     (eval-exps body-exp env ctxt)))))))
  
;; var-exp
(defun var-exp-p (exp) 
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :var)))
(defun var-context (exp) (second-exp exp))
(defun var-id (exp) (third-exp exp))
(defun var-value (exp) 
  (cons (fourth-exp exp) (<= 4 (exp-size exp))))
(defun var-exp (ctxt-exp name value-and-existp env ctxt)
  (let ((slot (make-slot :context (create-new-context 
				   ctxt-exp env ctxt)
			 :selector (if (id-exp-p name) 
				       name (error "var name must be symbol"))
			 :params cl:nil))
	(value-exp (car value-and-existp))
	(existp (cdr value-and-existp)))
    (if existp 
	(let ((value (eval-exp value-exp env ctxt)))
	  (setf (get-exp slot) value-exp
		(get-content slot) (lambda (args ctxt)
				     (declare (ignore args ctxt))
				     value))))
    (add-slot slot)))

;; def-exp

(defun def-exp-p (exp)
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :def)))
(defun def-context (exp) (second-exp exp))
(defun def-id (exp) (third-exp exp))
(defun def-value (exp) (fourth-exp exp))
(defun def-exp (ctxt-exp id value-exp env ctxt)
  (let ((value (eval-exp value-exp env ctxt)))
    (add-slot
     (make-slot :context (create-new-context ctxt-exp env ctxt)
		:selector (if (id-exp-p id) 
			      id (error "var name must be symbol"))
		:params cl:nil
		:content (lambda (args ctxt) 
			   (declare (ignore args ctxt))
			   value)))))
    
;; if-exp
(defun if-exp-p (exp)
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :if)))
(defun if-test (exp) (second exp))
(defun if-then (exp) (third exp))
(defun if-else (exp) (fourth exp))
(defun if-exp (test-exp then-exp else-exp env ctxt)
  (cond ((eq (eval-exp test-exp env ctxt) *true*)
	 (eval-exp then-exp env ctxt))
	((eq (eval-exp test-exp env ctxt) *false*)
	 (if (eq else-exp  cl:nil) *false*
	     (eval-exp else-exp env ctxt)))
	(cl:t (error "if test value must be true or false."))))

;; let-exp
(defun let-exp-p (exp)
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :let)))
(defun let-var (exp) (second-exp exp))
(defun let-body (exp) (nth-rest-exp 2 exp))
(defun let-exp (var-exp body-exp env ctxt)
  (let ((env (extend-envs 
	      (mapcar #'first (exp-to-list
			       var-exp))
	      (mapcar (compose 
		       (rcurry #'eval-exp env ctxt)
			       #'second)
		      (exp-to-list var-exp))
	      env)))
    (eval-exps body-exp env ctxt)))

;; set-exp
(defun set-exp-p (exp)
  (and (not-id-p exp) (eq (make-keyword
			   (first-exp exp)) :set)))
(defun set-place (exp)
  (second-exp exp))
(defun set-value (exp)
  (third-exp exp))
(defun set-exp (place-exp value-exp env ctxt)
  (if (consp place-exp)
      (let* ((newctxt (create-new-context 
		       (exp->context-exp-list place-exp)
		       env ctxt))
	     (slots (get-slot 
		    (make-call  newctxt
				(first-exp place-exp)
				(empty-args)))))
	(cond ((< 1 (length slots))
	       (error "Ambiguous."))
	      ((= 1 (length slots))
	       (let ((v (eval-exp value-exp env ctxt)))
		 (setf (get-content (car slots))
		       (lambda (args ctxt)
			 (declare (ignore args ctxt))
			 v))))
	      (cl:t (error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
			   newctxt
			   (first-exp place-exp)
			   (empty-args)))))
      (if (has-binding-p env place-exp)
	  (set-env env place-exp 
		   (eval-exp value-exp env ctxt))
	  (error "variable ~a is unbound." place-exp))))
