(in-package mykorz)

(defun exp-to-list (exp) exp)

;; immediate-exp
(defun immediate-exp (exp) exp)

;; eval-exps
(defun rest-exp-p (exp) (not (null (cdr exp))))

;; id-exp
(defun id-exp-p (exp) 
  (and (symbolp exp)
       (not (keywordp exp))
       (not (eq exp t))
       (not (eq exp nil))))

(defun id-exp (sym env ctxt)
  (cond ((has-binding-p env sym) (apply-env env sym))
	((has-dim-p sym ctxt)
	 (get-context-val-by-dim-var sym ctxt))
	(t (get-var ctxt sym))))

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
  (and (not-id-p exp) (eq (first-exp exp) 'progn)))

(defun progn-exp (exp) (rest-exp exp))

;; method-exp
(defun method-exp-p (exp)
  (and (not-id-p exp) (eq (first-exp exp) 'method)))
(defun method-context (exp) (second-exp exp))
(defun method-id (exp) (third-exp exp))
(defun method-params (exp) (fourth-exp exp))
(defun method-body (exp) (nth-rest-exp 4 exp))

(defun method-exp (ctxt-exp id-exp params-exp body-exp env ctxt)
  (let ((guard-ctxt (create-new-context ctxt-exp env ctxt))
	(selector (if (id-exp-p id-exp) 
		       id-exp
		      (error "method name must be symbol.")))
	(params (make-params params-exp)))
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
  (and (not-id-p exp) (eq (first-exp exp) 'var)))
(defun var-context (exp) (second-exp exp))
(defun var-id (exp) (third-exp exp))
(defun var-value (exp) 
  (cons (fourth-exp exp) (<= 4 (exp-size exp))))
(defun var-exp (ctxt-exp name value-and-existp env ctxt)
  (let ((slot (make-slot :context (create-new-context 
				   ctxt-exp env ctxt)
			 :selector (if (id-exp-p name) 
				       name (error "var name must be symbol"))
			 :params nil))
	(value (car value-and-existp))
	(existp (cdr value-and-existp)))
    (if existp (setf (get-content slot) value))
    (add-slot slot)))

;; def-exp

(defun def-exp-p (exp)
  (and (not-id-p exp) (eq (first-exp exp) 'def)))
(defun def-context (exp) (second-exp exp))
(defun def-id (exp) (third-exp exp))
(defun def-value (exp) (fourth-exp exp))
(defun def-exp (ctxt-exp id value-exp env ctxt)
  (add-slot
   (make-slot :context (create-new-context ctxt-exp env ctxt)
	      :selector (if (id-exp-p id) 
			    id (error "var name must be symbol"))
	      :params nil
	      :content (eval-exps value-exp env ctxt))))
  
;; if-exp
(defun if-exp-p (exp)
  (and (not-id-p exp) (eq (first-exp exp) 'if)))
(defun if-test (exp) (second exp))
(defun if-then (exp) (third exp))
(defun if-else (exp) (fourth exp))
(defun if-exp (test-exp then-exp else-exp env ctxt)
  (if (not (eq (eval-exp test-exp env ctxt) nil))
      (eval-exp then-exp env ctxt)
      (eval-exp else-exp env ctxt)))

;; let-exp
(defun let-exp-p (exp)
  (and (not-id-p exp) (eq (first-exp exp) 'let)))
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
  (and (not-id-p exp) (eq (first-exp exp) 'set)))
(defun set-id (exp)
  (second-exp exp))
(defun set-value (exp)
  (third-exp exp))
(defun set-exp (id-exp value-exp env ctxt)
  (unless (id-exp-p id-exp)
    (error "set symbol must be a symbol"))
  (if (has-binding-p env id-exp)
      (set-env env id-exp)
      (let* ((slotcall (make-call context selector nil))
	     (slots (get-slot slotcall)))
	(cond ((< 1 (length slots))
	       (error "Ambiguous."))
	      ((= 1 (length slots))
	       (setf (get-content (car slot))
		     (eval-exp value-exp env ctxt)))
	      (t (error "A slot call~%  context: ~a~%  selector: ~a~%  args: ~a~%is not unbound." 
			(get-context slotcall)
			(get-selector slotcall)
			(get-args slotcall)))))))

;; quote-exp
(defun quote-exp-p (exp)
  (and (not-id-p exp) (eq (first-exp exp) 'quote)))
(defun quote-exp (exp env ctxt)
  (declare (ignore env ctxt))
  exp)
