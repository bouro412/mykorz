;;;; mykorz.lisp

(in-package #:mykorz)

;;; "mykorz" goes here. Hacks and glory await!

(defparameter *current-context* nil)

(defparameter *primitive-slots* (make-hash-table))

(defparameter *slot-space* nil)

(defvar *any* (gensym "ANY"))

(defparameter *parent-relation* `((,*any* . nil)))

(defparameter *dimension-list* nil)


(decorator:enable-decorator)


;;;----------------------------------------------------
;;slot implements
;;; SS
;; set of Coordinate  == default symbol
;; parent relation == ((p . c) ...)
;; dimension name == default symbol
;; selectors == default symbol
;; slots == (list (slotguard . value))

;slotguard == (context selector args)
;context == (list (dim . coord))
;selector == symbol
;args == (list coord)

(defun push-slot (slot)
  (pushnew slot *slot-space* :key #'car :test #'equal))

(defun match-context (slotcall slotguard)
  (destructuring-bind (context _1 _2 _3) slotguard
    (declare (ignore _1 _2 _3))
    (every (lambda (cont)
	     (anaphora:aand
	      (assoc (car cont) (car slotcall))
	      (parent-p (cdr anaphora:it) (cdr cont))))
	   context)))

(defun match-args (slotcall slotguard)
  (let ((callargs (third slotcall))
	(guardargs (third slotguard)))
    (and (= (length callargs) (length guardargs))
	 (every #'argmatch callargs guardargs))))

(defun argmatch (callarg guardarg)
  (or (eq (second guardarg) *any*)
      (parent-p callarg guardarg)
      (typep callarg guardarg)))

(defun get-matched-slots (slotcall)
  (remove-if-not (lambda (slot) 
		   (and (match-context slotcall (car slot))
			(match-args slotcall (car slot))))
		 (get-by-selector (second slotcall))))

(defun get-by-selector (selector)
  (remove-if (lambda (slot) 
	       (not (eq selector (cadar slot)))) 
	     *slot-space*))

(defun slot< (slot1 slot2)
  (and (dimension< (car slot1) (car slot2))
       (args< (car slot1) (car slot2))
       (eq (cadar slot1) (cadar slot2))))

(defun dimension< (sg1 sg2)
  (let ((dcs1 (car sg1))
	(dcs2 (car sg2)))
    (or (and (> (length dcs1) (length dcs2))
	     (every (lambda (dimcoord)
		      (member (car dimcoord) dcs1 
			      :key #'car))  dcs2))
	(and (= (length dcs1) (length dcs2) )
	     (every (lambda (dc1)
		      (anaphora:aand 
		       (assoc (car dc1)  dcs2)
		       (parent-p (cdr dc1) (cdr anaphora:it)
))) 
		    dcs1)))))

(defun args< (sg1 sg2)
  (let ((args1 (third sg1))
	(args2 (third sg2)))
    (or (eq args1 args2)
	(and (= (length args1) (length args2))
	     (every (lambda (x y)
		      (or (eq x y) (parent-p x y))) sg1 sg2)))))


(defun get-slot (slotcall)
  (let ((slots (get-matched-slots slotcall)))
    (when (null slots)
      (error "slot ~a is not unbound." slotcall))
    (reduce #'(lambda (sl1 sl2)
		(cond ((null sl1) sl2)
		      ((slot< sl1 sl2) sl1)
		      ((slot< sl2 sl1) sl2)
		      (t (error "can't decide slot~%called by ~a~%~a~%~a~%" slotcall sl1 sl2))))
	    slots :initial-value nil)))

(defsetf get-slot (slotcall) (value)
  `(progn
     (when (eq (fourth (car (get-slot ,slotcall))) :def)
       (error "Can't rewrite constant value."))
     (setf (cdr (nth (position (get-slot ,slotcall) 
			       *slot-space* 
			       :test #'equal) 
		     *slot-space*))
	   ,value)))
  
(defun parent (name)
  (cdr (assoc name *parent-relation*)))

(defun parent-p (coord1 coord2)
  (or (eq coord1 coord2)
      (member coord2 (ancestor coord1))))

(defun children (name)
  (mapcar #'car 
	  (remove-if (lambda (xs) (not (eql (cdr xs) name)))
		     *parent-relation*)))

(defun ancestor (name)
  (labels ((run (n acc)
	     (if n
		 (run (parent n) (cons n acc))
		 acc)))
    (run (parent name) nil)))

(defun push-coord (newcoord parent)
  (push (cons newcoord parent) *parent-relation*))

(defun get-var (selector)
  (cdr (get-slot (list *current-context* selector nil))))

(defun get-method (selector args)
  (cdr (get-slot (list *current-context* 
		       selector
		       args))))

;;(:dim coord ...) -> ((:dim . coord) ...)
(defun read-context (context)
  (labels ((tag (list acc)
	     (cond ((null list) acc)
		   ((null (cdr list)) 
		    (error "~a dimension have no coordinate." (car list)))
		   ((keywordp (car list))
		    (pushnew (car list) *dimension-list*)
		    (tag (cddr list)
			 (cons (cons 
				(first list) 
				(korz-eval (second list)))
			       acc)))
		   (t (error "~a is not a keyword symbol. dimension must be a keyword." (car list))))))
    (tag context nil)))

;;;----------------------------------------------------
;;primitive definition

(defmacro define-primitive (selector parms &body body)
  `(setf (gethash ',selector *primitive-slots*)
	 (lambda ,parms ,@body)))

(define-primitive if (test then else)
  (if (korz-eval test)
      (korz-eval then)
      (korz-eval else)))

(define-primitive quote (some)
  some)

(define-primitive progn (&rest body)
  (if (cdr body)
      (progn (korz-eval (car body))
	     (korz-eval (cons 'progn (cdr body))))
      (korz-eval (car body))))



(define-primitive def (context name &optional (value nil))
  (when (or (not (symbolp name)) (eq name t) (eq name nil)
	    (keywordp name))
    (error "~a can't be a constant slot name" name))
  (push-slot (cons (list (read-context context)
			 name nil :def)
		   (korz-eval value))))

(define-primitive var (context name &optional (value nil))
  (when (or (not (symbolp name)) (eq name t) (eq name nil)
	    (keywordp name))
    (error "~a can't be a constant slot name" name))
  (push-slot (cons (list (read-context context)
			 name nil :var)
		   (korz-eval value))))

(define-primitive with-context (context &rest body)
  (let* ((*current-context* 
	  (merge-context (read-context context)
			 *current-context*)))
    (korz-eval (cons 'progn body))))

(define-primitive let (parms &rest body)
  (let ((*slot-space* 
	 (append (mapcar (lambda (parm)
			   (cons (list nil
				       (car parm)
				       nil :let)
				 (korz-eval (second parm))))
			 parms) *slot-space*)))
    (korz-eval (cons 'progn body))))

(define-primitive method (context name args &rest body)
  (push-slot 
   (cons 
    (list (read-context context)
	  name
	  (mapcar (lambda (arg)
		    (if (listp arg)
			arg
			(list arg *any*))) args)
	  :method)
    (lambda (&rest rest)
      (korz-eval 
       `(let ,(mapcar #'list
		      args rest)
	  (progn ,@body)))))))
    
(define-primitive newcoord (&optional (parent *any* parent-gived-p))
  (let ((coord (gensym)))
    (if parent-gived-p
	(setf parent (korz-eval parent)))
    (push-coord  coord parent)
    coord))

(define-primitive setq (symbol value)
  (setf (get-slot (list *current-context* symbol nil))
	(korz-eval value)))

(define-primitive setf (place value)
  (if (atom place)
      (korz-eval `(setq ,place ,value))
      (case (car place)
	(car (setf
	      (car (cdr (get-slot 
			 (list *current-context* (second place) nil))))
	       (korz-eval value)))
	(cdr (setf 
	      (cdr (cdr (get-slot
			 (list *current-context* (second place)
			       nil))))
	      (korz-eval value)))
	(elt (setf (elt (cdr (get-slot 
			      (list *current-context* 
				    (second place) nil)))
			(korz-eval (third place)))
		   (korz-eval value)))
	(nth (setf 
	      (nth (korz-eval (second place))
		   (cdr (get-slot 
			 (list *current-context* (third place)
			       nil))))
	      (korz-eval value))))))

(define-primitive decf (place)
  (korz-eval `(setf ,place (1- ,place))))

(define-primitive incf (place)
  (korz-eval `(setf ,place (1+ ,place))))

(define-primitive copy-coord (coord)
  (let ((newcoord (gensym)))
    (push-coord newcoord coord)
    coord))

;;define function
(defmacro same-function (name)
  `(define-primitive ,name (&rest args)
     (apply (function ,name) (mapcar #'korz-eval args))))

(defmacro same-functions (&rest names)
  `(progn ,@(mapcar (lambda (name) 
		      `(same-function ,name))
		    names)))

(same-functions + - * / mod eq equal = cons list car cdr
		apply funcall 1+ 1- print format make-array
		elt nth)

;;;----------------------------------------------------
(defun init-env ()
  (setf *current-context* nil
	*slot-space* nil
	*dimension-list* nil))

(defun get-primitive (selector)
  (gethash selector *primitive-slots*))

(defun take-context (params)
  (remove nil
	  (maplist (lambda (ps)
		     (when (member (car ps) *dimension-list*)
		       (cons (first ps) (korz-eval (second ps)))))
		   params)))

(defun delcont-args (args)
  (let ((pos (position-if #'keywordp args)))
    (if pos (subseq args 0 pos) args)))

(defun merge-context (ctxs1 ctxs2)
  (let ((result (copy-list ctxs2)))
    (mapcar (lambda (ctx1)
	      (let ((pos (position (car ctx1) result :key #'car)))
		(if pos (setf (nth pos result) ctx1)
		    (push ctx1 result)))) ctxs1)
    result))

(defun call-primitive (primitive-func args)
  (apply primitive-func args))

(defun call-function (selector args)
  (let* ((vals (mapcar #'korz-eval (delcont-args args)))
	 (*current-context* 
	  (merge-context (take-context args)
			 *current-context*))
	 (func (or (get-method selector vals)
		   (error "The function ~a is unbound." selector))))
    (apply func vals)))

(defun call (selector args)
  (let ((*current-context* 
	 (merge-context (take-context args)
			*current-context*))
	(args (delcont-args args)))
    (cond ((get-primitive selector)
	   (call-primitive (get-primitive selector) args))
	  (t (call-function selector args)))))

(defun korz-eval (src)
  (cond ((consp src) (call (car src) (cdr src)))
	((or (eq src t) (eq src nil) (keywordp src)) src)
	((symbolp src) (or (get-primitive src)
			   (get-var src)))
	(t src)))

(defun korz-test (src)
  (init-env)
  (korz-eval src))

(defun main (file)
  (with-open-file (s file)
    (init-env)
    (loop repeat (korz-eval (read s nil nil)))))

