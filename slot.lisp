(in-package #:mykorz)

;;;----------------------------------------------------
;;;; slot implements
;;;; slot-space : (list (slotguard . content))
;;; slotguard : (context selector params ID expression)
;;; slotcall : (context selector args)
;;; content = Common lisp content

;; context : (list (dimension . coordinate))
;; selector : normal symbol
;; params : (list (symbol . (or *any* coordinate type)))
;; ID : keyword symbol
;; args : (list content)

; dimension : keyword symbol
; Coordinate  : gensymed symbol

(defclass slot ()
  ((guard :initarg :guard :accessor get-guard)
   (content :initarg :content :accessor get-content)
   (init-exp :initarg :exp :accessor get-exp)))


(defmethod print-object ((s slot) stream)
  (format stream "slot(~a)" (get-guard s)))

(defun make-slot (&key context selector params content)
  (make-slot% (make-guard context selector params)
	      content))

(defun make-slot% (slotguard value)
  (make-instance 'slot :guard slotguard :content value))

(defclass guard ()
  ((context :initarg :context :accessor get-context)
   (selector :initarg :selector :accessor get-selector)
   (params :initarg :params :accessor get-params)))

(defmethod print-object ((g guard) stream)
  (format stream "ctxt: ~a, selector: ~a, params: ~a"
	  (get-context g) (get-selector g) (get-params g)))

(defun make-guard (context selector params)
  (make-instance 'guard :context context
		 :selector selector
		 :params params))

(defun empty-context () cl:nil)

(defun copy-context (ctxt) (copy-list ctxt))

(defun context-to-list (context)
  context)

(defun list-to-context (list)
  list)

(defun get-context-by-dim (dim context)
  (assoc dim context))

(defun has-dim-p (sym context)
  (not (null (assoc (if (keywordp sym) 
			sym
			(intern (symbol-name sym) "KEYWORD"))
		    context))))
(defun get-context-val-by-dim-var (sym context)
  (cdr (assoc (intern (symbol-name sym) "KEYWORD")
	      context)))

(defun context-size (context)
  (length context))

(defun params-num (params)
  (length params))

(defun args-num (args)
  (length args))

(defun args-to-list (args)
  args)

(defun list-to-args (listargs)
  listargs)

(defun params-to-list (params)
  params)

(defclass slot-call ()
  ((context :initarg :context :accessor get-context)
   (selector :initarg :selector :accessor get-selector)
   (args :initarg :args :accessor get-args)))

(defmethod print-object ((s slot-call) stream)
  (format stream "#<slot-call :ctxt ~a :selector ~a :args ~a>" (get-context s) (get-selector s) (get-args s)))
 

(defun make-call (context selector args)
  (make-instance 'slot-call :context context
		 :selector selector
		 :args args))

(defun empty-args () cl:nil)

(defmacro param-symbol (param)
  `(first ,param))

(defun params-symbol (params)
  (mapcar #'car params))

(defmacro param-type (param)
  `(second ,param))

(defun make-param (param)
  (if (listp param)
      param
      (list param *any*)))

(defun make-params (params)
  (mapcar #'make-param params))

;(defmacro get-ID (sg)
;  `(fourth ,sg))

(defmacro get-dim (context)
  `(car ,context))

(defmacro get-coord (context)
  `(cdr ,context))

(defun add-slot (slot)
  (pushnew slot *slot-space* :key #'get-guard 
	   :test (lambda (guard1 guard2)
		   (and (equal (get-context guard1)
			       (get-context guard2))
			(eq (get-selector guard1)
			    (get-selector guard2))
			(equal (get-params guard1)
			       (get-params guard2))))))

(defun search-slots (func)
  (remove-if-not func *slot-space*))

(defun search-context (dim context)
  (assoc dim context))

(defsetf search-context (dim context) (value)
  `(setf (cdr (assoc ,dim ,context)) ,value))

(defun add-context (dc context)
  (cons dc context))

(defun make-dim-and-coord (dim context)
  (if (keywordp dim)
      (cons dim context)
      (error "dimension name must be a keyword")))

(defun make-contexts (dcs)
  (mapcar (lambda (dc) (apply #'make-dim-and-coord dc))
	  (make-pairs dcs)))
