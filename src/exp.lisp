(in-package mykorz)

(defun exp-to-list (exp) exp)

;;(func arg1 arg2 ... argn :dmi1 coord1 ...)
;; -> (list arg1 arg2 arg3 ... argn)
(defun exp->func-exp (exp)
  (car exp))
(defun exp->rest-exp-list(exp)
  (cdr exp))
(defun exp->args-exp-list (exp)
  (subseq exp 1 (position-if #'dimension-p exp)))
(defun exp->context-exp-list (exp)
  (let ((pos (position-if #'dimension-p exp)))
    (if pos
	(subseq exp pos))))

;; exp operator
(defun first-exp (exp) (car exp))
(defun second-exp (exp) (second exp))
(defun third-exp (exp) (third exp))
(defun fourth-exp (exp) (fourth exp))
(defun fifth-exp (exp) (fifth exp))
(defun rest-exp (exp) (cdr exp))
(defun nth-rest-exp (n exp) (nthcdr n exp))
(defun nth-exp (n exp) (nth n exp))
(defun not-id-p (exp) (consp exp))
(defun exp-size (exp) (length exp))

;; context-exp -> context
(defun eval-context-exp-list (exps env ctxt)
  (letrec ((exps exps) (acc cl:nil))
    (cond ((null exps) (nreverse acc))
	  ((or (null (cdr exps))
	       (dimension-p (second exps)))
	   (rec (cdr exps) (cons *any*
				 (cons (first exps) acc))))
	  (cl:t (rec (cddr exps) 
		  (cons (eval-exp (second exps) env ctxt)
			(cons (first exps)
			      acc)))))))

;; (ctxt-exp env ctxt) -> ctxt
(defun create-new-context (ctxt-exp env ctxt)
  (merge-context (make-contexts
		  (eval-context-exp-list
		   ctxt-exp env ctxt))
		 ctxt))
