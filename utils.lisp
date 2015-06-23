(in-package #:mykorz)

(defmacro letrec (args &body body)
  (let ((params (mapcar #'first args))
	(vals (mapcar #'second args)))
    `(labels ((rec ,params ,@body))
       (apply #'rec (list ,@vals)))))

(defun make-pairs (list)
  (letrec ((lis list) (acc nil))
    (if (null lis) 
	acc 
	(rec (cddr lis) 
	     (cons (list (car lis) (cadr lis))
		   acc)))))
(defmacro while (test &body body)
  `(do ()
       ((not ,test) nil)
     ,body))

(defmacro trace-cond (cond-exp)
  (let ((new-exp (mapcar #'(lambda (exp)
			     `(,(car exp) 
				(format t "path ~a~%" 
					',(car exp))
				,@(cdr exp))) (cdr cond-exp))))
    `(cond ,@new-exp)))
	
    
