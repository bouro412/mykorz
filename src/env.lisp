(in-package mykorz)

(defun empty-env ()
  nil)

(defun extend-env (var val env)
  (cons (cons var val) env))

(defun extend-envs (vars vals env)
  (append (mapcar #'cons vars vals) env))

(defun apply-env (env sym)
  (let ((result (assoc sym env)))
    (if result
	(cdr result)
	(error "The variable ~a is unbound." sym))))

(defun has-binding-p (env sym)
  (not (null (assoc sym env))))

(defun set-env (env sym value)
  (setf (nth (position sym env :key #'car) env)
	(cons sym value)))
