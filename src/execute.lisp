(in-package mykorz)

(defun main (file)
  (with-open-file (s file)
    (init-env)
    (do ((src (read s cl:nil cl:nil) (read s cl:nil cl:nil))
	 (env (empty-env))
	 (ctxt (make-contexts cl:nil)))
	((not src) cl:nil)
      (eval-top-exp src env ctxt))))


