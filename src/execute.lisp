(in-package mykorz)

(defun main (file)
  (with-open-file (s file)
    (init-env)
    (do ((src (read s nil nil) (read s nil nil))
	 (env (empty-env))
	 (ctxt (make-contexts nil)))
	((not src) nil)
      (eval-top-exp src env ctxt))))


