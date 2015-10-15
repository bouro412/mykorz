(in-package #:mykorz)

;;(:dim coord ...) -> ((:dim . coord) ...)
;; if coord is nil,  remove this context.



;;;next
(defun merge-context (ctxt1 ctxt2)
  (let ((result (copy-context ctxt1)))
    (dolist (dc (context-to-list ctxt2) result)
      (unless (has-dim-p (get-dim dc) result)
	(setf result (add-context dc result))))))






