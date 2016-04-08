(require :prove)
(require :mykorz)
(defpackage #:mykorz-test
  (:use #:cl #:mykorz #:prove #:split-sequence)
  (:shadow is is-print))

(in-package mykorz-test)
(defmacro is (&body body)
  `(prove:is ,@body :test #'coord=rval))
(defmacro is-print (test &rest result)
  `(prove:is-print ,test (format nil ,@result)))

(defun coord=rval (coord val)
  (equal val (mykorz::get-value coord)))

(defvar *current-path* (load-time-value
			(or #.*compile-file-pathname* 
			    *load-pathname*)))
(defun file->path (name)
  (destructuring-bind (name type)
      (split-sequence #\. name)
    (make-pathname :defaults *current-path*
		   :name name :type type)))

(defmacro with-load-korz (file-path &body body)
  `(progn (main ,file-path)
	  (mykorz::run-korz ,@body)))

(plan 98)

;; immidiate test
(is (korz-test '3) 3)
(is (korz-test '2.3) 2.3)
(is (korz-test 'true) 'true)
(is (korz-test 'false) 'false)
(is (korz-test '"hello") (string-coord "hello") :test #'mykorz::coord=)
(is-error (korz-test '1/3) 'simple-error)
(is-error (korz-test '1.d0) 'simple-error)
(is-error (korz-test '#(1 2 3)) 'simple-error)
(is-error (korz-test '#2A((1 2) (3 4))) 'simple-error)

;; let-test
(is-error (korz-test 'x) 'simple-error)
(is (korz-test '(let ((x 1)) x)) 1)
(is (korz-test '(let ((x 1) (y 2)) y)) 2)
(is (korz-test '(let ((x 1)) (let ((x 3)) x))) 3)

;; if-test
(is (korz-test '(if true 1 2)) 1)
(is (korz-test '(if false 1 2)) 2)
(is (korz-test '(if false 1)) 'false)
(is (korz-test '(if (if false 1) 1 2)) 2)
(is-error (korz-test '(if 3 1 2)) 'simple-error)

;; slot-test
(is (korz-tests (def () test 10) (test))  10)
(is (korz-tests (var () fujichi 20) (fujichi)) 20)
(is (korz-tests (method () atsushi (x y z) 
			(if x y z))
		(atsushi true 30 40)) 
    30)
(is (korz-tests (def (:rcvr 23) brother 40)
		(brother :rcvr 23))
    40)

;; def-test
(is (korz-tests (def () A 3)
		(A))
    3)
(is (korz-tests (def () A 10)
		(set (A) 5)
		(A))
    5)
(is (korz-tests (def () A (newcoord))
		(def (:rcvr (A)) sub-A 10)
		(var () b (copy (A)))
		(sub-A :rcvr (b)))
    10)
(is (korz-tests (def () A (newcoord))
		(def (:rcvr (A)) sub-A 10)
		(set (sub-A :rcvr (A)) 20)
		(var () b (copy (A)))
		(sub-A :rcvr (b)))
    20)
(is (korz-tests (def () A (newcoord))
		(def (:rcvr (A)) sub-A 10)
		(var () b (copy (A)))
		(set (sub-A :rcvr (A)) 20)
		(sub-A :rcvr (b)))
    10)
(is (korz-tests (def () A (newcoord))
		(def (:rcvr (A)) sub-A 10)
		(var () b (copy (A)))
		(set (sub-A :rcvr (b)) 20)
		(sub-A :rcvr (A)))
    10)

;; var-test
(is (korz-tests (def () A (newcoord))
		(var (:rcvr (A)) x 1)
		(let ((a (copy (A))))
		  (x :rcvr a)))
    1)
(is (korz-tests (def () v (newcoord))
                (def () A (newcoord))
		(var (:rcvr (A)) x 1)
		(let ((a (copy (A))))
		  (set (x :rcvr a) 2)
		  (x :rcvr (A))))
    1)
(is (korz-tests (def () A (newcoord))
		(var (:rcvr (A)) x 1)
		(set (x :rcvr (A)) 2)
		(let ((a (copy (A))))
		  (x :rcvr (a))))
    2)
(is (korz-tests (def () A (newcoord))
		(var (:rcvr (A)) x 1)
		(def () B (newcoord))
		(var (:rcvr (B)) a (copy (A)))
		(var () b1 (copy (B)))
		(var () b2 (copy (B)))
		(set (x :rcvr (a :rcvr (b1))) 10)
		(x :rcvr (a :rcvr (b2))))
    1)

;; newcoord-test
(is (korz-tests (def () x (newcoord))
		(var (:rcvr (x)) t 50)
		(t :rcvr (x)))
    50)
(is (korz-tests (def () parent (newcoord))
		(def () child (newcoord (parent)))
		(var (:self (parent)) hoge true)
		(method (:self (parent)) test ()
		   (if (hoge) 60 70))
		(test :self (child)))
    60)
(is (korz-tests (def () parent (newcoord))
		(def () child (newcoord (parent)))
		(var (:self (parent)) hoge true)
		(var (:self (child)) hoge false)
		(method (:self (parent)) test ()
		   (if (hoge) 60 70))
		(test :self (child)))
    70)

;;context test
(is (korz-tests (def () c (newcoord))
		(def () d (newcoord (c)))
		(method (:rcvr (c)) func (a)
			1)
		(method (:rcvr (d)) func (a)
			2)
		(func 1 :rcvr (d)))
    2)

(is (korz-tests (def () c (newcoord))
		(def () d (newcoord (c)))
		(method (:rcvr (c)) func ((a (c)))
			1)
		(method (:rcvr (d)) func ((a (c)))
			3)
		(func (c) :rcvr (d)))
    3)

;; progn-test
(is (korz-test '(progn 1 2 3)) 3)
(is-error (korz-test '(progn (def () tst 3))) 'simple-error)
(is-error (korz-test '(progn (method () tst () 3))) 'simple-error)
(is-error (korz-test '(progn (var () tst 3))) 'simple-error)
(is (korz-test '(progn (let ((x 1)) x))) 1)

;; set-test
(is (korz-test '(let ((x 2)) (set x 10) x)) 10)
(is (korz-test '(let ((x 1)) 
		  (let ((x 3)) (set x 10) x)))
    10)
(is (korz-test '(let ((x 1)) 
		  (let ((x 3)) (set x 10)) x))
    1)
(is (korz-tests (def () x 10)
		(set (x) 20)
		(x)) 
    20)
(is (korz-tests (var (:rcvr 10) x 10)
		(set (x :rcvr 10) 30)
		(x :rcvr 10))
    30)

;; primitive-function test
; print
(is-print (korz-test '(print 3)) "3
")
(is-print (korz-test '(print true)) "TRUE
")
(is-print (korz-test '(print "hello")) "hello~%")
(is (korz-test '(print "hello")) "hello")
(is (korz-test '(print 3)) 3)

; eq
(is (korz-test '(eq 1 1)) 'false)
(is (korz-test '(let ((x 1)) (eq x x))) 'true)
(is (korz-tests (def () x (newcoord))
		(eq (x) (x)))
    'true)
(is (korz-tests (def () x (newcoord))
		(def () y (newcoord))
		(eq (x) (y))) 
    'false)

; copy
(is (korz-tests (def () x (newcoord))
		(var () x1 (copy (x)))
		(eq (x) (x1)))
    'false)
(is (korz-tests (def () x (newcoord))
		(var (:rcvr (x)) a 1)
		(def () x1 (copy (x)))
		(= (a :rcvr (x1)) :rcvr (a :rcvr (x))))
    'true)
(is (korz-tests (def () tmp (newcoord))
		(def () x (newcoord))
		(var (:rcvr (x)) a (tmp))
		(def () x1 (copy (x)))
		(eq (a :rcvr (x1)) (a :rcvr (x))))
    'true)

; error
(is-error (korz-test '(error "test")) 'simple-error)

; +
(is (korz-test '(+ 3 :rcvr 4)) 7)
(is (korz-tests (def () tkkw 10)
		(def () pipi 11)
		(+ (tkkw) :rcvr (pipi)))
    21)

; 四則演算
(is (korz-test '(- 10 :rcvr 4)) -6)
(is (korz-test '(* 10 :rcvr 4)) 40)
(is (korz-test '(/ 10 :rcvr 4)) 0.4)
; 比較演算
(is (korz-test '(< 10 :rcvr 3)) 'true)
(is (korz-test '(> 10 :rcvr 3)) 'false)
(is (korz-test '(<= 10 :rcvr 3)) 'true)
(is (korz-test '(>= 10 :rcvr 3)) 'false)
(is (korz-test '(< 20 :rcvr 20)) 'false)
(is (korz-test '(> 20 :rcvr 20)) 'false)
(is (korz-test '(<= 20 :rcvr 20)) 'true)
(is (korz-test '(>= 20 :rcvr 20)) 'true)
; length
(is (korz-test '(length :rcvr "tkkw")) 4)
(is (korz-test '(length :rcvr "")) 0)
; elt
(is (korz-test '(elt 5 :rcvr "kuwakuwa")) "u")
(is (korz-test '(elt 5 :rcvr "kuwakuwa")) "u")
(is-error  (korz-test '(elt 8 :rcvr "kuwakuwa")) 
	   'sb-kernel::index-too-large-error)
(is-error (korz-test '(elt -1 :rcvr "kuwa"))
	  'type-error)
; cat
(is (korz-test '(cat "kuwa" :rcvr "taka"))
    "takakuwa")

;;proceed
(is (korz-tests (method () fn (a)
				 (+ 1 :rcvr a))
		(method (:assert true) fn (a)
			(* 2 :rcvr (proceed)))
		(fn 10 :assert true)) 
    22)

; file test
(is-print (with-load-korz (file->path "example1.korz")
	    (main))
	  "100~%200~%")

(is-print (with-load-korz (file->path "example1.korz")
	    (main :assertions true))
	  "100~%200~%")

(is-print (with-load-korz (file->path "example1.korz")
	    (main :multithread true))
	  "This is multi thread~%100~%This is multi thread~%200~%")

(is-print (with-load-korz (file->path "example1.korz")
	    (main :assertions true :multithread true))
	  "This is multi thread and enable assertions~%100~%This is multi thread and enable assertions~%200~%")

(is-error (with-load-korz (file->path "example1-2.korz")
	    (main :assertions true :multithread true))
	  'simple-error "Ambiguous.")

(is-print (with-load-korz (file->path "example2.korz")
	    (let ((p1 (make-point 10 10 "red"))
		  (s (copy (screen))))
	      (display :rcvr p1 :device s)))
	  "draw~%10~%10~%red~%draw-complete~%")

(is-print (with-load-korz (file->path "example2.korz")
	    (let ((p1 (make-point 10 10 "red"))
		  (s (copy (screen))))
	      (display :rcvr p1 :device s 
		       :is-color-blind true)))
	  "draw~%10~%10~%gray-red~%draw-complete~%")

(is-print (with-load-korz (file->path "example2.korz")
	    (let ((p1 (make-point 10 10 "red"))
		  (s (copy (screen))))
	      (display :rcvr p1 :device s 
		       :location (australia))))
	  "draw~%10~%-10~%red~%draw-complete~%")

(is-print (with-load-korz (file->path "example2.korz")
	    (let ((p1 (make-point 10 10 "red"))
		  (s (copy (screen))))
	      (display :rcvr p1 :device s 
		       :location (antarctica))))
	  "draw~%20~%-20~%red~%draw-complete~%")

(is-print (with-load-korz (file->path "example2.korz")
	    (let ((p1 (make-point 10 10 "red"))
		  (s (copy (screen))))
	      (display :rcvr p1 :device s 
		       :location (antarctica)
		       :is-color-blind true)))
	  "draw~%20~%-20~%gray-red~%draw-complete~%")


(is-error (with-load-korz (file->path "example2-2.korz")
	    (let ((p1 (make-point 10 10 "red"))
		  (s (copy (screen))))
	      (display :rcvr p1 :device s 
		       :location (australia)
		       :is-color-blind true)))
	  'simple-error)

(is-print (with-load-korz (file->path "ecpad2-5.korz")
	    (main))
	  "B.y with alpha~%0~%A.x with beta~%")

(is-print (with-load-korz (file->path "ecpad2-5.korz")
	    (main :beta true))
	  "B.y with alpha~%0~%A.x with beta~%")
(is-print (with-load-korz (file->path "ecpad2-5.korz")
	    (let ((b (copy (B))))
	      (z :rcvr b)))
	  "0~%")
(is-print (with-load-korz (file->path "ecpad2-5.korz")
	    (let ((b (copy (B))))
	      (z :rcvr b :alpha true)))
	  "0~%")
(is-print (with-load-korz (file->path "ecpad2-5.korz")
	    (let ((b (copy (B))))
	      (z :rcvr b :beta true)))
	  "0~%A.x with beta~%")
(is-print (with-load-korz (file->path "ecpad8-1.korz")
	    (main))
	  "kuwa~%")
(is-print (with-load-korz (file->path "ecpad8-1.korz")
	    (main :address true))
	  "kuwaooo~%")

;; next collide test

(finalize)
