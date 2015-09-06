(require :prove)
(require :mykorz)
(defpackage #:mykorz-test
  (:use #:cl #:mykorz #:prove)
  (:shadow is))

(in-package mykorz-test)
(defmacro is (&body body)
  `(prove:is ,@body :test #'coord=rval))

(defun coord=rval (coord val)
  (equal val (mykorz::get-value coord)))

(defun rewrite-func (exp)
  (case (car exp)
    ('is (append exp '(:test #'coord=rval)))))
(defmacro with-rewrite (&body body)
  `(progn ,@(mapcar #'rewrite-func body)))

(plan 33)

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
(is-error (korz-test '(if 3 1 2)) 'simple-error)

;; slot-test
(is (korz-tests (def () test 10) (test)) 10)
(is (korz-tests (var () fujichi 20) (fujichi)) 20)
(is (korz-tests (method () atsushi (x y z) 
			(if x y z))
		(atsushi true 30 40)) 
    30)
(is (korz-tests (def (:rcvr 23) brother 40)
		(brother :rcvr 23))
    40)
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
; length
(is (korz-test '(length :rcvr "tkkw")) 4)
; elt

(finalize)

