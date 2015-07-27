;;;; package.lisp

(defpackage #:mykorz
  (:use #:cl #:decorator #:alexandria)
  (:shadow slot 
	   t
	   nil)
  (:export
   #:true
   #:false
   #:string-coord
   #:main
   #:korz-test
   #:korz-tests))


