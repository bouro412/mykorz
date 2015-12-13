;;;; package.lisp

(defpackage #:mykorz
  (:use #:cl  #:alexandria #:iterate)
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


