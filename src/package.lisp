;;;; package.lisp

(defpackage #:mykorz
  (:use #:cl  #:alexandria #:iterate)
  (:shadow slot)
  (:export
   #:true
   #:false
   #:string-coord
   #:main
   #:korz-test
   #:korz-tests))


