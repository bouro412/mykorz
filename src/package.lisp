;;;; package.lisp

(defpackage #:mykorz
  (:use #:cl  #:alexandria #:iterate #:anaphora)
  (:shadow slot)
  (:export
   #:true
   #:false
   #:string-coord
   #:run
   #:korz-test
   #:korz-tests))
