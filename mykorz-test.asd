;;;; mykorz-test.asd

(asdf:defsystem #:mykorz-test
  :description "Describe mykorz here"
  :author "Keita watanabe <keita.bouro@gmail.com>"
  :license "Specify license here"
  :serial t
  :components ((:module 
		test
		:components
		((:file "test-case"))))
  :depends-on (:prove :mykorz))

