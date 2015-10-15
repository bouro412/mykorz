;;;; mykorz.asd

(asdf:defsystem #:mykorz
  :description "Describe mykorz here"
  :author "Keita watanabe <keita.bouro@gmail.com>"
  :license "Specify license here"
  :serial t
  :components ((:module 
		src
		:components
		((:file "package")
		 (:file "utils")
		 (:file "slotspace")
		 (:file "coord")
		 (:file "slot")
		 (:file "match")
		 (:file "context")
		 (:file "env")
		 (:file "exp")
		 (:file "exp-func")
		 (:file "primitivef")
		 (:file "evaluate")
		 (:file "primitive")
		 (:file "execute"))))
  :depends-on (:alexandria :anaphora :split-sequence))

