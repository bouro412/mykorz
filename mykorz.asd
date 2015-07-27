;;;; mykorz.asd

(asdf:defsystem #:mykorz
  :description "Describe mykorz here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
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
	       (:file "execute"))
  :depends-on (:alexandria :anaphora :decorator))

