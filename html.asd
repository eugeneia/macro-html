;;;; System definition for html.

(defpackage html-asd
  (:use :cl :asdf))

(in-package :html-asd)

(defsystem html
  :components ((:file "package")
	       (:file "html" :depends-on ("package"))
	       (:file "syntax" :depends-on ("package" "html"))
	       (:file "widgets"
		:depends-on ("package" "html" "syntax")))
  :depends-on ("named-readtables"))
