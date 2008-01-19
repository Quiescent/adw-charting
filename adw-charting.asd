(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.charting.system)
    (defpackage :net.acceleration.charting.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.charting.system)


(defsystem :adw-charting
  :description "Charting package to make pretty graphs and charts"
  :author "Ryan Davis <ryan@acceleration.net>"
  :licence "LGPL (or talk to me)"
  :version "0.2"
  :depends-on (#:vecto)
  :components ((:module :src
			:components ((:file "packages")
				     (:file "utils" :depends-on ("packages"))
				     (:file "charting" :depends-on ("utils"))
				     (:file "pie-charts" :depends-on ("charting"))
				     (:file "line-charts" :depends-on ("charting"))))
	       (:module :test
			:depends-on (:src)
			:components ((:file "lisp-unit")
				     (:file "test-package" :depends-on ("lisp-unit"))
				     (:file "examples" :depends-on ("test-package"))
				     (:file "tests" :depends-on ("test-package"))))))