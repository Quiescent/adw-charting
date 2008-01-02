(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.charting.system)
    (defpackage :net.acceleration.charting.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.charting.system)


(defsystem :adw-charting
  :description "Charting package to make pretty graphs and charts"
  :author "<programmers@acceleration.net>"
  :licence "LGPL (or talk to me)"
  :version "0.1"
  :depends-on (#:vecto #:arnesi)
  :components ((:module :src
			:components ((:file "packages")
				     (:file "charting" :depends-on ("packages"))))
	       (:module :test
			:depends-on (:src)
			:components ((:file "lisp-unit")
				     (:file "charting" :depends-on ("lisp-unit"))
			))))

