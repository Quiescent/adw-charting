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
  :components
  ((:module :src
    :components ((:file "packages")
		 (:file "png" :depends-on ("packages"))
		 (:file "cl-vector-chart" :depends-on ("png"))
		 (:file "charts" :depends-on ("cl-vector-chart"))		 
		 ))))

