;; Copyright (c) 2008 Accelerated Data Works, Ryan Davis

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
  :depends-on (#:iterate #:closer-mop)
  :components ((:module :src
			:components ((:file "packages")
				     (:file "utils" :depends-on ("packages"))
				     (:file "charting" :depends-on ("utils"))))))

