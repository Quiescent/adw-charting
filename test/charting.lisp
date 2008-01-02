(defpackage #:adw-charting-tests
  (:use #:cl #:adw-charting #:lisp-unit))

(in-package #:adw-charting-tests)

(defparameter *pie-chart-data* (list
				(make-instance 'data-item :value 10)
				(make-instance 'data-item :value 15)
				(make-instance 'data-item :value 20)))

(define-test pie-chart-total
  (assert-equal 10 (total (make-instance 'pie-chart :total 10))))

(define-test pie-chart-calculated-total
  "tests summing the pie-chart total from the data items"
  (assert-equal 45 (total (make-instance 'pie-chart :data-items *pie-chart-data*))))


(defun pie-chart-sample ()  
  "draws a simple pie chart, rending to pie-chart-sample.png"
  (render-chart
   (make-instance 'pie-chart
		  :data-items (make-items '(40
					    (10 "baz")
					    (60 "bar" (.5 .5 .5)))))
   "pie-chart-sample.png"))