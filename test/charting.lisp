(defpackage #:adw-charting-tests
  (:use #:cl #:adw-charting #:lisp-unit))

(in-package #:adw-charting-tests)

(defparameter *pie-chart-data* (list
				(make-instance 'slice :value 10)
				(make-instance 'slice :value 15)
				(make-instance 'slice :value 20)))

(define-test pie-chart-total
  (assert-equal 10 (total (make-instance 'pie-chart :total 10))))

(define-test pie-chart-calculated-total
  "tests summing the pie-chart total from the data items"
  (assert-equal 45 (total (make-instance 'pie-chart :slices *pie-chart-data*))))


(defun pie-chart-sample ()  
  "draws a simple pie chart, rending to pie-chart-sample.png"
  (render-chart
   (make-instance 'pie-chart
		  :slices (make-slices '(40
					    (10 "baz")
					    (60 "bar" (.5 .5 .5)))))
   "pie-chart-sample.png"))



;;see output at http://ryepup.unwashedmeme.com/files/line-chart-sample.png
(defun line-chart-sample ()  
  "draws a simple line chart"
  (let ((seriesA (make-instance 'series
				:label "SeriesA"
					;data expressed as x/y pairs
				:data '((-1 -2) (0 4) (1 5) (4 6) (5 -3))))
	(seriesB (make-instance 'series
				:label "SeriesB"
				:data '((-1 4) (0 -2) (1 6) (5 -2) (6 5)))))
    (render-chart
     (make-instance 'line-chart
		    :width 400
		    :background '(.7 .7 .7)
		    :series (list seriesA seriesB))
     "line-chart-sample.png")))