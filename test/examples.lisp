(defpackage #:adw-charting-tests
    (:use #:cl #:adw-charting #:lisp-unit))

(in-package #:adw-charting-tests)

(define-test pie-chart-total
  (assert-equal 10 (total (make-instance 'pie-chart :total 10))))

(define-test pie-chart-calculated-total
  "tests summing the pie-chart total from the data items"
  (assert-equal 45
		(total (make-instance 'pie-chart :slices (make-slices '(10 15 20))))))


(define-test pie-chart-sample
  "draws a simple pie chart, rending to pie-chart-sample.png"
  (assert-true (render-chart
		(make-instance 'pie-chart :slices
			       (make-slices '((5.0d0 "POP3-First")
					      (2.0d0 "POP3-Additional"))))
		"pie-chart-sample.png")))

(define-test line-chart-sample
  "draws a simple line chart"
  (let* ((seriesA (make-instance 'series
				 :label "SeriesA"
					;data expressed as a list (x y) pairs
				 :data '((-1 -2) (0 4) (1 5) (4 6) (5 -3))))
	 (seriesB (make-instance 'series
				 :label "SeriesB"
				 :data '((-1 4) (0 -2) (1 6) (5 -2) (6 5))))
	 (chart (make-instance 'line-chart
			       :width 400
			       :background '(.7 .7 .7)
			       :series (list seriesA seriesB))))
    (assert-true (render-chart chart "line-chart-sample.png"))))

(defun months-from-now->mm/yy (months-ago)
  "converts months-ago (-1 for 1 month ago) to a string of mm/yy"
  (let* ((now (get-universal-time))
	 (one-month (encode-universal-time 0 0 0 1 2 1900 0))
	 (seconds-ago (floor (* months-ago one-month)))
	 (date (multiple-value-list
		   (decode-universal-time
		    (+ now seconds-ago)))))
    (format nil "~D/~D"
	    (nth 4 date)
	    (subseq (princ-to-string (nth 5 date))
		    2))))

(defun line-chart-with-axis () 
  (render-chart
   (make-line-chart 400 300
		    :background '(.7 .7 .7)
		    :series (list (make-series "SeriesA"
					       '((-1 -2) (0 4) (1 5) (4 6) (5 -3)))
				  (make-series "SeriesB"
					       '((-1 4) (0 -2) (1 6) (5 -2) (6 5))
					       :color '(.3 .7 .9))
				  (make-series "SeriesC"
					       '((-1 0) (0 3) (1 1) (2 5) (4 -6))))
		    :y-axis (make-axis "widgets"
				       :control-string "~$")
		    :x-axis (make-axis "time"
				       :label-formatter #'months-from-now->mm/yy))
   "line-chart-with-axis-labels.png"))

(defun line-chart-with-axis-imperative ()
  (with-line-chart (400 300 :background '(.7 .7 .7))
    (add-series "SeriesA"
		'((-1 -2) (0 4) (1 5) (4 6) (5 -3)))
    (add-series "SeriesB"
		'((-1 4) (0 -2) (1 6) (5 -2) (6 5))
		:color '(.3 .7 .9))
    (add-series "SeriesC"
		'((-1 0) (0 3) (1 1) (2 5) (4 -6)))
    (set-axis :y "widgets" :control-string "~$")
    (set-axis :x "time" :label-formatter #'months-from-now->mm/yy)
    (save-file "line-chart-with-axis-labels.png")))


(define-test line-chart-with-axis-labels
  (assert-true (line-chart-with-axis)))

(define-test line-chart-with-axis-labels-imperative
  (assert-true (line-chart-with-axis-imperative)))
