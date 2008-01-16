(in-package :adw-charting-tests)

(define-test pie-chart-total
  (assert-equal 10 (total (make-instance 'pie-chart :total 10))))

(define-test pie-chart-calculated-total
  "tests summing the pie-chart total from the data items"
  (assert-equal 45
		(total (make-instance 'pie-chart
				      :chart-elements (make-slices '(10 15 20))))))


;;;;test that the example programs run
(define-test pie-chart-clos-test
  (assert-true (pie-chart-sample)))

(define-test pie-chart-imperative-test
  (assert-true (pie-chart-imperative)))

(define-test pie-chart-imperative-test
  (assert-true (line-chart-sample)))

(define-test line-chart-with-axis-labels
  (assert-true (line-chart-with-axis)))

(define-test line-chart-with-axis-labels-imperative
  (assert-true (line-chart-with-axis-imperative)))
