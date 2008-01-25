(in-package :adw-charting-tests)

(define-test pie-chart-total
  (assert-equal 10 (adw-charting::total (make-instance 'adw-charting::pie-chart :total 10))))

(define-test pie-chart-calculated-total
  "tests summing the pie-chart total from the data items"
  (assert-equal 45
		(with-pie-chart (400 400)
		  (add-slice "A" 10)
		  (add-slice "B" 15)
		  (add-slice "C" 20)
		  (adw-charting::total adw-charting::*current-chart*))))

;;;;test that the example programs run
(define-test examples
  (assert-true (and (minimal-pie-chart)
		    (minimal-line-chart)
		    (customized-line-chart)
		    (boinkmark))))