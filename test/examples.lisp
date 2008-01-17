(in-package :adw-charting-tests)

(defvar *root* (asdf:component-pathname
		(asdf:find-system :adw-charting)))

(defun minimal-pie-chart ()  
  (with-pie-chart (400 400)
    (add-slice "A" 5.0d0)
    (add-slice "B" 2.0d0)
    (save-file (merge-pathnames *root* "minimal-pie-chart.png"))))

(defun minimal-line-chart ()
  (with-line-chart (400 300)
    (add-series "A" '((-1 -2) (0 4) (1 5) (4 6) (5 -3)))
    (add-series "B" '((-1 4) (0 -2) (1 6) (5 -2) (6 5)))
    (save-file (merge-pathnames *root* "minimal-line-chart.png"))))

(defun customized-line-chart ()
  (with-line-chart (400 300 :background '(.7 .5 .7))
    (add-series "A" '((-1 -2) (0 4) (1 5) (4 6) (5 -3)))
    (add-series "B" '((-1 4) (0 -2) (1 6) (5 -2) (6 5)))
    (add-series "C"
		'((-1 0) (0 3) (1 1) (2 5) (4 -6))
		:color '(.3 .7 .9))
    (set-axis :y "widgets" :label-formatter "~D")
    (set-axis :x nil
	      :label-formatter #'(lambda (v)
				   (format nil "#~a" v)))
    (save-file (merge-pathnames *root* "customized-line-chart.png"))))


