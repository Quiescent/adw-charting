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

(in-package :adw-charting)

(defclass bar-chart (line-chart) ())

(defvar *bar-width* 1)

(defun number-of-bars (chart)
  (loop for series in (chart-elements chart)
	sum (length (data series))))

(defun draw-bar (x y bars-drawn chart graph)
  (declare (ignore chart))
  (let* ((gp (dp->gp graph x y))
	 (rx (+ (x gp)
		(* *bar-width*
		   bars-drawn)))
	 (ry (y (data-origin graph)))
	 (rw *bar-width*)
	 (rh (- (y gp)
		(y (data-origin graph)))))
    (rectangle rx ry rw rh)
    (fill-path)))

(defmethod draw-series ((chart bar-chart) graph)
  (let ((bars-drawn (make-hash-table))
	(*bar-width* (max 1
			  (truncate (/ (* 0.5 (width graph))
				       (number-of-bars chart))))))
    (dolist (series (chart-elements chart))
      (if (eq (mode series) 'default)
	  (draw-bar-series series graph bars-drawn chart)
	  (draw-line-series series graph)))))

(defun draw-bar-series (series graph bars-drawn chart)
  (decf (width graph) (* 2 *bar-width*))
  (with-graphics-state
    (set-line-width 2)
    (set-fill series)
    (loop for (x y) in (data series)
	  do (draw-bar x y (gethash x bars-drawn 0) chart graph )
	  (incf (gethash x bars-drawn 0)))
    )
  (incf (width graph) (* 2 *bar-width*)))

(defmacro with-bar-chart ((width height &key (background ''(1 1 1))) &body body)
  "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."
  `(let ((*current-chart*  (make-instance 'bar-chart
					  :width ,width
					  :height ,height
					  :background ,background)))
     ,@body))