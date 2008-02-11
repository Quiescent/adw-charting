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

(defun bar-width (chart)
  1)

(defun draw-bar (x y bars-drawn chart graph)
  (let* ((gp (dp->gp graph x y))
	 (rx (+ (x gp)
		(* (bar-width chart)
		   bars-drawn)))
	 (ry (y (data-origin graph)))
	 (rw (bar-width chart))
	 (rh (- (y gp)
		(y (data-origin graph)))))
    (rectangle rx ry rw rh)
    (fill-path)))

(defmethod draw-series ((chart bar-chart) graph)
  (decf (width graph) (bar-width chart))
  (let ((bars-drawn (make-hash-table)))    
    (with-graphics-state
      (set-line-width 2)
      (dolist (series (chart-elements chart))
	(set-fill series)
	(loop for (x y) in (data series)
	   do (draw-bar x y (gethash x bars-drawn 0) chart graph )
	   (incf (gethash x bars-drawn 0))))))
  (incf (width graph) (bar-width chart)))

(defmacro with-bar-chart ((width height &key (background ''(1 1 1))) &body body)
  "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."
  `(let ((*current-chart*  (make-instance 'bar-chart
					  :width ,width
					  :height ,height
					  :background ,background)))
     ,@body))