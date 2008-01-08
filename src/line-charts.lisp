(in-package :adw-charting)

(defclass series (chart-element)
  ((data :accessor data :initarg :data :documentation "a list of (x y) pairs (as lists, not cons cells)"))  
  (:documentation "represents a line on a line chart"))

(defclass line-chart (chart)
  ((series :accessor series :initarg :series :documentation "list of series objects" )))

(defmethod chart-elements ((chart line-chart))
  (series chart))

(defun find-extremes (data)
  "takes a list of (x y) pairs, and returns the ((x-min y-min) (x-max y-max))"
  (loop for (x y) in data
	maximizing x into x-max
	minimizing x into x-min
	maximizing y into y-max
	minimizing y into y-min
	finally (return (list (list x-min y-min)
			      (list x-max y-max)))))

(defmethod has-data-p ((chart line-chart))
  (and (series chart) (find-if-not #'null (mapcar #'data (series chart)))))

(defmethod draw-chart ((chart line-chart))
  (let* ((width (width chart))
	 (height (height chart))
	 (graph-margin (margin chart))
	 (text-height (default-font-height chart))
	 (legend-space (* 4 text-height))
	 (graph-height (- height graph-margin graph-margin legend-space) )
	 (graph-width (- width graph-margin graph-margin))
	 (graph-x graph-margin)
	 (graph-y (- height graph-height graph-margin)))

    ;;draw the graphs
    (set-rgb-fill .9 .9 .9) ;TODO: base this off the (background chart) color
    (set-rgb-stroke 0 0 0)
    (rectangle (1- graph-x) (1- graph-y) (1+ graph-width) (1+ graph-height))
    (fill-and-stroke)

    (when (has-data-p chart)
      ;;figure out the right scaling factors so we fill the graph    
					;find the min/max x/y across all series
      (destructuring-bind ((min-x min-y) (max-x max-y))
	  (find-extremes
	   (mapcan #'(lambda (series)
		       (find-extremes (data series)))
		   (series chart)))
	(let* ((gx graph-x)
	       (gy graph-y)
	       (scale-x (/ graph-width (- max-x min-x)))
	       (scale-y (/ graph-height (* 1.1 (- max-y min-y)))))
					;adjust the origins if we need to
	  (when (> 0 min-y)
	    (incf gy (abs (* scale-y min-y))))
	  (when (> 0 min-x)
	    (incf gx (abs (* scale-x min-x))))

	  (flet ((convert-point (x y)
		   "convert a point from data space to graph space"
		   (values (+ gx (* scale-x x))
			   (+ gy (* scale-y y)))))    
    
					;draw the 0 line
	    (multiple-value-bind (x y) (convert-point min-x 0)
	      (move-to x y))
	    (multiple-value-bind (x y) (convert-point max-x 0)
	      (line-to x y))
	    (set-stroke '(0 0 0))
	    (stroke)
	    (set-line-width 2)		;TODO: make this a property of the series
	    (dolist (series (series chart))
	      (with-graphics-state
		(set-stroke series)
		(loop for (x y) in (data series)
		      for first-p = T then nil
		      counting T into i
		      do (multiple-value-bind (px py) (convert-point x y)			     
			   (if first-p
			       (move-to px py)
			       (line-to px py))))
		(stroke)))))))))

(defmethod translate-to-next-label ((chart line-chart) w h)
  (declare (ignore chart h))
  (translate w 0))

(defmethod legend-start-coords ((chart line-chart) box-size label-spacing)
  (declare (ignore box-size label-spacing))
  (list (margin chart) (margin chart)))