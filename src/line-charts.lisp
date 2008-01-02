(in-package :adw-charting)

(defclass series (chart-element)
  ((data :accessor data :initarg :data))
  (:documentation "represents a line on a line chart"))

(defclass line-chart (chart)
  ((series :accessor series :initarg :series)))

(defun find-extremes (data)
  "takes a list of x,y pairs, and returns the ((x-min y-min) (x-max y-max))"
  (let* ((x-values (mapcar #'first data))
	 (y-values (mapcar #'second data))
	 (x-min (reduce #'min x-values))
	 (x-max (reduce #'max x-values))
	 (y-min (reduce #'min y-values))
	 (y-max (reduce #'max y-values)))
    (list (list x-min y-min)
	  (list x-max y-max))))

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
    (set-rgb-fill .9 .9 .9)
    (set-rgb-stroke 0 0 0)
    (rectangle (1- graph-x) (1- graph-y) (1+ graph-width) (1+ graph-height))
    (fill-and-stroke)

    ;;figure out the right scaling factors so we fill the graph    
    ;find the min/max x/y across all series
    (let* ((extremes (find-extremes
		      (mapcan #'(lambda (series)
				  (find-extremes (data series)))
			      (series chart))))
	   (min-x (caar extremes))
	   (min-y (cadar extremes))
	   (max-x (caadr extremes))
	   (max-y (cadadr extremes))
	   (gx graph-x)
	   (gy graph-y)
	   (scale-x (/ graph-width (- max-x min-x)))
	   (scale-y (/ graph-height (* 1.1 (- max-y min-y)))))
					;adjust the origins if we need to
      (when (> 0 min-y)
	(incf gy (abs (* scale-y min-y))))
      (when (> 0 min-y)
	(incf gx (abs (* scale-x min-x))))

      ;;define our graph bounds
      (let ((max-graph-x (truncate (+ graph-x graph-width)))
	    (min-graph-x graph-x)
	    (max-graph-y (- (truncate (+ graph-y graph-height)) graph-margin))
	    (min-graph-y (+ (truncate graph-y) graph-margin)))
	(format *trace-output*
		"graph bounds: ~a ~a, ~a ~a~%"
		min-graph-x min-graph-y max-graph-x max-graph-y)
	(flet ((convert-point (x y)
		 "convert a point from data space to graph space"

		 ;;try to keep the point in our graph bounds
		 (values (truncate (min (max (+ gx (* scale-x x))
					     min-graph-x)
					max-graph-x))
			 (truncate (min (max (+ gy (* scale-y y))
					     min-graph-y)
					max-graph-y)))))
    
    
					;draw the 0 line
	  (multiple-value-bind (x y) (convert-point min-x 0)
	    (move-to x y))
	  (multiple-value-bind (x y) (convert-point max-x 0)
	    (line-to x y))
	  (set-stroke '(0 0 0))
	  (stroke)
	  (set-line-width 2)
	  
	  (mapc
	   #'(lambda (series)
	       (with-graphics-state
		 (set-stroke series)
		 (let ((drawing-p nil))
		   (loop for (x y) in (data series)
			 counting T into i
			 do (multiple-value-bind (px py) (convert-point x y)
			      (format *trace-output* "converted ~a: ~a ~a -> ~a ~a~%" i x y px py)
			      (if drawing-p
				  (line-to px py)
				  (progn
				    (move-to px py)
				    (setf drawing-p T))))))
		 (stroke)))
	   (series chart))))))
  (draw-legend chart))

(defmethod draw-legend ((chart line-chart))
  (let* ((label-x (margin chart))
	 (label-y (margin chart))
	 (font (get-font *default-font-file*))
	 (text-height (default-font-height chart))
	 (box-length (* 3 text-height))
	 (label-spacing text-height)
	 )
    (set-font font (label-size chart)) ;set the font
    (set-rgb-fill 0 0 0) ;text should be black
    (mapc
     #'(lambda (series)
	 (with-graphics-state
	   (set-fill (color series))
	   (rounded-rectangle label-x label-y box-length box-length text-height text-height)
	   (fill-and-stroke))
	 (draw-string (+ box-length label-x label-spacing)
		      (+ label-y text-height)
		      (label series))
	 (incf label-x (+ box-length label-spacing label-spacing
			  (aref (string-bounding-box (label series)
						     (label-size chart)
						     font)
				2)))
	 )
     (series chart))))