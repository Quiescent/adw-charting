(in-package :adw-charting)

(deflogger line-chart-log nil :appender (make-slime-repl-log-appender))

(defclass series (chart-element)
  ((data :accessor data :initarg :data :documentation "a list of (x y) pairs (as lists, not cons cells)"))  
  (:documentation "represents a line on a line chart"))

(defclass axis ()
  ((label :accessor label
	  :initarg :label
	  :initform nil
	  :documentation "description of this axis, usually the unit of measurement ($, s, km, etc)")   
   (label-formatter :accessor label-formatter
		    :initarg :label-formatter
		    :initform #'princ-to-string
		    :documentation "a function to format data points, for printing periodic values along the axis")
   (draw-gridlines-p :accessor draw-gridlines-p
		     :initarg :draw-gridlines-p
		     :initform T
		     :documentation "determines if grid-lines are drawn across the chart"))
  (:documentation "represents an axis on a line chart"))

(defclass line-chart (chart)
  ((series :accessor series
	   :initarg :series
	   :documentation "list of series objects" )
   (x-axis :accessor x-axis
	   :initarg :x-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for the X axis")
   (y-axis :accessor y-axis
	   :initarg :y-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for the Y axis")))

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
	 (graph-y (- height graph-height graph-margin))
	 (y-axis-x (+ graph-x (* 1.75 text-height))))
    

    
    ;;adjust our graph region to account for labels
    (when (y-axis chart)
      (let ((offset (+ (default-font-width chart "1234") (* 2.5 text-height))))
      ;;increase graph-x to account
      (incf graph-x offset)
      ;;decrease the width
      (decf graph-width offset)))

    ;;set the chart background as the avg between the background color and 1
    (set-fill (mapcar #'(lambda (c)
			  (/ (+ 1 c) 2))
		      (background chart)))

    (set-rgb-stroke 0 0 0)    
    (rectangle (1- graph-x) (1- graph-y) (1+ graph-width) (1+ graph-height))
    (fill-and-stroke)

    ;;if we're going to be drawing any axes, set the font and color
    (when (or (y-axis chart) (x-axis chart))      
      (set-font (get-font *default-font-file*) (label-size chart))
      (set-rgb-fill 0 0 0))
    
    (awhen (y-axis chart)
      (with-graphics-state
	;;move to the site of the y axis label
	(translate (+ graph-margin text-height) (+ graph-y (/ graph-height 2)))
	;;rotate the canvas so we're sideways	
	(rotate (/ pi 2))
	(draw-centered-string 0 0 (label it))))

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

	  (flet ((data-point->graph-point (x y)
		   "convert a point from data space to graph space"
		   (list (+ gx (* scale-x x))
			 (+ gy (* scale-y y))))
		 (graph-point->data-point (x y)
		   "convert a point from graph space to data space"
		   (list (/ (- x gx) scale-x)
			 (/ (- y gy) scale-y))))



	    ;;draw labels at regular intervals
	    (awhen (y-axis chart)
	      (flet ((draw-label (y)
		       (destructuring-bind (_ data-y) (graph-point->data-point 0 y)
			 (draw-string y-axis-x y (funcall (label-formatter it) data-y))
			 (when (draw-gridlines-p it)
			   (with-graphics-state
			     (set-line-width 1)
			     (set-stroke (background chart))
			     (set-dash-pattern #(10 2) 0)
			     (move-to graph-x y)
			     (line-to (+ graph-x graph-width) y)
			     (stroke)		     
			     )
			   )
			 ))
		     (at-top-p (y)
		       (< y (+ graph-height graph-y)))
		     (at-bottom-p (y)
		       (> y graph-y)))
		(let ((spacing (* text-height 3)))
		  (destructuring-bind (_ gy) (data-point->graph-point 0 0)
		    ;;start at 0, go up until we can't draw any more
		    (loop for uy = gy then (+ uy spacing)
			  for dy = (- gy spacing) then (- dy spacing)
			  while (or (at-top-p uy) (at-bottom-p dy))
			  when (at-top-p uy) do (draw-label uy)
			  when (at-bottom-p dy) do (draw-label dy)
			  )))))
	    

	    
					;draw the 0 line
	    (destructuring-bind (x y) (data-point->graph-point min-x 0)
	      (move-to x y))
	    (destructuring-bind (x y) (data-point->graph-point max-x 0)
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
		      do (destructuring-bind (px py) (data-point->graph-point x y)			     
			   (if first-p
			       (move-to px py)
			       (line-to px py))
			   ))
		(stroke)))


	    ))))))

(defmethod translate-to-next-label ((chart line-chart) w h)
  "moves the cursor right to the next legend position"
  (declare (ignore chart h))
  (translate w 0))

(defmethod legend-start-coords ((chart line-chart) box-size label-spacing)
  "starts the legends on the bottom row"
  (declare (ignore box-size label-spacing))
  (list (margin chart) (margin chart)))