(in-package :adw-charting)

(deflogger line-chart-log nil :appender (make-slime-repl-log-appender))

(defclass series (chart-element)
  ((data :accessor data :initarg :data :documentation "a list of (x y) pairs (as lists, not cons cells)"))  
  (:documentation "represents a line on a line chart"))

(defun make-series (label data &key (color nil))
  "makes a series"
  (make-instance 'series :label label :data data :color color))

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

(defun make-axis (label &key (control-string nil control-string-supplied-p)
			(draw-gridlines-p T) (label-formatter nil))
  "creates an axis object"
  (let ((axis (make-instance 'axis :label label :draw-gridlines-p draw-gridlines-p)))
    (when control-string-supplied-p
      (setf (label-formatter axis)
	    #'(lambda (val)
		(format nil control-string val))))
    (when label-formatter
      (setf (label-formatter axis) label-formatter))
    axis))

(defclass line-chart (chart)
  ((x-axis :accessor x-axis
	   :initarg :x-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for the X axis")
   (y-axis :accessor y-axis
	   :initarg :y-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for the Y axis")))

(defun make-line-chart (width height &key chart-elements (y-axis nil) (x-axis nil) (background nil))
  "creates a line-chart object"
  (make-instance 'line-chart
		 :width width
		 :height height
		 :chart-elements chart-elements
		 :y-axis y-axis
		 :x-axis x-axis
		 :background background))

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
  (and (chart-elements chart)
       (some #'data (chart-elements chart))))

(defmethod draw-chart ((chart line-chart))
  (declare (ignore _))
  (with-font ()
  (let* ((width (width chart))
	 (height (height chart))
	 (graph-margin (margin chart))
	 (text-height (default-font-height chart))
	 (legend-space (* 4 text-height))
	 (graph-height (- height graph-margin graph-margin legend-space) )
	 (graph-width (- width graph-margin graph-margin))
	 (graph-x graph-margin)
	 (graph-y (- height graph-height graph-margin))
	 (y-axis-x nil)
	 (x-axis-y nil))
    

    
   

    (awhen (x-axis chart)
      (let ((offset (* text-height
		       (if (label it)
			   3
			   2))))	
	(incf graph-y offset)
	(decf graph-height offset)
	(setf x-axis-y (- graph-y graph-margin text-height))))

    ;;set the chart background as the avg between the background color and 1
    (set-fill (mapcar #'(lambda (c)
			  (/ (+ 1 c) 2))
		      (background chart)))

    (set-rgb-stroke 0 0 0)    
    (rectangle (1- graph-x) (1- graph-y) (1+ graph-width) (1+ graph-height))
    (fill-and-stroke)

    ;;if we're going to be drawing any axes, set the font and color
    (when (or (y-axis chart) (x-axis chart))      
      (set-font *font* (label-size chart))
      (set-rgb-fill 0 0 0)

    ;;draw the y-label
    (awhen (y-axis chart)
      (with-graphics-state
	;;move to the site of the y axis label
	(translate (+ graph-margin text-height) (+ graph-y (/ graph-height 2)))
	;;rotate the canvas so we're sideways	
	(rotate (/ pi 2))
	(draw-centered-string 0 0 (label it))))

    ;;draw the x-label
    (awhen (and (x-axis chart) (label (x-axis chart)))
      (draw-centered-string (+ graph-x (/ graph-width 2)) (+ (/ graph-margin 2) legend-space) it))

    (when (has-data-p chart)
      ;;figure out the right scaling factors so we fill the graph    
					;find the min/max x/y across all series
      (destructuring-bind ((min-x min-y) (max-x max-y))
	  (find-extremes
	   (mapcan #'(lambda (series)
		       (find-extremes (data series)))
		   (chart-elements chart)))
	;;adjust our graph region to account for labels
	(awhen (y-axis chart)
	  (let* ((text-width (loop for y in (list min-y max-y)
				   maximizing (default-font-width chart y) into longest
				   finally (return longest)))
		 (offset (+ text-width
			    (* text-height (if (label it)
					       3
					       1.5)))))
	    ;;increase graph-x to account
	    (incf graph-x offset)
	    ;;decrease the width
	    (decf graph-width offset)
	    (setf y-axis-x (- graph-x graph-margin text-width))))

	
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




	    (when (or (y-axis chart) (x-axis chart))
	      (macrolet ((draw-gridline ((axis) &body gridline)
			   `(when (draw-gridlines-p ,axis)
			     (with-graphics-state
			       (set-line-width 1)
			       (set-stroke (background chart))
			       (set-dash-pattern #(10 2) 0)
			       ,@gridline
			       (stroke)))
			   ))
		(destructuring-bind (gx gy) (data-point->graph-point 0 0)
		  ;;draw y labels at regular intervals
		  (awhen (y-axis chart)
		    (flet ((draw-label (y)
			     (destructuring-bind (_ data-y) (graph-point->data-point 0 y)
			       (draw-string y-axis-x y (funcall (label-formatter it) data-y))
			       (draw-gridline (it)
					      (move-to graph-x y)
					      (line-to (+ graph-x graph-width) y))))
			   (at-top-p (y)
			     (< y (+ graph-height graph-y)))
			   (at-bottom-p (y)
			     (> y graph-y)))
		      (let ((spacing (* text-height 3)))
			;;start at 0, go up until we can't draw any more
			(loop for uy = gy then (+ uy spacing)
			      for dy = (- gy spacing) then (- dy spacing)
			      while (or (at-top-p uy) (at-bottom-p dy))
			      when (at-top-p uy) do (draw-label uy)
			      when (at-bottom-p dy) do (draw-label dy)))))

		  (awhen (x-axis chart)
		    (flet ((draw-label (x)
			     (destructuring-bind (data-x _) (graph-point->data-point x 0)
			       (let ((label (funcall (label-formatter it) data-x)))
				 (draw-centered-string x x-axis-y label)
				 (draw-gridline (it)
						(move-to x graph-y)
						(line-to x (+ graph-y graph-height)))
				 label)))
			   (at-left-p (x)
			     (> x graph-x))
			   (at-right-p (x)
			     (< x (+ graph-width graph-x))))
		      ;;start at the 0, go left / right while we can
		      (let ((spacing (* 2 (default-font-width chart (draw-label gx)))))
			(loop for rx = (+ gx spacing) then (+ rx spacing)
			      for lx = (- gx spacing) then (- lx spacing)
			      while (or (at-right-p rx)
					(at-left-p lx))
			      when (at-right-p rx) do (draw-label rx)
			      when (at-left-p lx) do (draw-label lx))))))))
	    

	    
					;draw the 0 line

	    (apply #'move-to (data-point->graph-point min-x 0))
	    (apply #'line-to (data-point->graph-point max-x 0))

	    (set-stroke '(0 0 0))
	    (stroke)
	    (set-line-width 2)		;TODO: make this a property of the series
	    (dolist (series (chart-elements chart))
	      (set-stroke series)
	      (loop for (x y) in (data series)
		    for first-p = T then nil
		    counting T into i
		    do (apply (if first-p #'move-to #'line-to)
			      (data-point->graph-point x y)))
	      (stroke)))))))))

(defmethod translate-to-next-label ((chart line-chart) w h)
  "moves the cursor right to the next legend position"
  (declare (ignore chart h))
  (translate w 0))

(defmethod legend-start-coords ((chart line-chart) box-size label-spacing)
  "starts the legends on the bottom row"
  (declare (ignore box-size label-spacing))
  (list (margin chart) (margin chart)))

(defmacro with-line-chart ((width height &key background) &rest body)
  `(let ((*current-chart* (make-line-chart ,width ,height :background ,background)))
    ,@body))

(defun add-series (&rest args)
  "adds a series to the *current-chart*.  args must match make-series signature"
   (push (apply #'make-series args) (chart-elements *current-chart*)))

(defun set-axis (axis &rest args)
  "set the axis on the *current-chart*.  axis is either :x or :y, args must match make-axis"
  (let ((ax (apply #'make-axis args)))
    (cond
      ((eq :x axis) (setf (x-axis *current-chart*) ax))
      ((eq :y axis) (setf (y-axis *current-chart*) ax))
      (t (error "axis must be :x or :y, not ~a" axis )))))