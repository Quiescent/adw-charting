(in-package :adw-charting)

(defclass series (chart-element)
  ((data :accessor data
	 :initarg :data
	 :documentation "a list of (x y) pairs (as lists, not cons cells)"))  
  (:documentation "represents a line on a line chart"))

(defclass axis ()
  ((label :accessor label
	  :initarg :label
	  :initform nil
	  :documentation "description of this axis, usually the unit
of measurement ($, s, km, etc)")   
   (label-formatter :accessor label-formatter
		    :initarg :label-formatter
		    :initform #'princ-to-string
		    :documentation "a function to format data points, for
printing periodic values along the axis")
   (draw-gridlines-p :accessor draw-gridlines-p
		     :initarg :draw-gridlines-p
		     :initform T
		     :documentation "determines if grid-lines are drawn
across the chart"))
  (:documentation "represents an axis on a line chart"))

(defmethod axis-label ((axis axis) data)
  (funcall (label-formatter axis) data))

(defclass line-chart (chart)
  ((x-axis :accessor x-axis
	   :initarg :x-axis
	   :initform nil
	   :documentation "an axis object to determine formatting for
the X axis")
   (y-axis :accessor y-axis
	   :initarg :y-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for
the Y axis")))

(defclass graph-region (point area)
  ((chart :accessor chart
	  :initarg :chart)
   (data-origin :accessor data-origin
		:initarg :data-origin
		:initform nil)
   (data-scale :accessor data-scale
	       :initarg :data-scale
	       :initform nil)))

(defmethod data-origin ((gr graph-region))
  (if-let (d (slot-value gr 'data-origin))
	  d
	  (setf (data-origin gr) (clone gr))))

(defmethod offset-y ((gr graph-region) offset)
  (incf (y gr) offset)
  (decf (height gr) offset))

(defmethod offset-x ((gr graph-region) offset)
  (incf (x gr) offset)
  (decf (width gr) offset))

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

(defun draw-axes (graph y-axis-labels-x text-height x-axis-labels-y)
  "draws the axes"
  (macrolet ((draw-gridline ((axis) &body gridline)
	       `(when (draw-gridlines-p ,axis)
		 ,@gridline
		 (stroke))))
		
    (destructuring-bind (gx gy) (dp->gp graph 0 0)
      ;;draw y labels at regular intervals
      (when-let (axis (y-axis (chart graph)))
	(flet ((draw-label (y)
		 (destructuring-bind (_ data-y) (gp->dp graph 0 y)
		   (declare (ignore _))
		   (draw-string y-axis-labels-x (- y (/ text-height 2))
				(axis-label axis data-y))
		   (draw-gridline (axis)
				  (move-to (x graph) y)
				  (line-to (+ (x graph) 
					      (width graph)) 
					   y))))
	       (below-top-p (y)
		 (< y (+ (height graph) (y graph))))
	       (above-bottom-p (y)
		 (> y (y graph))))
	  (let ((spacing (* text-height 3)))
	    ;;start at 0, go up until we can't draw any more
	    (loop for uy = gy then (+ uy spacing)
		  for dy = (- gy spacing) then (- dy spacing)
		  while (or (below-top-p uy) (above-bottom-p dy))
		  when (below-top-p uy) do (draw-label uy)
		  when (above-bottom-p dy) do (draw-label dy)))))

      (when-let (axis (x-axis (chart graph)))
	(flet ((draw-label (x)
		 (destructuring-bind (data-x _) (gp->dp graph x 0)
		   (declare (ignore _))
		   (let ((label (axis-label axis data-x)))
		     (draw-centered-string x x-axis-labels-y label)
		     (draw-gridline (axis)
				    (move-to x (y graph))
				    (line-to x
					     (+ (y graph) (height graph))))
		     label)))
	       (after-left-p (x)
		 (> x (x graph)))
		   
	       (before-right-p (x)
		 (< x (+ (width graph) (x graph)))))
	  (let ((spacing (* 2 (font-width
			       (chart graph)
			       (draw-label gx)))))
	    ;;start at the 0, go left / right while we can
	    (loop for rx = (+ gx spacing) then (+ rx spacing)
		  for lx = (- gx spacing) then (- lx spacing)
		  while (or (before-right-p rx)
			    (after-left-p lx))
		  when (before-right-p rx) do (draw-label rx)
		  when (after-left-p lx) do (draw-label lx))))))))
 
(defun draw-graph-area (graph)
  "draws the graph aread"
  (with-graphics-state
    ;;set the chart background as the avg
    ;;between the background color and 1
    (set-fill (mapcar #'(lambda (c)
			  (/ (+ (if (eq 1 c)
				    .7
				    1) c) 2))
		      (background (chart graph))))
    (set-rgb-stroke 0 0 0)    
    (rectangle (1- (x graph)) (1- (y graph))
	       (1+ (width graph)) (1+ (height graph)))
    (fill-and-stroke)))

(defmethod dp->gp ((graph graph-region) x y)
  "convert a point from data space to graph space"  
  (mapcar #'floor 
	  (list (+ (x (data-origin graph)) 
		   (* (x (data-scale graph)) x))
		(+ (y (data-origin graph)) 
		   (* (y (data-scale graph)) y)))))

(defmethod gp->dp ((graph graph-region) x y)
  "convert a point from graph space to data space"
  (list (/ (- x (x (data-origin graph))) 
	   (x (data-scale graph)))
	(/ (- y (y (data-origin graph))) 
	   (y (data-scale graph)))))

(defmethod draw-chart ((chart line-chart))
  (with-font ()
    (let* ((width (width chart))
	   (height (height chart))
	   (graph-margin (margin chart))
	   (text-height (font-height chart))
	   (legend-space (* 4 text-height))
	   (graph (make-instance 'graph-region 
				 :x graph-margin
				 :y (+ legend-space graph-margin) 
				 :width (- width graph-margin graph-margin)
				 :height (- height graph-margin graph-margin legend-space)
				 :chart chart))
	   (y-axis-labels-x nil)
	   (x-axis-labels-y nil))

      ;;if we're going to be drawing any axes, set the font and color
      (when (or (y-axis chart) (x-axis chart))      
	(set-font *font* (label-size chart))
	(set-rgb-fill 0 0 0)

	;;move the graph region about
	(when-let (axis (x-axis chart))
	  (let ((offset (* text-height
			   (if (label axis)
			       3
			       2))))	
	    (offset-y graph offset)
	    (setf x-axis-labels-y (- (y graph) graph-margin text-height))
	    ;;draw the x-label
	    (when-let (label (label axis))
	      (draw-centered-string (+ (x graph) (/ (width graph) 2))
				    (+ (/ graph-margin 2) legend-space)
				    label))))

	;;draw the y-label
	(when-let (label (and (y-axis chart) 
			   (label (y-axis chart))))
	  (with-graphics-state
	    ;;move to the site of the y axis label
	    (translate (+ graph-margin text-height)
		       (+ (y graph) (/ (height graph) 2)))
	    ;;rotate the canvas so we're sideways	
	    (rotate (/ pi 2))
	    (draw-centered-string 0 0 label))))

      (when (has-data-p chart)
	;;figure out the right scaling factors so we fill the graph    
					;find the min/max x/y across all series
	(destructuring-bind ((min-x min-y) (max-x max-y))
	    (find-extremes
	     (mapcan #'(lambda (series)
			 (find-extremes (data series)))
		     (chart-elements chart)))
	  ;;adjust our graph region to account for labels
	  (when-let (axis (y-axis chart))
	    (let* ((text-width (loop for y in (list min-y max-y)
				     maximizing (font-width chart
							    (axis-label axis y)) 
				     into longest
				     finally (return longest)))
		   (offset (+ text-width
			      (* text-height (if (label axis)
						 3
						 1.5)))))
	      (offset-x graph offset)
	      (setf y-axis-labels-x (- (x graph) graph-margin text-width))))

	  (draw-graph-area graph)
	
	  (let* ((d-o (data-origin graph))
		 (scale-x (/ (width graph) 
			     (max 1 (- max-x min-x))))
		 (scale-y (/ (height graph) 
			     (max 1 (* 1.1 (- max-y min-y))))))
	    (setf (data-scale graph) (make-instance 'point 
						    :x scale-x
						    :y scale-y))
	    ;;adjust the origins if we need to
	    (when (minusp min-y)
	      (incf (y d-o) (abs (* scale-y min-y))))
	    (when (minusp min-x)
	      (incf (x d-o) (abs (* scale-x min-x))))
	    (setf (data-origin graph) d-o)

	    (when (or (y-axis chart) (x-axis chart))
	      ;;set the drawing for grid-lines
	      (with-graphics-state
		(set-line-width 1)
		(set-stroke (background chart))
		(set-dash-pattern #(10 2) 0)

		(draw-axes graph y-axis-labels-x 
			   text-height
			   x-axis-labels-y)))

	      ;;draw the 0 line
	      (apply #'move-to (dp->gp graph min-x 0))
	      (apply #'line-to (dp->gp graph max-x 0))
	      (set-rgb-stroke 0 0 0)
	      (stroke)

	      ;;TODO: make this a property of the series
	      (with-graphics-state
		(set-line-width 2)		
		(dolist (series (chart-elements chart))
		  (set-stroke series)
		  (loop for (x y) in (data series)
			for firstp = T then nil
			do (apply (if firstp #'move-to #'line-to)
				  (dp->gp graph x y)))
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

(defmacro with-line-chart ((width height &key (background ''(1 1 1))) &body body)
  "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."
  `(let ((*current-chart*  (make-instance 'line-chart
					  :width ,width
					  :height ,height				  
					  :background ,background)))
    ,@body))

(defun add-series (label data &key (color nil))
  "adds a series to the *current-chart*."
  (push (make-instance 'series :label label :data data :color color)
	(chart-elements *current-chart*)))

(defun set-axis (axis title &key (draw-gridlines-p T) (label-formatter #'princ-to-string))
  "set the axis on the *current-chart*.  axis is either :x or :y.
label-formatter is either a format-compatible control string or
a function of 1 argument to control label formatting"
  (let ((ax (make-instance 'axis
			   :label title
			   :draw-gridlines-p draw-gridlines-p
			   :label-formatter (etypecase label-formatter
					      (string #'(lambda (v)
							  (format nil label-formatter v)))
					      (function label-formatter)))))
    (ccase axis
      (:x (setf (x-axis *current-chart*) ax))
      (:y (setf (y-axis *current-chart*) ax)))))
