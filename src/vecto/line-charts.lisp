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



(defclass line-chart (vchart)
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
   (data-min :accessor data-min 
	     :initform nil)
   (data-max :accessor data-max 
	     :initform nil)))

(defmethod offset-y ((gr graph-region) offset)
  (let ((offset (floor offset)))
    (incf (y gr) offset)
    (decf (height gr) offset)))

(defmethod offset-x ((gr graph-region) offset)
  (let ((offset (floor offset)))
    (incf (x gr) offset)
    (decf (width gr) offset)))

(defmethod data-scale ((gr graph-region))
  (with-accessors ((w width)
		   (h height)
		   (min data-min)
		   (max data-max)) gr
    (make-point (/ w
		   (- (x max) (x min)))
		(/ h
		   (* 1.1 (- (y max) (min 0 (y min))))))))

(defmethod data-origin ((graph graph-region))
  (with-accessors ((x x)
		   (y y)
		   (d-s data-scale)
		   (min data-min)
		   (max data-max)) graph
  (let ((d-o (make-point x y)))
    
    ;;adjust the origins if we need to
    
    (when (minusp (y min))
      (incf (y d-o) (abs (* (y d-s) (y min)))))
    
    (when (minusp (x min))
      (incf (x d-o) (abs (* (x d-s) (x min)))))
    
    (when (plusp (x min))
      (decf (x d-o) (* (x d-s) (x min))))
    d-o)))

(defmethod has-data-p ((chart line-chart))
  (and (chart-elements chart)
       (some #'data (chart-elements chart))))

(defun draw-axes (graph y-axis-labels-x text-height x-axis-labels-y)
  "draws the axes"
  (macrolet ((draw-gridline ((axis) &body gridline)
	       `(when (draw-gridlines-p ,axis)
		 (with-graphics-state
		   (set-line-width 1)
		   (set-stroke (background (chart graph)))
		   (set-dash-pattern #(10 2) 0)
		   ,@gridline
		   (stroke)))))

    (when-let (axis (y-axis (chart graph)))
      (loop for (txt x y) in 
	    (calculate-y-axes graph text-height y-axis-labels-x)
	    do (let ((half-text (/ text-height 2)))

		 (draw-string x (- y half-text) txt)
		 (draw-gridline (axis)
				(move-to (x graph) y)
				(line-to (+ (x graph) 
					    (width graph)) 
					 y))))
      (when (draw-zero-p axis)
	(with-graphics-state
	  (set-line-width 2)
	  (set-rgb-stroke 0 0 0)
	  (move (dp->gp graph (x (data-min graph)) 0))
	  (line (dp->gp graph (x (data-max graph)) 0))
	  (stroke))))

    (when-let (axis (x-axis (chart graph)))
      (loop for (txt x) in (calculate-x-axes graph)
	    do (progn
		 (draw-centered-string x  
				       x-axis-labels-y txt)
		 (draw-gridline (axis)
				(move-to x (y graph))
				(line-to x
					 (+ (y graph) (height graph))))))
      (when (draw-zero-p axis)
	(with-graphics-state
	  (set-line-width 2)
	  (set-rgb-stroke 0 0 0)
	  (move (dp->gp graph 0 (y (data-min graph))))
	  (line (dp->gp graph 0 (y (data-max graph))))
	  (stroke)))))) 

(defun calculate-x-axes (graph)
  (let ((axis (x-axis (chart graph))))  
    (ccase (mode axis)
      (:value (calculate-value-x graph))
      (:category (break "should draw in order"))
      )))

(defun calculate-value-x (graph)
  (let* ((min-x (x (data-min graph)))
	 (max-x (x (data-max graph)))
	 (diff (abs (- min-x max-x)))
	 (axis (x-axis (chart graph)))
	 (data-interval (or (data-interval axis)
			    (expt 10 
				  (- (floor (log diff 10))
				     1))))
	 (current-x (x graph))
	 (lst ()))
    ;;start drawing at 0, see how much we have
    (loop for x = min-x then (+ x data-interval)
	  for gx = (round (x (dp->gp graph x 0)))
	  until (> x max-x)
	  do (when (<= current-x gx)
	       ;;record + increment current-x
	       (let* ((txt (axis-label axis x))
		      (width (font-width (chart graph) txt)))
		 
		 (push (list txt gx) lst)
		 (setf current-x (+ gx width
				    (margin (chart graph)))))))
    lst))

(defun calculate-y-axes (graph text-height y-axis-labels-x)
  (let* ((min-y (y (data-min graph)))
	 (max-y (y (data-max graph)))
	 (axis (y-axis (chart graph)))
	 (diff (abs (- min-y max-y)))
	 (data-interval (or (data-interval axis)
			    (expt 10 
				  (- (floor (log diff 10))
				     1))))
	 (desired-text-space (* 2 text-height)))
    ;;be sure the interval has plenty of room in it for our text-height
    (loop for i = 1 then (1+ i)
	  until (< desired-text-space 
		   (* i data-interval (y (data-scale graph))))
	  finally (setf data-interval (* i data-interval)))

    (loop for (txt gp) in
	  (nconc
	   (loop for y = 0 then (+ y data-interval)
		 for gy = (y (dp->gp graph 0 y))
		 until (> gy (+ (height graph) (y graph)))
		 collect (list (axis-label axis y)
			       gy))
	   (loop for y = (- data-interval) then (- y data-interval)
		 until (< y min-y)
		 collect (list (axis-label axis y) 
			       (y (dp->gp graph 0 y)))))
	  collect (list txt y-axis-labels-x gp)
	  
	  )))

(defun draw-graph-area (graph &optional (border-only nil))
  "draws the graph aread"
  (with-graphics-state
    ;;set the chart background as the avg
    ;;between the background color and 1
    
    (set-rgb-stroke 0 0 0)    
    (rectangle (1- (x graph)) (1- (y graph))
	       (1+ (width graph)) (1+ (height graph)))
    
    (if border-only
	(set-rgba-fill 0 0 0 0)
	(set-fill (mapcar #'(lambda (c)
			      (/ (+ (if (eq 1 c)
					.7
					1) c) 2))
			  (background (chart graph)))))
    (fill-and-stroke)))


(defmethod dp->gp ((graph graph-region) x y)
  "convert a point from data space to graph space"
  (with-accessors ((d-o data-origin)
		   (d-s data-scale)) graph
    (make-point (+ (x d-o) 
		   (* (x d-s) x))
		(+ (y d-o) 
		   (* (y d-s) y)))))

(defmethod gp->dp ((graph graph-region) x y)
  "convert a point from graph space to data space"
  (with-accessors ((d-o data-origin)
		   (d-s data-scale)) graph
    (make-point (/ (- x (x d-o)) 
		   (x d-s))
		(/ (- y (y d-o)) 
		   (y d-s)))))

(defmethod draw-chart ((chart line-chart))
  (with-font ()

    (let* ((width (width chart))
	   (height (height chart))
	   (graph-margin (margin chart))
	   (text-height (font-height chart))
	   (legend-space (* 4 text-height))
	   (graph (make-instance 'graph-region 
				 :x graph-margin
				 :y (floor (+ legend-space graph-margin)) 
				 :width (- width graph-margin graph-margin graph-margin)
				 :height (- height graph-margin graph-margin 
					    legend-space)
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
	    (find-chart-extremes chart)
	  
	  (setf (data-min graph) (make-point min-x min-y))
	  (setf (data-max graph) (make-point max-x max-y))
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

	  (when (or (y-axis chart) (x-axis chart))
	    ;;set the drawing for grid-lines 
	    (draw-axes graph y-axis-labels-x 
		       text-height
		       x-axis-labels-y))

	  ;;TODO: make this a property of the series
	  (draw-series chart graph)
	  (draw-graph-area graph T))))))

(defgeneric draw-series (chart graph))

(defmethod draw-series ((chart line-chart) graph) 
  (dolist (series (chart-elements chart))
    (draw-line-series series graph)))

(defun draw-line-series (series graph)
  (with-graphics-state
    (set-line-width 2)
    (set-stroke series)
    (loop for (x y) in (data series)
	  for firstp = T then nil
	  do (funcall (if firstp #'move #'line)
		      (dp->gp graph x y)))
    (stroke)))

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