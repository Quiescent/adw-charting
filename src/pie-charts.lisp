(in-package :adw-charting)

(defclass pie-chart (chart)
  ((total :accessor total :initarg :total :initform nil))
  (:default-initargs :width 400))

(defmethod total ((chart pie-chart))
  "computes the pie-chart total based on the data, if no value is explicitly set"
  (if-let (total (slot-value chart 'total))
       total
       (setf (total chart)
	     (loop for item in 
		   (chart-elements chart)
	     summing (value item)))))

(defclass slice (chart-element)  
  ((value :accessor value :initarg :value))
  (:documentation "this is a slice of a pie chart"))

(defmethod radius ((chart pie-chart))
  (truncate (/ (- (height chart) 10)
	       2)))

(defmethod translate-to-next-label ((chart pie-chart) w h)
  (declare (ignore chart w))
  (translate 0 (- h)))

(defmethod legend-start-coords ((chart pie-chart) box-size label-spacing)
  (list (* 2 (+ (radius chart) (margin chart)))
	(- (height chart) box-size label-spacing)))

(defmethod has-data-p ((chart pie-chart))
  (chart-elements chart))

(defmethod draw-chart ((chart pie-chart))
  (let* ((radius (radius chart))
	 (width (width chart))
	 (height (height chart))
	 (cy (- height (+ 5 radius)))
	 (cx (+ 5 radius))
	 (slices (chart-elements chart)))
    ;;draw the background circle
    (set-rgb-stroke 0 0 0)
    (set-rgb-fill 0 0 0)
    (centered-circle-path cx cy (1+ radius))
    (fill-and-stroke)
    
    ;;draw the slices with a clipping path so we can avoid calcuting the circular bits.
    (with-graphics-state		;make a new state to handle the clipping paths
      (centered-circle-path cx cy (+ 1 radius))
      (clip-path)
      (end-path-no-op)
      (if (has-data-p chart)	  
	  (if (= 1 (length slices));;only one slice, draw all over the damn thing and call it good.
	      (progn 
		(set-fill (first slices))
		(rectangle 0 0 width height)
		(fill-path)) 
	      (let* ((bigr (* 3 radius));; more than one slice, go for it
		     (x (+ cx bigr))
		     (y cy)
		     (angle 0)
		     (val-to-radians (/ (* 2 pi) (total chart)))
		     (half-pi (/ pi 2)))
		(dolist (item slices)
		  (let* ((color (color item))
			 (segment (* val-to-radians (value item)))
			 (slice-size (+ angle segment))
			 (endx (+ cx (* bigr (cos slice-size))))
			 (endy (+ cy (* bigr (sin slice-size))))
			 (obtuse-p (> segment half-pi)))		    
		    ;;draw the sector as a huge wedge, the clipping path will take care of the spill-over
		    (move-to cx cy)
		    (line-to x y)
		    ;;if we're too big to encompassed with a wedge, fan out.  Assumes the slices
		    ;; are drawn counter-clockwise, starting at 3 o'clock.
		    (when obtuse-p		   
		      ;; move up to the top of graph, at the left or right edge depending on where we
		      ;; start
		      (when (>= y cy)
			(line-to (if (> x cx)
				     (width chart)
				     0)
				 (height chart)))

		      ;; if we cross the center-y, go all the way around
		      (when (and (>= y cy) (> cy endy))
			(line-to 0 (height chart))
			(line-to 0 0))

		      ;; if we cross the center-x, go all the way right
		      (when (> endx cx)		   
			(line-to (width chart) 0)))
	     
		    (line-to endx endy)
		    (line-to cx cy)
		    (close-subpath)
		    (set-fill color)
		    (fill-and-stroke)
		    ;;update our counters
		    (setf x endx
			  y endy
			  angle slice-size)))))
	  (setf (draw-legend-p chart) nil);;no data, supress the legend
	  ))))

(defmacro with-pie-chart ((width height &key (background ''(1 1 1))) &rest body)
  `(let ((*current-chart* (make-instance 'pie-chart
					 :width ,width
					 :height ,height
					 :background ,background)))
    ,@body))

(defun add-slice (label value &key color)
  "add a slice to the pie"
  (push (make-instance 'slice :color color :label label :value value)
	(chart-elements *current-chart*)))