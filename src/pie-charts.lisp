(in-package :adw-charting)

(defclass pie-chart (chart)
  ((slices :accessor slices :initarg :slices)
   (total :accessor total :initarg :total :initform nil))
  (:default-initargs :width 400))

(defmethod total ((chart pie-chart))
  "computes the pie-chart total based on the data, if no value is explicitly set"
  (aif (slot-value chart 'total)
       it
       (let ((total 0))
	 (mapc
	  #'(lambda (item)
	      (incf total (value item)))
	  (slices chart))
	 total)))

(defclass slice (chart-element)  
  ((value :accessor value :initarg :value))
  (:documentation "this is a slice of a pie chart"))

(defun make-slices (items)
  "makes slices from the list provided.  Accepts several formats:
make-items 10 10 10  makes 3 items with 10 as the value and label"
  (mapcar
   #'(lambda (item)
       (if (listp item)
	   (destructuring-bind (value &optional label color) item
	     (make-instance 'slice
			    :value value
			    :label label
			    :color color))
	   (make-instance 'slice :value item
				  :label (princ-to-string item))))
   items))

(defmethod radius ((chart pie-chart))
  (truncate (/ (- (height chart) 10)
	       2)))

(defmethod draw-legend ((chart pie-chart))
  (let* ((font (get-font *default-font-file*))
	 (text-height (default-font-height chart))
	 (box-length (* 3 text-height))
	 (label-spacing (/ box-length 2))
	 (radius (radius chart))
	 (label-height (+ box-length text-height))
	 (label-x (+ radius radius box-length (margin chart)))
	 (label-y (- (height chart) label-height)))
    (set-font font (label-size chart)) ;set the font
    (set-rgb-fill 0 0 0) ;text should be black
    (dolist (elem (slices chart))
      (with-graphics-state
	   (set-fill (color elem))
	   (rounded-rectangle label-x label-y box-length box-length text-height text-height)
	   (fill-and-stroke))
      	 (draw-string (+ box-length label-x label-spacing)
		      (+ label-y text-height)
		      (label elem))
	 	 (decf label-y label-height))))


(defmethod draw-chart ((chart pie-chart))
  (let* ((radius (radius chart))
	 (width (width chart))
	 (height (height chart))
	 (cy (- height (+ 5 radius)))
	 (cx (+ 5 radius))
	 (slices (slices chart)))
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
      (cond
	((= 0 (length slices));;supress the legend if we have no slices
	 (setf (draw-legend-p chart) nil))
	((= 1 (length slices)) ;;only one slice, draw all over the damn thing and call it good.
	 (set-fill (first slices))
	 (rectangle 0 0 width height)
	 (fill-path))
	((> (length slices) 1) ;; more than one slice, go for it
	 (let* ((bigr (* 3 radius))
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
	       (format *trace-output* "Making slice for ~a, obtuse-p ~a ~%" (label item) obtuse-p)
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
		     angle slice-size)))))))))