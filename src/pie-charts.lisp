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
	   (let ((value (first item))
		 (label (second item))
		 (color (and (>= (length item) 3)
			     (third item))))
	     (make-instance 'slice
			    :value value
			    :label label
			    :color color))
	   (make-instance 'slice :value item
				  :label (princ-to-string item))))
   items))

(defmethod draw-chart ((chart pie-chart))
  (let* ((radius (truncate (/ (- (height chart) 10)
			      2)))
	 (bigr (* 3 radius))
	 (cy (- (height chart) (+ 5 radius)))
	 (cx (+ 5 radius))
	 (total (total chart))
	 (x (+ cx bigr))
	 (y cy)
	 (angle 0)
	 (font (get-font *default-font-file*))
	 (slices '())
	 (labelboxes '())
	 (text-height (aref (string-bounding-box "A" (label-size chart) font) 3))
	 (label-spacing (* 3 text-height))
	 (box-width label-spacing)
	 (label-x (+ cx cx box-width))
	 (label-height (+ label-spacing text-height))
	 (label-y (- (height chart) label-height)) ;start the labels with a blank at the top
	 )
    ;;draw the background circle
    (set-rgb-stroke 0 0 0)
    (set-rgb-fill 0 0 0)
    (centered-circle-path cx cy (+ 1 radius))
    (fill-and-stroke)

    ;;;; plan out how to draw things

    ;;loop over the items, filling the slices and labelbox lists
    (mapc #'(lambda (item)
	      (let* ((color (color item))
		     (label (label item))
		     (segment (* 2 pi (/ (value item) total)))
		     (slice-size (+ angle segment))
		     (endx (+ cx (* bigr (cos slice-size))))
		     (endy (+ cy (* bigr (sin slice-size)))))
		(push (list x y endx endy color (> segment (/ pi 2))) slices)
		(push (list label-y label color) labelboxes)
		;;update our counters
		(setf x endx
		      y endy
		      angle slice-size)
		(decf label-y label-height)
		))
	  (slices chart))


    ;;;; do the actual drawing
    
    ;;draw the slices with a clipping path so we can avoid calcuting the circular bits.
    (with-graphics-state		;make a new state to handle the clipping paths
      (centered-circle-path cx cy (+ 1 radius))
      (clip-path)
      (end-path-no-op)
      (mapc
       #'(lambda (slice)	   
	   (destructuring-bind (x y endx endy color obtuse) slice
	     ;;draw the sector as a huge wedge, the clipping path will take care of the spill-over
	     (move-to cx cy)
	     (line-to x y)
	     (when obtuse ;;if we're too big to encompassed with a wedge, fan out
	       (progn
		 (when (> x cx)
		   (progn
		     (line-to x (height chart))
		     (line-to 0 (height chart))))
		 (line-to 0 0)

		 (when (> endx cx)
		   (line-to (width chart) 0))))
	     (line-to endx endy)
	     (line-to cx cy)
	     (close-subpath)
	     (set-fill color)
	     (fill-and-stroke)))
       slices))

    (set-font font (label-size chart)) ;set the font
    (set-rgb-fill 0 0 0) ;text should be black
    ;;draw the labels
    (mapc #'(lambda (labelbox)
	 (destructuring-bind (label-y label color) labelbox
	   (with-graphics-state
	     (set-fill color)
	     (rounded-rectangle label-x label-y box-width box-width 5 5)
	     (fill-and-stroke))
	   (draw-string (+ box-width label-x (/ box-width 2)) (+ label-y text-height) label)))
	 
	  labelboxes)))

(defmethod draw-legend ())
