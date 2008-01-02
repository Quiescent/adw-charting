(in-package :adw-charting)

(defparameter +default-colors+ '((1 1 0)
				 (1 0 1)
				 (1 0 0)
				 (0 1 1)
				 (0 1 0)
				 (0 0 1)))

;;might be able to: you can merge-pathnames with (component-pathname (find-system :my-system)) probably
(defvar *default-font-file* "/usr/share/fonts/truetype/freefont/FreeSans.ttf")

(defvar *color-stack* +default-colors+)

(defclass chart ()
  ((width :accessor width
	  :initarg :width
	  :type integer
	  :initform 200)
   (height :accessor height
	   :initarg :height
	   :type integer
	   :initform 200)
   (background :accessor background
	       :initarg :background
	       :initform nil )))


(defgeneric render-chart (chart filename)
  (:documentation "renders the chart to the given file"))

(defgeneric draw-chart (chart)
  (:documentation "draws the chart, assuming a vecto canvas is open"))

(defmethod render-chart ((chart chart) filename)
  (with-canvas (:width (width chart) :height (height chart))
    (awhen (background chart)
      (progn
	(apply #'set-rgb-fill it)
	(clear-canvas)))
    (draw-chart chart)
    (save-png filename)))

(defclass data-item ()  
  ((color :accessor color :initarg :color :initform nil)
   (label :accessor label :initarg :label :initform "none")
   (value :accessor value :initarg :value))
  (:documentation "this is a triple storing value, color, and label"))

(defmethod color ((item data-item))
  (aif (slot-value item 'color)
       it
       (pop *color-stack*)))

(defclass pie-chart (chart)
  ((data-items :accessor data-items :initarg :data-items)
   (total :accessor total :initarg :total :initform nil)
   (label-size :accessor label-size :initarg :label-size :initform 12))
  (:default-initargs :width 400))

(defmethod total ((chart pie-chart))
  "computes the pie-chart total based on the data, if no value is explicitly set"
  (aif (slot-value chart 'total)
       it
       (let ((total 0))
	 (mapc
	  #'(lambda (item)
	      (incf total (value item)))
	  (data-items chart))
	 total)))


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

    (setq *color-stack* +default-colors+) ;ensure we have colors to auto-assign

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
	  (data-items chart))


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
	     (apply #'set-rgb-fill color)
	     (fill-and-stroke)))
       slices))

    (set-font font (label-size chart)) ;set the font
    (set-rgb-fill 0 0 0) ;text should be black
    ;;draw the labels
    (mapc #'(lambda (labelbox)
	 (destructuring-bind (label-y label color) labelbox
	   (with-graphics-state
	     (apply #'set-rgb-fill color)
	     (rounded-rectangle label-x label-y box-width box-width 5 5)
	     (fill-and-stroke))
	   (draw-string (+ box-width label-x (/ box-width 2)) (+ label-y text-height) label)))
	 
	  labelboxes)))


(defun make-items (items)
  "makes data-items from the list provided.  Accepts several formats:
make-items 10 10 10  makes 3 items with 10 as the value and label"
  (mapcar
   #'(lambda (item)
       (if (listp item)
	   (let ((value (first item))
		 (label (second item))
		 (color (and (>= (length item) 3)
			     (third item))))
	     (make-instance 'data-item
			    :value value
			    :label label
			    :color color))
	   (make-instance 'data-item :value item
				  :label (princ-to-string item))))
   items))
