(in-package :adw-charting)

(defparameter +default-colors+ '((1 1 0) ;rgb 0-1
				 (1 0 1)
				 (1 0 0)
				 (0 1 1)
				 (0 1 0)
				 (0 0 1)))

(defvar *default-font-file* (merge-pathnames
			     (asdf:component-pathname
			      (asdf:find-system :adw-charting))
			     "FreeSans.ttf"))

(defvar *color-stack* +default-colors+)

(defvar *current-font* nil "a font object")

(defclass chart ()
  ((width :accessor width
	  :initarg :width
	  :type integer
	  :initform 200)
   (height :accessor height
	   :initarg :height
	   :type integer
	   :initform 200)
   (label-size :accessor label-size
	       :initarg :label-size
	       :initform 12)
   (margin :accessor margin
	   :initarg :margin
	   :initform 10)
   (draw-legend-p :accessor draw-legend-p
		  :initarg :draw-legend-p
		  :initform T)
   (background :accessor background
	       :initarg :background
	       :initform '(1 1 1))
   (chart-elements :accessor chart-elements
		   :initarg :chart-elements
		   :initform nil)))


(defmethod default-font-bounding-box ((chart chart) text)
  "gets the bounding box for the given text on the given chart."
  (with-font ()
    (string-bounding-box text
			 (label-size chart)
			 *font*)))

(defmethod default-font-height ((chart chart))
  "gets the pixel height of the default font, at
the size specified in the chart's label-size"
  (aref (default-font-bounding-box chart "A") 3))

(defmethod default-font-width ((chart chart) text)
  "gets the pixel width of the default font, as the size
specified in the chart's label-size"
    (aref (default-font-bounding-box chart text) 2))

(defvar *font* nil "a font object")

(defmacro with-font ((&optional (font-file *default-font-file*) ) &rest body)
  "ensures *font* is a valid font loader."
  `(let ((*font* (or *font* (get-font ,font-file))))
    ,@body))

(defgeneric render-chart (chart filename)
  (:documentation "renders the chart to the given file")
  (:method ((chart chart) filename)
	   (with-canvas (:width (width chart) :height (height chart))
	     (set-fill chart) 
	     (clear-canvas) ;;fills in the background
	     (setq *color-stack* +default-colors+) ;ensure we have colors to auto-assign
	     (draw-chart chart)
	     (when (draw-legend-p chart)
	       (draw-legend chart))
	     (save-png filename))))

(defgeneric draw-chart (chart)
  (:documentation "draws the chart, assuming a vecto canvas is open"))

(defclass chart-element ()
  ((color :accessor color :initarg :color :initform nil)
   (label :accessor label :initarg :label :initform "none"))
  (:documentation "this is a super-class for various chart elements"))

(defmethod color ((item chart-element))
  (aif (slot-value item 'color)
       it
       (setf (color item) (pop *color-stack*))))

(defgeneric set-fill (obj)
  (:documentation "shortcuts for setting the vecto fill color"))

(defmethod set-fill ((lst cons))
  (apply #'set-rgb-fill lst))

(defmethod set-fill ((chart chart))
  (awhen (background chart)
    (set-fill it)))

(defmethod set-fill ((elem chart-element))
  (set-fill (color elem)))

(defgeneric set-stroke (obj)
  (:documentation "shortcuts for setting the vecto stroke color"))

(defmethod set-stroke ((lst cons))
  (apply #'set-rgb-stroke lst))

(defmethod set-stroke ((elem chart-element))
  (set-stroke (color elem)))

(defgeneric draw-legend (elem)
  (:documentation "handles drawing legends for the given chart")
  (:method ((chart chart))
	   (draw-legend-labels chart)))

(defgeneric legend-start-coords (chart box-size label-spacing)
  (:documentation "specifies where legends should start drawing"))

(defgeneric translate-to-next-label (chart w h)
  (:documentation "translates the active vecto canvas to the next place a label should go")
  (:method ((chart chart) w h)
	   (declare (ignore chart w h))))

(defun draw-legend-labels (chart)
  "handles drawing legend labels"
  (with-graphics-state
    (with-font ()
      (let* ((elems (chart-elements chart))
	     (text-height (default-font-height chart))
	     (box-size (* 3 text-height))
	     (label-spacing (/ box-size 2)))
	(set-font *font* (label-size chart)) ;set the font
	(set-rgb-fill 0 0 0)		;text should be black
	(apply #'translate (legend-start-coords chart box-size label-spacing))
	(dolist (elem elems)
	  ;;translate the origin to the next label
	  (with-graphics-state
	    (set-fill (color elem))
	    (rounded-rectangle 0 0 box-size box-size text-height text-height)
	    (fill-and-stroke))
	  (draw-string (+ box-size label-spacing)
		       text-height
		       (label elem))
	  (translate-to-next-label chart
				   (+ box-size label-spacing label-spacing
				      (default-font-width chart (label elem)))
				   (+ box-size label-spacing)))))))




(defvar *current-chart* nil
  "The currently active chart. Bound for the
      duration of WITH-CHART.")

(defun save-file (filename)
  "saves the *current-chart* to the given file."
  (render-chart *current-chart* filename))
