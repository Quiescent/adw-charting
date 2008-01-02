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
   (label-size :accessor label-size
	       :initarg :label-size
	       :initform 12)
   (margin :accessor margin
	   :initarg :margin
	   :initform 10)
   (background :accessor background
	       :initarg :background
	       :initform '(1 1 1))))


(defgeneric render-chart (chart filename)
  (:documentation "renders the chart to the given file"))

(defgeneric draw-chart (chart)
  (:documentation "draws the chart, assuming a vecto canvas is open"))

(defmethod render-chart ((chart chart) filename)
  (with-canvas (:width (width chart) :height (height chart))
    (set-fill chart)
    (clear-canvas)
    (setq *color-stack* +default-colors+) ;ensure we have colors to auto-assign
    (draw-chart chart)
    (save-png filename)))


(defclass chart-element ()
  ((color :accessor color :initarg :color :initform nil)
   (label :accessor label :initarg :label :initform "none"))
  (:documentation "this is a super-class for various chart elements")
  )

(defmethod color ((item chart-element))
  (aif (slot-value item 'color)
       it
       (pop *color-stack*)))


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

(defgeneric draw-legend (chart)
  (:documentation "handles drawing legends for the given chart"))