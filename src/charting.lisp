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

(defparameter +default-colors+ '((1 1 0) ;rgb 0-1
				 (1 0 1)
				 (1 0 0)
				 (0 1 1)
				 (0 1 0)
				 (0 0 1)))
(defvar *default-font-file* "FreeSans.ttf")
(defvar *color-stack* +default-colors+)
(defvar *current-font* nil "a font object")
(defvar *font* nil "a font object")
(defvar *current-chart* nil
  "The currently active chart. Bound for the
      duration of WITH-CHART.")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-font ((&optional font-file) &body body)
    "ensures *font* is a valid font loader."
    `(let ((*font* (or *font* (get-font (or ,font-file (merge-pathnames
							*default-font-file*
							(asdf:component-pathname
							 (asdf:find-system :adw-charting))))))))
      ,@body)))


(defclass area ()
  ((width :accessor width
	  :initarg :width
	  :type integer
	  :initform nil)
   (height :accessor height
	   :initarg :height
	   :type integer
	   :initform nil)))

(defclass point ()
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)))

(defmethod x ((lst list))
  (first lst))
(defmethod y ((lst list))
  (second lst))

(defmethod move ((p point))
  (move-to (x p) (y p)))

(defmethod line ((p point))
  (line-to (x p) (y p)))

(defmethod clone ((p point))
  (make-instance 'point 
		 :x (x p)
		 :y (y p)))
(defun make-point (x y)
  (make-instance 'point :x x :y y))

(defclass chart (area)
  ((label-size :accessor label-size
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
		   :initform nil))
  (:default-initargs :width 200 :height 200))

(defmethod font-bounding-box ((chart chart) text)
  "gets the bounding box for the given text on the given chart."
  (with-font ()
    (string-bounding-box text
			 (label-size chart)
			 *font*)))

(defmethod font-height ((chart chart))
  "gets the pixel height of the default font, at
the size specified in the chart's label-size"
  (aref (font-bounding-box chart "A") 3))

(defmethod font-width ((chart chart) text)
  "gets the pixel width of the default font, as the size
specified in the chart's label-size"
  (aref (font-bounding-box chart text) 2))

(defun %render-chart (&optional (chart *current-chart*))
  (set-fill chart) 
  (clear-canvas);;fills in the background
  
  ;;ensure we have colors to auto-assign
  (let ((*color-stack* (copy-list +default-colors+)))
    (draw-chart chart)
    (when (draw-legend-p chart)
      (draw-legend chart))))

(defgeneric draw-chart (chart)
  (:documentation "draws the chart, assuming a vecto canvas is open"))

(defclass chart-element ()
  ((color :accessor color :initarg :color :initform nil)
   (label :accessor label :initarg :label :initform "none"))
  (:documentation "this is a super-class for various chart elements"))

(defmethod color ((item chart-element))
  (if-let (color (slot-value item 'color))
	  color
	  (let ((c (pop *color-stack*)))
	    (setf *color-stack* (nconc *color-stack*				       
				       (list (mapcar #'(lambda (x)
						   (/ (+ (if (eq 1 x)
							     .7
							     1) x) 2))
					       c))))
	    (setf (color item) c))))

(defgeneric set-fill (obj)
  (:documentation "shortcuts for setting the vecto fill color"))

(defmethod set-fill ((lst cons))
  (apply #'set-rgb-fill lst))

(defmethod set-fill ((chart chart))
  (when-let (bg (background chart))
    (set-fill bg)))

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
  (:documentation "translates the active vecto canvas to the next
place a label should go")
  (:method ((chart chart) w h)
	   (declare (ignore chart w h))))

(defun draw-legend-labels (chart)
  "handles drawing legend labels"
  (with-graphics-state
    (with-font ()
      (let* ((elems (chart-elements chart))
	     (text-height (font-height chart))
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
				      (font-width chart (label elem)))
				   (+ box-size label-spacing)))))))

(defun save-file (filename)
  "saves the *current-chart* to the given file."
  (with-canvas (:width (width *current-chart*) :height (height *current-chart*))
    (setf (chart-elements *current-chart*) (reverse (chart-elements *current-chart*)) )
    (%render-chart)
    (save-png filename)))

(defun save-stream (stream)
  "saves the *current-chart* to the given stream."
  (with-canvas (:width (width *current-chart*) :height (height *current-chart*))
    (%render-chart)
    (save-png-stream stream)))
