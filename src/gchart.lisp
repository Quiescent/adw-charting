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

(defclass gchart (chart)
  ((chart-type :accessor chart-type
	       :initarg :chart-type)
   (parameters :accessor parameters
	       :initform (make-hash-table :test 'equal)
	       :initarg :parameters)))

(defvar *chart-types* '((:pie . "p")
			(:pie-3d . "p3")
			(:line . "lxy")))

(defparameter +google-chart-url+ "http://chart.apis.google.com/chart")

(defmethod single-dataset-p ((chart gchart))
  "does this chart have a single dataset, or many?"
  (member (chart-type chart) '(:pie :pie-3d)))

(defmethod build-data ((chart gchart))
  "helper to build the list of data"
  (case (chart-type chart)
	((:pie :pie-3d) (format nil
				"t:~{~F~^,~}"
				(mapcar #'value
					(chart-elements chart))))
	(:line
	 ;;pairs of X | Y, normalized to
	 (format nil "t:~{~a~^|~}"
		 (loop for (exes wyes) in (normalized-series chart)
		    collect (format nil
				    "~{~,2F~^,~}|~{~,2F~^,~}"
				    exes wyes))))))

(defun interpolate (min max val)
  (* 100 (/ (- val min)
	    (- max min))))

(defun normalized-series (chart)
  (destructuring-bind ((min-x min-y) (max-x max-y))
      (find-chart-extremes chart)
    (loop for series in (chart-elements chart)
       for exes = nil then nil
       for wyes = nil then nil
       do
	 (loop for (x y) in (reverse (data series))
	    do
	      (push (interpolate min-x max-x x) exes)
	      (push (interpolate min-y max-y y) wyes))
       collect (list exes wyes))))

(defmethod build-labels ((chart gchart))
  "helper to build the list of labels"
  (format nil "~{~a~^|~}"
	  (mapcar #'label
		  (chart-elements chart))))

(defmethod set-parameter ((chart gchart) key value)
  (setf (gethash (string-downcase (princ-to-string key))
		 (parameters chart))
	value))

(defmacro set-parameters ((chart) &rest params)
  `(progn
    ,@(loop for (k v) in params
     collect 
       `(set-parameter ,chart ,k ,v)))
  )

(defmethod ensure-default-parameters ((chart gchart))
  (set-parameters (chart)
		  (:chs (format nil "~ax~a"
			       (width chart)
			       (height chart)))
		  (:cht (cdr (assoc (chart-type chart)
				   *chart-types*)))
		  (:chd (build-data chart))))


(defgeneric add-feature (feature-name))

(defmethod add-feature ((feature-name (eql :label)))
  (set-parameter *current-chart* (case (chart-type *current-chart*)
				   ((:pie :pie-3d) "chl")
				   (:line "chdl"))
		 (build-labels *current-chart*)))

(defun add-features (&rest names)
  (mapc #'add-feature names))

(defmethod build-parameters ((chart gchart))
  "returns an alist that defines to google what
it should be rendering"
  (loop for k being the hash-keys in (parameters chart) using (hash-value v)
       collect (cons k v)))

(defmethod save-chart-to-file (filename (chart gchart))
  "makes the call to google, saves the result in the file"
  (ensure-default-parameters chart)
  (with-open-file (dst filename :direction :output
		       :element-type 'unsigned-byte
		       :if-does-not-exist :create
		       :if-exists :supersede)
    (write-sequence (drakma:http-request
		     +google-chart-url+
		     :parameters (build-parameters chart))
		    dst)
    (truename filename)))

(defun chart-url ()
  (ensure-default-parameters *current-chart*)
  "returns the URL for the current google chart"
  (concatenate 'string
	       +google-chart-url+
	       "?"
	       (drakma::alist-to-url-encoded-string
		(build-parameters *current-chart*)
		drakma:*drakma-default-external-format*)))

(defmacro with-gchart ((type width height) &body body)
  "creates a new context with a gchart of the given type, width, and height."
  `(let ((*current-chart*
	  (make-instance 'gchart
			 :chart-type ,type
			 :width ,width
			 :height ,height)))
     ,@body))