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
	       :initarg :chart-type)))

(defvar *chart-types* '((:pie . "p")
			(:pie-3d . "p3")))

(defparameter +google-chart-url+ "http://chart.apis.google.com/chart")

(defmethod single-dataset-p ((chart gchart))
  (member (chart-type chart) '(:pie :pie-3d)))

(defmethod build-data ((chart gchart))
  (when (single-dataset-p chart)
      (format nil
	      "t:~{~F~^,~}"
	      (mapcar #'value
		      (chart-elements chart)))))

(defmethod build-labels ((chart gchart))
  (format nil "~{~a~^|~}"
	  (mapcar #'label
		  (chart-elements chart))))

(defmethod build-parameters ((chart gchart))
  "returns an alist that defines to google what
it should be rendering"
  (list (cons "chs" (format nil "~ax~a"
			    (width chart)
			    (height chart)))
	(cons "cht" (cdr (assoc (chart-type chart)
				*chart-types*)))
	(cons "chd" (build-data chart))
	(cons "chl" (build-labels chart))))

(defmethod save-chart-to-file (filename (chart gchart))
  (with-open-file (dst filename :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (write-sequence (drakma:http-request
		     +google-chart-url+
		     :parameters (build-parameters chart))
		    dst)))


(defmacro with-gchart ((type width height) &body body)
  `(let ((*current-chart*
	  (make-instance 'gchart
			 :chart-type ,type
			 :width ,width
			 :height ,height)))
     ,@body))