;;reads a darcs changes file and generates a
;;graph of contributions by day

(defpackage #:net.acceleration.darcs-changes
  (:use #:cl))

(in-package #:net.acceleration.darcs-changes)

(defparameter *data* nil)

(defun date-string-to-day (date-string)
  "picks month,year,day from a darcs date, returns a universal time"
  (let ((year (parse-integer date-string :end 4))
	(month (parse-integer date-string :start 4 :end 6))
	(day (parse-integer date-string :start 6 :end 8)))
    (encode-universal-time 0 0 0 day month year)))

(defun date-string-to-month (date-string)
  "picks month/year out of a darcs date, assumes 1 for the day, returns a
universal time"
  (let ((year (parse-integer date-string :end 4))
	(month (parse-integer date-string :start 4 :end 6)))
    (encode-universal-time 0 0 0 1 month year)))

(defun add-contrib (author date)
  "adds the author's contribution on the given date to the *data*"
  ;;pick out the first bit of email from the darcs author
  (cl-ppcre:do-register-groups (username) ("([^<@\\s]+)@" author)
    ;;ensure we have an entry for this author in the hashtable
    (when (null (gethash username *data*))
      (setf (gethash username *data*) (make-hash-table)))
    ;;increment the count for this date
    (incf (gethash date
		   (gethash username *data*)
		   0))))

(defun patches-per-day (&optional (filename #p"source/adw-charting/examples/darcs-changes.xml"))
  (make-chart filename #'print-day #'date-string-to-day))

(defun patches-per-month (&optional (filename #p"darcs-changes.xml"))
  (make-chart filename #'print-month #'date-string-to-month))

(defun make-chart (filename x-label-formatter date-fn)
  (let ((*data* (make-hash-table :test 'equal)))
    (parse-xml filename date-fn)
    
    (adw-charting:with-bar-chart (400 400 :background '(.5 .5 .5))
      (dolist (series (get-chart-data))
	(apply #'adw-charting:add-series series))
      
      (adw-charting:set-axis :y "patches")
      (adw-charting:set-axis :x nil
			     :draw-gridlines-p nil
			     :label-formatter x-label-formatter)
      (adw-charting:save-file "darcs.png")
      (get-chart-data))))

(defun get-chart-data ()
  (sort (loop for author being the hash-keys in *data*
	   using (hash-value contribs)
	   collect (list author
			 (sort
			  (loop for k being the hash-keys in contribs
			     using (hash-value v)
			     collect (list k v))
			  ;;sort the patches by date
			  #'< :key #'first)))
	;;sort by total patches
	#'>
	:key #'(lambda (i)
		 (reduce #'+ (second i) :key #'second))))

(defun parse-xml (filename date-fn)
  "parse the darcs xml file, calls add-contrib when it finds a contribution"
  (klacks:with-open-source
      (s (cxml:make-source filename))
    (loop
       for key = (klacks:peek s)
       while key
       do
       (when (and (eql key :start-element)
		  (string= (klacks:current-qname s) "patch"))
	 (let (author date)
	   (klacks:map-attributes
	    #'(lambda (_ local-name __ attribute-value ___)
		(declare (ignore _ __ ___))
		(when (string= local-name "author")
		  (setf author attribute-value))
		(when (string= local-name "date")
		  (setf date (funcall date-fn attribute-value))))
	    s)
	   (add-contrib author date)))
       (klacks:consume s))))




(defun print-time (v t1 t2)
  (let ((time-list (multiple-value-list
		    (decode-universal-time v))))
    (format nil "~a/~a"
	    (nth t1 time-list)
	    (nth t2 time-list))))

(defun print-month (v)
  (print-time v 4 5))

(defun print-day (v)
  (print-time v 4 3))