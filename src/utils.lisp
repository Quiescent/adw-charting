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
;;pulled from drakma
(defmacro when-let ((var expr) &body body)
  "Evaluates EXPR, binds it to VAR, and executes BODY if VAR has
a true value."
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

(defmacro if-let ((var expr) if-form else-form)
  "Evaluates EXPR, binds it to VAR, and uses
VAR as the first argument to IF, executing the if-form
or else-form depending on VAR"
  `(let ((,var ,expr))
    (if ,var ,if-form ,else-form)))


(defun copy (obj &optional deep)
  (typecase obj
    (standard-object (copy-instance obj deep))
    (hash-table (copy-hashtable obj deep))
    (sequence (copy-seq obj))
    ;; Dont know how to copy it?
    (T obj)))

(defun copy-hashtable (ht &optional deep)
  (let ((new (make-hash-table
	      :test (hash-table-test ht)
	      :size (hash-table-size ht)))) 
    (iter (for (key value) in-hashtable ht)
	  (setf (gethash key new) (if deep
				      (copy value)
				      value)))
    new))

(defun copy-instance (i &optional deep)
  "Copies a clos-instance"
  (loop with i-class = (class-of i)
	with c = (allocate-instance i-class)
	for sd in (closer-mop:class-slots i-class)
	for sn = (closer-mop:slot-definition-name sd)
	when (slot-boundp i sn)
	do (setf (slot-value c sn)
		 (if deep
		     (copy (slot-value i sn) T)
		     (slot-value i sn)))
	finally
	(reinitialize-instance c)
	(return c)))