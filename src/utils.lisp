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