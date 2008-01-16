
(defpackage :net.acceleration.charting
    (:documentation "Charting library to make pretty graphs")
    (:nicknames #:adw-charting)
    (:use #:cl #:vecto #:arnesi)
    (:shadow :value)
    (:export
       ;;generic
       :total
       :render-chart
       :save-file
       ;;pie-chart related
       :slice       
       :pie-chart
       :make-slices
       ;;line-chart related
       :series
       :make-series
       :line-chart
       :make-line-chart
       :axis
       :make-axis
       :with-line-chart
       :add-series
       :set-axis
    ))