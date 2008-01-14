
(defpackage :net.acceleration.charting
    (:documentation "Charting library to make pretty graphs")
    (:nicknames #:adw-charting)
    (:use #:cl #:vecto #:arnesi)
    (:shadow :value)
    (:export
       ;;generic
       :total
       :render-chart
       ;;pie-chart related
       :slice       
       :pie-chart
       :make-slices
       ;;line-chart related
       :series
       :line-chart
       :axis
    ))