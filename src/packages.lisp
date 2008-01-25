
(defpackage :net.acceleration.charting
    (:documentation "Charting library to make pretty graphs")
    (:nicknames #:adw-charting)
    (:use #:cl #:vecto)
    (:export
       ;;generic
       :save-file
       :save-stream
       ;;pie-chart related
       :with-pie-chart
       :add-slice
       ;;line-chart related
       :with-line-chart
       :add-series
       :set-axis
    ))