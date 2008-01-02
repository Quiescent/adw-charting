
(defpackage :net.acceleration.charting
    (:documentation "Charting library to make pretty graphs")
    (:nicknames #:adw-charting)
    (:use #:cl #:vecto #:arnesi)
    (:shadow :value)
    (:export
       ;;functions
       :total
       :render-chart
       :make-items
       ;;classes
       :data-item
       :pie-chart     
    ))