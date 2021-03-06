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

(defpackage :net.acceleration.charting
    (:documentation "Charting library to make pretty graphs")
    (:nicknames #:adw-charting)
    (:use #:cl #:iterate)
    (:export
       :save-file
       :save-stream
       :add-slice
       :add-series
       :set-axis
       #:color
       #:add-marker
       #:add-legend
       #:chart-elements
       #:*current-chart*
       #:label
       #:label-formatter
       #:series
       #:x-axis
       #:x2-axis
       #:y-axis
       #:data-interval
       #:data))