(cl:in-package #:cl-user)

(defpackage #:promises
  (:use #:cl)
  (:export
   ;; runloop
   #:runloop
   #:current-runloop
   #:run-until
   #:add-timer
   #:remove-timer
   #:run-function
   ;; promise
   #:promise
   #:make-promise
   #:then
   #:fulfill
   #:reject
   #:promisep
   #:with-promised-values
   #:on-promises
   #:delayed-promise
   #:with-delay))

(defpackage #:pcl
  (:export #:progn
           #:let
           #:let*
           #:handler-case))

(defpackage #:pcl-impl
  (:use #:cl #:promises))
