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
   #:promisep
   #:then
   #:fulfill
   #:reject
   #:promise-values
   #:promise-condition
   #:promise-values-bind
   #:on-promises
   #:delayed-promise
   #:with-delay))

(defpackage #:pcl
  (:export #:progn
           #:let
           #:let*
           #:handler-case
           #:print))

(defpackage #:pcl-impl
  (:use #:cl #:promises))
