(cl:in-package #:promises)

(defvar *current-runloop*)

(defclass runloop ()
  ((event-base :initarg :event-base
               :accessor event-base
               :initform (make-instance 'iomux:event-base))))

(defun current-runloop ()
  *current-runloop*)

(defmethod run-until ((runloop runloop) timeout)
  (iomux:event-dispatch (event-base runloop) :timeout timeout))

(defmethod add-timer ((runloop runloop) function timeout &key repeat)
  (iomux:add-timer (event-base runloop)
                   (lambda (&aux (*current-runloop* runloop))
                     (funcall function))
                   timeout
                   :one-shot (not repeat)))

(defmethod remove-timer ((runloop runloop) timer)
  (iomux:remove-timer (event-base runloop) timer))

(defun run-function (runloop timeout fn &rest values)
  (add-timer runloop (lambda () (apply fn values)) 0)
  (run-until runloop timeout))
