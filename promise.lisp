(cl:in-package #:promises)

(deftype promise-state ()
  `(member :unfulfilled :fulfilled :rejected))

(defclass promise ()
  ((state :accessor state :initform :unfulfilled :type promise-state)
   (new-promise :accessor new-promise :initform nil)
   (fulfilled-handler :accessor fulfilled-handler :initform nil)
   (error-handler :accessor error-handler :initform nil)))

(defun make-promise ()
  (make-instance 'promise))

(defun promise-handled-p (promise)
  (not (null (new-promise promise))))

(defun then (promise fulfilled-handler &optional error-handler)
  (assert (not (promise-handled-p promise)))
  (assert (eql (state promise) :unfulfilled))
  (let ((new-promise (make-promise)))
    (setf (new-promise promise) new-promise
          (fulfilled-handler promise) fulfilled-handler
          (error-handler promise) error-handler)
    new-promise))

(defun finish (promise state &rest values)
  (assert (or (eql state :fulfilled) (eql state :rejected)))
  (assert (eql :unfulfilled (state promise)))
  (setf (state promise) state)
  (with-slots (new-promise fulfilled-handler error-handler)
      promise
    (let ((handler (if (eql state :fulfilled) fulfilled-handler error-handler)))
      (cond
        ((and new-promise handler)
         (handler-case
             (destructuring-bind (val1 &rest other-values)
                 (multiple-value-list (apply handler values))
               (if (promisep val1)
                   (then val1
                         (lambda (&rest values)
                           (apply #'fulfill new-promise values))
                         (lambda (error)
                           (funcall #'reject new-promise error)))
                   (apply #'fulfill new-promise val1 other-values)))
           (error (c) (reject new-promise c))))
        (new-promise (apply #'finish new-promise state values))
        ((eql state :rejected) (error (first values)))
        ((eql state :fulfilled) nil)))))

(defun fulfill (promise &rest values)
  (apply #'finish promise :fulfilled values))

(defun reject (promise error)
  (funcall #'finish promise :rejected error))

(defmacro when-fulfilled (promise vars &body body)
  `(then ,promise (lambda ,vars
                    ,@body)))

(defmacro when-rejected (promise vars &body body)
  `(then ,promise nil (lambda ,vars
                        ,@body)))

(defun promisep (x)
  (typep x 'promise))

(defmacro with-promised-values (var-list form &body body)
  (let ((values (gensym "VALUES-")))
    `(multiple-value-call
         (lambda (&rest ,values)
           (if (promisep (first ,values))
               (then (first ,values)
                           (lambda ,var-list
                             ,@body))
               (destructuring-bind ,var-list
                   ,values
                 ,@body)))
       ,form)))

(defun on-promises (values)
  (let* ((empty-token (gensym "EMPTY-"))
         (length (length values))
         (unfilled-count length)
         (result (make-list length :initial-element empty-token))
         (new-promise (make-promise)))
    (flet ((fill-value (value i)
             (setf (elt result i) value)
             (when (zerop (decf unfilled-count))
               (fulfill new-promise result))))
      (do ((i 0 (1+ i)))
          ((= length i))
        (let* ((i i)
               (element (elt values i)))
          (if (promisep element)
              (then element (lambda (x)
                                    (fill-value x i)))
              (fill-value element i)))))
    new-promise))

(defun delayed-promise (delay &optional (runloop (current-runloop)))
  (let ((promise (make-promise)))
    (add-timer runloop
               (lambda ()
                 (fulfill promise))
               delay)
    promise))

(defmacro with-delay ((delay) &body body)
  `(then (delayed-promise ,delay)
         (lambda ()
           ,@body)))
