(cl:in-package #:promises)

(defclass thendler ()
  ((promise :reader thendler-promise :initform (make-promise))
   (fulfiller :reader thendler-fulfiller :initarg :fulfiller)
   (rejecter :reader thendler-rejecter :initarg :rejecter)))

(defun finish-thendler (thendler handler &rest values)
  (let ((thendler-promise (thendler-promise thendler)))
    (handler-case
        (let* ((new-values (multiple-value-list (apply handler values)))
               (first-new-value (first new-values)))
          (if (promisep first-new-value)
              (then first-new-value
                    (lambda (&rest values)
                      (apply #'fulfill thendler-promise values))
                    (lambda (condition)
                      (reject thendler-promise condition)))
              (apply #'fulfill thendler-promise new-values)))
      (condition (c)
        (reject thendler-promise c)))))

(defmethod fulfill ((thendler thendler) &rest values)
  (let ((fulfiller (thendler-fulfiller thendler)))
    (if (null fulfiller)
        (apply #'fulfill (thendler-promise thendler) values)
        (apply #'finish-thendler thendler fulfiller values))))

(defmethod reject ((thendler thendler) condition)
  (let ((rejecter (thendler-rejecter thendler)))
    (if (null rejecter)
        (funcall #'reject (thendler-promise thendler) condition)
        (finish-thendler thendler rejecter condition))))

(defclass promise ()
  ((values :accessor promise-values :initform nil :type (or null list))
   (condition :accessor promise-condition :initform nil :type (or null condition))
   (thendlers :accessor promise-thendlers :initform nil :type (or null list))))

(defmethod print-object ((promise promise) stream)
  (print-unreadable-object (promise stream :type t :identity t)
    (cond
      ((fulfilledp promise)
       (format stream "Ful: ~A" (promise-values promise)))
      ((rejectedp promise)
       (format stream "Rej: ~A" (promise-condition promise)))
      (t (format stream "Un")))))

(defun fulfilledp (promise)
  (not (null (promise-values promise))))

(defun rejectedp (promise)
  (not (null (promise-condition promise))))

(defun make-promise ()
  (make-instance 'promise))

(defun then (promise fulfiller &optional rejecter)
  (let ((thendler (make-instance 'thendler
                                 :fulfiller fulfiller
                                 :rejecter rejecter)))
    (push thendler (promise-thendlers promise))
    (cond
      ((fulfilledp promise) (apply #'fulfill thendler (promise-values promise)))
      ((rejectedp promise) (reject thendler (promise-condition promise))))
    (thendler-promise thendler)))

(defmethod fulfill ((promise promise) &rest values)
  (setf (promise-values promise) values)
  (dolist (thendler (promise-thendlers promise))
    (apply #'fulfill thendler values)))

(defmethod reject ((promise promise) condition)
  (let ((thendlers (promise-thendlers promise)))
    (setf (promise-condition promise) condition)
    (when (null thendlers)
      (error condition))
    (dolist (thendler (promise-thendlers promise))
      (reject thendler condition))))

(defmacro when-fulfilled (promise vars &body body)
  `(then ,promise (lambda ,vars
                    ,@body)))

(defmacro when-rejected (promise vars &body body)
  `(then ,promise nil (lambda ,vars
                        ,@body)))

(defun promisep (x)
  (typep x 'promise))

(defmacro promise-values-bind (var-list form &body body)
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
