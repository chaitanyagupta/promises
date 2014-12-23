(cl:in-package #:pcl-impl)

(defmacro pcl:progn (&body forms)
  (cond ((null forms) nil)
        ((null (rest forms))
         (first forms))
        (t (let ((x (gensym "X-")))
             `(promise-values-bind (&rest ,x)
                  ,(first forms)
                (declare (ignore ,x))
                (pcl:progn ,@(rest forms)))))))

(defmacro pcl:let (bindings &body body)
  `(then (on-promises (list ,@(mapcar #'cadr bindings)))
         (lambda (values)
           (destructuring-bind ,(mapcar #'car bindings)
               values
             ,@body))))

(defmacro pcl:let* (bindings &body body)
  (if (null bindings)
      `(pcl:progn ,@body)
      (let ((binding (first bindings)))
        `(promise-values-bind (,(first binding))
             ,(second binding)
           (pcl:let* ,(rest bindings)
             ,@body)))))

(defmacro pcl:handler-case (expression &rest clauses)
  (let ((values (gensym "VALUES-"))
        (clauses (mapcar (lambda (clause)
                           (destructuring-bind (type (&rest vars) &body body)
                               clause
                             (list type vars (cons 'pcl:progn body))))
                         clauses)))
    `(flet ((handle-condition (c)
              (typecase c
                ,@(mapcar (lambda (clause)
                            (destructuring-bind (type (&rest vars) &body body)
                                clause
                              (list type `((lambda (,@vars &rest ,values)
                                             (declare (ignore ,values))
                                             ,@body)
                                           c))))
                          clauses))))
       (handler-case
           (multiple-value-call
               (lambda (&rest ,values)
                 (if (promisep (first ,values))
                     (then (first ,values)
                           nil
                           #'handle-condition)
                     (values-list ,values)))
             ,expression)
         ,@clauses))))

(defun promisify (fn)
  (lambda (&rest values)
    (if (promisep (first values))
        (then (first values) fn)
        (apply fn values))))

(setf (fdefinition 'pcl:print) (promisify #'print))
