(cl:in-package #:pcl-impl)

(defmacro pcl:progn (&body forms)
  (let* ((reversed (reverse forms))
         (result (first reversed)))
    (dolist (form (rest reversed) result)
      (setf result (let ((x (gensym "X-")))
                     `(with-promised-values (&rest ,x)
                          ,form
                        (declare (ignore ,x))
                        ,result))))))

(defmacro pcl:let (bindings &body body)
  `(then (on-promises (list ,@(mapcar #'cadr bindings)))
         (lambda (values)
           (destructuring-bind ,(mapcar #'car bindings)
               values
             (pcl:progn ,@body)))))

(defmacro pcl:let* (bindings &body body)
  (let ((reversed-bindings (reverse bindings))
        (result `(pcl:progn ,@body)))
    (dolist (binding reversed-bindings result)
      (setf result `(with-promised-values (,(car binding))
                        ,(cadr binding)
                      ,result)))))
