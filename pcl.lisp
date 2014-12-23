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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun prognate-body (documentationp body)
    (let ((declarations nil)
          (docstring nil)
          (forms nil))
      (loop
         with state = :declarations
         for expression in body
         do (progn
              (when (eql state :declarations)
                (if (and (listp expression) (eql (first expression) 'declare))
                    (push expression declarations)
                    (setf state :docstring)))
              (when (eql state :docstring)
                (if (stringp expression)
                    (setf docstring expression)
                    (setf state :forms)))
              (when (eql state :forms)
                (push expression forms))))
      (setf declarations (nreverse declarations)
            forms (nreverse forms))
      (append declarations
              (if docstring
                  (if documentationp
                      (list docstring (cons 'pcl:progn forms))
                      (list (cons 'pcl:progn (cons docstring forms))))
                  (list (cons 'pcl:progn forms)))))))

(defmacro pcl:let (bindings &body body)
  `(then (on-promises (list ,@(mapcar #'cadr bindings)))
         (lambda (values)
           (destructuring-bind ,(mapcar #'car bindings)
               values
             ,@(prognate-body nil body)))))

(defmacro pcl:let* (bindings &body body)
  (let ((reversed-bindings (reverse bindings))
        (result (prognate-body nil body)))
    (dolist (binding reversed-bindings (first result))
      (setf result `((promise-values-bind (,(car binding))
                        ,(cadr binding)
                      ,@result))))))

(defmacro pcl:handler-case (expression &rest clauses)
  (let ((values (gensym "VALUES-"))
        (clauses (mapcar (lambda (clause)
                           (destructuring-bind (type (&rest vars) &body body)
                               clause
                             (append (list type vars) (prognate-body nil body))))
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
