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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pcl-prognate-body (documentationp body)
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
             ,@(pcl-prognate-body nil body)))))

(defmacro pcl:let* (bindings &body body)
  (let ((reversed-bindings (reverse bindings))
        (result (pcl-prognate-body nil body)))
    (dolist (binding reversed-bindings (first result))
      (setf result `((with-promised-values (,(car binding))
                        ,(cadr binding)
                      ,@result))))))
