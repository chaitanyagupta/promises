(cl:defpackage #:promises-test1
  (:use #:cl #:promises #:promises-test))

(cl:in-package #:promises-test1)

(defun get-tweets-for (user callback)
  (add-timer (current-runloop)
             (lambda ()
               (funcall callback
                        nil
                        (loop repeat (random 10)
                           collect (make-tweet :user user
                                               :text (random-element *tweet-texts*)
                                               :url (random-element *urls*)))))
             2))

(defun expand-url-using-twitter-api (url callback)
  (add-timer (current-runloop)
             (lambda ()
               (funcall callback
                        nil
                        (concatenate 'string url "-expanded")))
             1))

(defun http-get (url callback)
  (add-timer (current-runloop)
             (lambda ()
               (funcall callback
                        nil
                        (format nil "The URL is: ~A" url)))
             2))

(defun run ()
  (get-tweets-for "foo"
                  (lambda (error tweets)
                    (declare (ignore error))
                    (let ((short-urls (parse-tweets-for-urls tweets)))
                      (expand-url-using-twitter-api (elt short-urls 0)
                                                    (lambda (error url)
                                                      (declare (ignore error))
                                                      (http-get url (lambda (error body)
                                                                      (declare (ignore error))
                                                                      (format t "Most recent link text: ~A~%" body)))))))))

