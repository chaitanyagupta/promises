(cl:defpackage #:promises-test2
  (:use #:cl #:promises #:promises-test))

(cl:in-package #:promises-test2)

(defun get-tweets-for (user)
  (with-delay (2)
    (if (userp user)
        (loop repeat (random 10)
           collect (make-tweet :user user
                               :text (random-element *tweet-texts*)
                               :url (random-element *urls*)))
        (error "Couldn't find user: ~A" user))))

(defun expand-url-using-twitter-api (url)
  (with-delay (1)
    (concatenate 'string url "-expanded")))

(defun http-get (url)
  (with-delay (2)
    (format nil "The URL is: ~A" url)))

(defun run ()
  (let* ((p1 (then (get-tweets-for "foo")
                   (lambda (tweets)
                     (let ((short-urls (parse-tweets-for-urls tweets)))
                       (expand-url-using-twitter-api (elt short-urls 0))))))
         (p2 (then p1 #'http-get)))
    (then p2
          (lambda (body)
            (format t "Most recent link text: ~A~%" body))
          (lambda (error)
            (format t "Got an error: ~A" error)))))
