(cl:defpackage #:promises-test3
  (:use #:cl #:promises #:promises-test #:promises-test2))

(cl:in-package #:promises-test3)

(defun run (user)
  (pcl:handler-case
      (pcl:let* ((tweets (get-tweets-for user))
                 (short-urls (parse-tweets-for-urls tweets))
                 (expanded-url (expand-url-using-twitter-api (elt short-urls 0)))
                 (body (http-get expanded-url)))
        (format t "Most recent link text: ~A~%" body))
    (error (c) (format t "Got error: ~A~%" c))))
