(cl:defpackage #:promises-test2
  (:use #:cl #:promises #:promises-test)
  (:export #:get-tweets-for
           #:expand-url-using-twitter-api
           #:http-get))

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

(defun run-1 (user)
  (then (then (then (get-tweets-for user)
                    (lambda (tweets)
                      (let ((short-urls (parse-tweets-for-urls tweets)))
                        (expand-url-using-twitter-api (elt short-urls 0)))))
              #'http-get)
        (lambda (response-body)
          (format t "Most recent link text: ~A~%" response-body))
        (lambda (error)
          (format t "Got an error: ~A~%" error))))

(defun run-2 (user)
  (let* ((tweets-promise (get-tweets-for user))
         (short-urls-promise (then tweets-promise (lambda (tweets)
                                                    (parse-tweets-for-urls tweets))))
         (expanded-url-promise (then short-urls-promise
                                     (lambda (short-urls)
                                       (expand-url-using-twitter-api (elt short-urls 0)))))
         (response-body-promise (then expanded-url-promise #'http-get)))
    (then response-body-promise
          (lambda (response-body)
            (format t "Most recent link text: ~A~%" response-body))
          (lambda (error)
            (format t "Got an error: ~A~%" error)))))
