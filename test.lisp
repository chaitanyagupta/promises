(cl:defpackage #:promises-test
  (:use #:cl #:promises)
  (:export #:userp
           #:make-tweet
           #:tweet
           #:*tweet-texts*
           #:*urls*
           #:random-element
           #:parse-tweets-for-urls))

(cl:in-package #:promises-test)

(defparameter *users*
  (list "foo" "bar" "baz"))

(defun userp (name)
  (find name *users* :test #'string=))

(defstruct tweet
  user
  text
  url)

(defparameter *tweet-texts*
  '("foo bar baz"
    "lorem ipsum dolor"
    "blah zzz"
    "gaaah"))

(defparameter *urls*
  '("http://t.co/foo"
    "http://t.co/bar"))

(defun random-element (sequence)
  (elt sequence (random (length sequence))))

(defun parse-tweets-for-urls (tweets)
  (mapcar #'tweet-url (remove-if #'null tweets :key #'tweet-url)))
