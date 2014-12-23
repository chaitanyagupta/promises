# Promises in Common Lisp

Companion source code for the post
[Promises and Lisp](http://lisper.in/promises). Contains a
not-meant-for-production implementation of promises in
[promise.lisp](promise.lisp) and promises based CL counterparts in
[pcl.lisp](pcl.lisp).

Install using quicklisp by symlinking the project directory inside
`~/quicklisp/local-projects/` and then run `(ql:quickload :promises)`.

Runloop facilities are provided by
[iolib](http://common-lisp.net/project/iolib/).

Contains no networking code, all async calls (e.g. `get-tweets-for`,
`http-get`, etc.) are stubbed using runloop timers.

`test[123].lisp` contain some example source code to run.

You can run the test code like this:

```lisp
(in-package :promises-test3)
(run-function (make-instance 'runloop) 7 #'run "foo")
```
