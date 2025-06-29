(defpackage #:xlib/tests
  (:use :cl)
  (:export #:run-all-tests #:xlib-test #:xlib-all-tests))
(in-package #:xlib/tests)
(rt:defsuite :xlib)
