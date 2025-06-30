(defpackage #:xlib/tests
  (:use :cl :rt)
  (:export #:run-all-tests #:xlib-test #:xlib-all-tests))
(in-package #:xlib/tests)
(defsuite :xlib)
