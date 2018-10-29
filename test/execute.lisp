(defpackage :cl-lua-test.execute
  (:use :cl :cl-lua)
  (:export :test))
(in-package :cl-lua-test.execute)

(defun test ()
  (prove:plan 1)
  (prove:is (run-string "x=3; return _ENV['x']") 3)
  (prove:is (run-string "x=3; return _ENV[ [[x]] ]") 3)
  (prove:finalize))
