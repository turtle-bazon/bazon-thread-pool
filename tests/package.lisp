;;; -*- lisp -*-

(defpackage :ru.bazon.thread-pool-tests
  (:nicknames :bazon-thread-pool-tests)
  (:use
   :cl
   :bazon-collections
   :bazon-thread-pool
   :bordeaux-threads
   :fiveam
   :iterate)
  (:export
   :run-all-tests)
  (:documentation "An enhanced thread pool system (test package)"))

(in-package :ru.bazon.thread-pool-tests)

(defun ensure-same (expected result &optional &key report)
  (if report
      (is (eq expected result) report)
      (is (eq expected result))))

(defun ensure-null (result &optional &key report)
  (if report
      (is (eq nil result) report)
      (is (eq nil result))))

(defun ensure (result &optional &key report)
  (if report
      (is-true result report)
      (is-true result)))

(def-suite all-tests
  :description "All tests")
