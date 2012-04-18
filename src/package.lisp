;;; -*- lisp -*-

(defpackage :ru.bazon.enhanced-thread-pool
  (:nicknames :enhanced-thread-pool)
  (:use :cl
	:iterate
	:bordeaux-threads)
  (:export
   :start-pool
   :stop-pool
   :execute
   :thread-pool)
  (:documentation "An enhanced thread pool system"))
