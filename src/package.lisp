;;; -*- lisp -*-

(defpackage :ru.bazon.enhanced-thread-pool
  (:nicknames :enhanced-thread-pool)
  (:use :cl
	:iterate
	:bordeaux-threads)
  (:export
   :start-pool
   :stop-pool
   :join-pool
   :execute
   :thread-pool

   :make-fixed-thread-pool
   :make-cached-thread-pool)
  (:documentation "An enhanced thread pool system"))
