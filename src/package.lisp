;;; -*- lisp -*-

(defpackage :ru.bazon.enhanced-thread-pool
  (:nicknames :enhanced-thread-pool)
  (:use :cl
	:iterate
	:bordeaux-threads)
  (:export
   :make-thread-pool
   :start-pool
   :stop-pool
   :add-to-pool
   :pool-size
   :pool-name)
  (:documentation "An enhanced thread pool system"))
