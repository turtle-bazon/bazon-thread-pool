;;; -*- lisp -*-

(defpackage :ru.bazon.thread-pool
  (:nicknames :bazon-thread-pool :btp)
  (:use :bazon-collections :cl :iterate :bordeaux-threads)
  (:export
   :start-pool
   :stop-pool
   :join-pool
   :execute
   :thread-pool

   :make-fixed-thread-pool
   :make-cached-thread-pool)
  (:documentation "An thread pool system"))
