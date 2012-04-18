;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool-tests)

(deftestsuite test-enhanced-thread-pool () ())

(defmacro thread-function (sleep-time lock result addition)
  `(lambda ()
     (sleep ,sleep-time)
     (bordeaux-threads:with-lock-held (,lock)
       (incf ,result ,addition))))

(addtest
    test-simple-pooling
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :min-size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1))
    (execute thread-pool (thread-function 0 lock result 1))
    (stop-pool thread-pool)
    (sleep 1)
    (ensure-same 2 result)
    (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (ensure-same t (enhanced-thread-pool::empty-queue-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers-queue)))))
