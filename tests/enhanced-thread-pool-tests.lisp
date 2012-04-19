;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool-tests)

(deftestsuite test-enhanced-thread-pool () ())

(defmacro thread-function (sleep-time lock result addition condition)
  `(lambda ()
     (if ,condition
	 (error "Error in test")
	 (progn 
	   (sleep ,sleep-time)
	   (bordeaux-threads:with-lock-held (,lock)
	     (incf ,result ,addition))))))

(addtest
    test-simple-pooling
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :min-size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (sleep 0.5)
    (stop-pool thread-pool)
    (sleep 0.5)
    (ensure-same 2 result)
    (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (ensure-same t (enhanced-thread-pool::empty-queue-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers-queue)))))

(addtest
    test-simple-pooling-condition
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :min-size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 1 t))
    (sleep 0.5)
    (stop-pool thread-pool)
    (sleep 0.5)
    (ensure-same 1 result)
    (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (ensure-same t (enhanced-thread-pool::empty-queue-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers-queue)))))

(addtest
    test-fixed-thread-pooling
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-fixed-thread-pool "test" :size 2)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 2 lock result 1 nil))
    (execute thread-pool (thread-function 2 lock result 2 nil))
    (sleep 0.2)
    (execute thread-pool (thread-function 2 lock result 3 nil))
    (execute thread-pool (thread-function 2 lock result 4 nil))
    (sleep 0.2)
    (stop-pool thread-pool)
    (sleep 2)
    (ensure-same 3 result)
    (ensure-same 2 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (sleep 2)
    (ensure-same 10 result)
    (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (ensure-same t (enhanced-thread-pool::empty-queue-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers-queue)))))

(addtest
    test-cached-thread-pooling
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-cached-thread-pool "test" :size 2 :max-size 4 :keep-alive-time 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 2 lock result 1 nil))
    (execute thread-pool (thread-function 2 lock result 2 nil))
    (sleep 0.2)
    (execute thread-pool (thread-function 2 lock result 3 nil))
    (execute thread-pool (thread-function 2 lock result 4 nil))
    (execute thread-pool (thread-function 2 lock result 5 nil))
    (sleep 0.2)
    (ensure-same 4 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (sleep 2)
    (ensure-same 2 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (sleep 2)
    (ensure-same 2 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (stop-pool thread-pool)
    (sleep 0.2)
    (ensure-same 15 result)
    (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)))
    (ensure-same t (enhanced-thread-pool::empty-queue-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers-queue)))))