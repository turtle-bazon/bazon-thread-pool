;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool-tests)

(deftestsuite test-enhanced-thread-pool () ())

(defmacro thread-function (sleep-time lock result addition condition)
  (let ((addition-real addition))
    `(lambda ()
       (if ,condition
	   (error "Error in test")
	   (progn
	     (sleep ,sleep-time)
	     (bordeaux-threads:with-lock-held (,lock)
	       (incf ,result ,addition-real)))))))

#+nil(addtest
    test-simple-pooling
  (let ((threads-before-start (length (bordeaux-threads:all-threads)))
	(result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :min-size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 2 nil))
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 3 result :report "result")
    (ensure-same 0 (slot-value thread-pool 'enhanced-thread-pool::workers-count) :report "workers-size")
    (ensure-same t (enhanced-thread-pool::empty-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers)) :report "pool-empty")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads))) :report "threads"))

#+nil(addtest
    test-simple-pooling-condition
  (let ((threads-before-start (length (bordeaux-threads:all-threads)))
	(result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :min-size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 1 t))
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 1 result)
    (ensure-same 0 (slot-value thread-pool 'enhanced-thread-pool::workers-count) :report "workers-size")
    (ensure-same t (enhanced-thread-pool::empty-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers)) :report "pool-empty")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads)) :report "threads")))

#+nil(addtest
    test-fixed-thread-pooling
  (let ((threads-before-start (length (bordeaux-threads:all-threads)))
	(result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-fixed-thread-pool "test" :size 2)))
    (start-pool thread-pool)
    (sleep 0.2)
    (execute thread-pool (thread-function 2 lock result 1 nil))
    (execute thread-pool (thread-function 2 lock result 2 nil))
    (sleep 0.2)
    (execute thread-pool (thread-function 2 lock result 3 nil))
    (execute thread-pool (thread-function 2 lock result 4 nil))
    (sleep 2)
    (ensure-same 3 result :report "result-1")
    (ensure-same 2 (slot-value thread-pool 'enhanced-thread-pool::workers-count) :report "workers-size-1")
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 10 result :report "result-2")
    (ensure-same 0 (slot-value thread-pool 'enhanced-thread-pool::workers-count) :report "workers-size-2")
    (ensure-same t (enhanced-thread-pool::empty-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers)) :report "pool-empty")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads)) :report "threads")))

#+nil(addtest
    test-cached-thread-pooling
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-cached-thread-pool "test" :size 2 :max-size 4 :keep-alive-time 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 2 lock result 1 nil))
    (execute thread-pool (thread-function 2 lock result 2 nil))
    (execute thread-pool (thread-function 2 lock result 3 nil))
    (execute thread-pool (thread-function 2 lock result 4 nil))
    (execute thread-pool (thread-function 2 lock result 5 nil))
    (sleep 2.5)
    (ensure-same 4 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)) :report "workers-size-1")
    ;(sleep 2)
    ;(ensure-same 2 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)) :report "workers-size-2")
    ;(sleep 2)
    ;(ensure-same 2 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)) :report "workers-size-3")
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 15 result :report "result")
    (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)) :report "workers-size-4")
    (ensure-same 0 (length (slot-value thread-pool 'enhanced-thread-pool::idle-workers)) :report "pool-empty")))

(addtest
    test-agressive-pooling
  (iter (for i from 1 to 1)
	(let ((stress-size 1024)
	      (result 0)
	      (lock (bordeaux-threads:make-lock))
	      (thread-pool (make-cached-thread-pool "test" :size 16 :max-size 24 :keep-alive-time 1)))
	  (start-pool thread-pool)
	  (iter (for i from 1 to stress-size)
		(execute thread-pool (thread-function 0.01 lock result 1 nil)))
	  (stop-pool thread-pool)
	  (join-pool thread-pool)
	  (ensure-same stress-size result :report "result")
	  (ensure-same 0 (enhanced-thread-pool::size (slot-value thread-pool 'enhanced-thread-pool::workers-set)) :report "workers-size")
	  (ensure-same 0 (length (slot-value thread-pool 'enhanced-thread-pool::idle-workers)) :report "pool-empty"))))









