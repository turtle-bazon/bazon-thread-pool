;;; -*- lisp -*-

(in-package :ru.bazon.thread-pool-tests)

(defmacro thread-function (sleep-time lock result addition condition)
  (let ((addition-real addition))
    `(lambda ()
       (if ,condition
	   (error "Error in test")
	   (progn
	     (sleep ,sleep-time)
	     (bordeaux-threads:with-lock-held (,lock)
	       (incf ,result ,addition-real)))))))

(in-suite all-tests)

(test test-simple-pooling
  (let ((threads-before-start (length (bordeaux-threads:all-threads)))
	(result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 2 nil))
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 3 result :report "result")
    (ensure-same 0 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads))) :report "threads"))

(test test-simple-pooling-condition
  (let ((threads-before-start (length (bordeaux-threads:all-threads)))
	(result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 1 t))
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 1 result)
    (ensure-same 0 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads)) :report "threads")))

(test test-fixed-thread-pooling
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
    (ensure-same 2 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size-1")
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 10 result :report "result-2")
    (ensure-same 0 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size-2")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads)) :report "threads")))

(test test-cached-thread-pooling
  (format t "~&Begin test test-cached-thread-pooling~&")
  (let ((result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-cached-thread-pool "test" :size 2 :max-size 4 :keep-alive-time 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 2 lock result 1 nil))
    (execute thread-pool (thread-function 2 lock result 2 nil))
    (execute thread-pool (thread-function 2 lock result 3 nil))
    (execute thread-pool (thread-function 2 lock result 4 nil))
    (sleep 5)
    (ensure-same 4 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size-1")
    (execute thread-pool (thread-function 2 lock result 5 nil))
    (ensure-same 2 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size-2")
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 15 result :report "result")
    (ensure-same 0 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                 :report "workers-size-4")))

(test test-agressive-pooling
  (format t "~&Begin test test-agressive-pooling~&")
  (iter (for i from 1 to 10)
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
	  (ensure-same 0 (size (slot-value thread-pool 'bazon-thread-pool::workers-set))
                       :report "workers-size"))))