;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pool-worker ()
  ((lock
    :initform (make-lock "pool-worker lock"))
   (condition
    :initform (make-condition-variable :name "pool-worker condition"))
   (thread)
   (running-p
    :type boolean
    :initform t)))

(defgeneric sleep-down (pool-worker)
  (:documentation ""))

(defgeneric wake-up (pool-worker)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass thread-pool ()
  ((name
    :initarg :name)
   (min-size
    :type integer
    :initarg :min-size
    :initform 1)
   (max-size
    :type (or integer null)
    :initarg :max-size
    :initform 1)
   (keep-alive-time
    :type integer
    :initarg :keep-alive-time)
   (main-lock
    :initform (make-lock "thread-pool main lock"))
   (jobs-queue
    :type blocking-queue
    :initform (make-blocking-queue))
   (idle-workers-queue
    :type blocking-queue
    :initform (make-blocking-queue))
   (workers-set
    :type blocking-hash-set
    :initform (make-blocking-hash-set))
   (running-p
    :type boolean
    :initform nil)))

(defgeneric start-pool (thread-pool)
  (:documentation ""))

(defgeneric stop-pool (thread-pool)
  (:documentation ""))

(defgeneric execute (thread-pool &rest functions)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sleep-down ((pool-worker pool-worker))
  (with-slots (lock condition)
      pool-worker
    (condition-wait condition lock)))

(defmethod wake-up ((pool-worker pool-worker))
  (with-slots (condition)
      pool-worker
    (condition-notify condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pool-worker (thread-pool)
  (let ((pool-worker (make-instance 'pool-worker)))
    (with-slots (thread lock running-p)
	pool-worker
      (with-slots (name jobs-queue idle-workers-queue)
	  thread-pool
	(setf thread
	      (new-thread (format nil "~a pool worker" name)
		(iter
		  (with-lock-held (lock)
		    (while running-p)
		    (sleep-down pool-worker)
		    (iter
		      (for function = (dequeue jobs-queue))
		      (while function)
		      (handler-case
			  (funcall function)
			(error (condition) condition)))
		    (enqueue pool-worker idle-workers-queue)))))))
    pool-worker))

(defmethod start-pool ((thread-pool thread-pool))
  (with-slots (name running-p min-size
		    jobs-queue idle-workers-queue workers-set)
      thread-pool
    (unless running-p
      (setf running-p t)
      (iter (for i from 1 to min-size)
	    (collect
		(let ((pool-worker (make-pool-worker thread-pool)))
		  (enqueue pool-worker idle-workers-queue)
		  (add-object pool-worker workers-set)
		  pool-worker))))))

(defmethod stop-pool ((thread-pool thread-pool))
  (with-slots (running-p idle-workers-queue workers-set)
      thread-pool
    (setf running-p nil)
    (each workers-set
	  #'(lambda (pool-worker)
	      (with-slots (condition running-p)
		  pool-worker
		(setf running-p nil)
		(condition-notify condition))))
    (iter (for pool-worker = (dequeue idle-workers-queue))
	  (while pool-worker))))

(defmethod execute ((thread-pool thread-pool) &rest functions)
  (with-slots (jobs-queue idle-workers-queue workers-set max-size)
      thread-pool
    (iter (for function in functions)
	  (enqueue function jobs-queue)
	  (let ((pool-worker (dequeue idle-workers-queue)))
	    (if pool-worker
		(wake-up pool-worker)
		(when (or (not max-size)
			  (< (size workers-set) max-size))
		  (let ((new-pool-worker (make-pool-worker thread-pool)))
		    (add-object new-pool-worker workers-set)
		    (wake-up new-pool-worker))))))))
