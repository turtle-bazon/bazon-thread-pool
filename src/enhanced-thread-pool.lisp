;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pool-worker ()
  ((lock
    :initform (make-lock "pool-worker lock"))
   (condition
    :initform (make-condition-variable :name "pool-worker condition"))
   (thread)
   (last-used-time
    :type integer
    :initform (get-universal-time))
   (running-p
    :type boolean
    :initform t)))

(defgeneric sleep-down (pool-worker)
  (:documentation ""))

(defgeneric wake-up (pool-worker)
  (:documentation ""))

(defgeneric stop (pool-worker)
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
    :type (integer 1 *)
    :initform 60
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

(defgeneric create-pool-worker (thread-pool)
  (:documentation ""))

(defgeneric start-pool (thread-pool)
  (:documentation ""))

(defgeneric stop-pool (thread-pool)
  (:documentation ""))

(defgeneric execute (thread-pool &rest functions)
  (:documentation ""))

(defgeneric pool-worker-finished (thread-pool pool-worker)
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

(defmethod stop ((pool-worker pool-worker))
  (with-slots (condition running-p)
      pool-worker
    (setf running-p nil)
    (condition-notify condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pool-worker (thread-pool)
  (let ((pool-worker (make-instance 'pool-worker)))
    (with-slots (thread lock last-used-time running-p)
	pool-worker
      (with-slots (name jobs-queue idle-workers-queue
			(thread-pool-running-p running-p))
	  thread-pool
	(setf thread
	      (new-thread (format nil "~a pool worker" name)
		(iter
		  (with-lock-held (lock)
		    (while (and running-p thread-pool-running-p))
		    (sleep-down pool-worker)
		    (iter
		      (for function = (dequeue jobs-queue))
		      (while function)
		      (handler-case
			  (funcall function)
			(error (condition) condition)))
		    (setf last-used-time (get-universal-time))
		    (pool-worker-finished thread-pool pool-worker)))))))
    pool-worker))

(defmethod make-instance :after ((thread-pool thread-pool) &key initargs)
  (with-slots (min-size max-size)
      thread-pool
    (when (and max-size (< max-size min-size))
      (setf max-size min-size))))

(defmethod create-pool-worker ((thread-pool thread-pool))
  (with-slots (idle-workers-queue workers-set)
      thread-pool
    (let ((pool-worker (make-pool-worker thread-pool)))
      (enqueue pool-worker idle-workers-queue)
      (add-object pool-worker workers-set)
      pool-worker)))

(defmethod start-pool ((thread-pool thread-pool))
  (with-slots (name running-p min-size
		    jobs-queue idle-workers-queue workers-set)
      thread-pool
    (unless running-p
      (setf running-p t)
      (iter (for i from 1 to min-size)
	    (collect (create-pool-worker thread-pool))))))

(defmethod stop-pool ((thread-pool thread-pool))
  (with-slots (running-p idle-workers-queue workers-set)
      thread-pool
    (setf running-p nil)
    (each workers-set
	  #'(lambda (pool-worker)
	      (stop pool-worker)))
    (iter (for pool-worker = (dequeue idle-workers-queue))
	  (while pool-worker))))

(defmethod execute ((thread-pool thread-pool) &rest functions)
  (with-slots (jobs-queue idle-workers-queue workers-set max-size)
      thread-pool
    (iter (for function in functions)
	  (enqueue function jobs-queue)
	  (when (and (empty-queue-p idle-workers-queue)
		     (or (not max-size)
			 (< (size workers-set) max-size)))
	    (create-pool-worker thread-pool))
	  (let ((pool-worker (dequeue idle-workers-queue)))
	    (when pool-worker
	      (wake-up pool-worker))))))

(defmethod pool-worker-finished ((thread-pool thread-pool) (pool-worker pool-worker))
  (with-slots (min-size keep-alive-time idle-workers-queue workers-set
			(thread-pool-running-p running-p))
      thread-pool
    (with-slots (running-p)
	pool-worker
      (if (and running-p thread-pool-running-p)
	  (progn
	    (enqueue pool-worker idle-workers-queue)
	    (let ((current-time (get-universal-time)))
	      (when (> (size workers-set) min-size)
		(iter (for pool-worker-candidate = (peek-queue idle-workers-queue))
		      (while (and pool-worker-candidate
				  (> current-time (+ (slot-value pool-worker-candidate 'last-used-time) keep-alive-time))))
		      (let ((pool-worker-actual (dequeue idle-workers-queue)))
			(stop pool-worker-actual)
			(remove-object pool-worker-actual workers-set))))))
	  (progn
	    (stop pool-worker)
	    (remove-object pool-worker workers-set))))))
