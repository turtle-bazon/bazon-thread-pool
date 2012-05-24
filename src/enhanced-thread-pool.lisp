;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pool-worker ()
  ((execute-function
    :initform nil
    :documentation "Function to be executed in thread")
   (lock
    :initform (make-lock "pool-worker lock")
    :documentation "Worker lock (used in condition-wait)")
   (condition
    :initform (make-condition-variable :name "pool-worker condition")
    :documentation "Condition variable (used in condition-wait)")
   (thread
    :documentation "Thread, that holds parallel process to execute code")
   (last-used-time
    :type integer
    :initform (get-universal-time)
    :documentation "Worker's last used time (to determine inactive workers)")
   (running-p
    :type boolean
    :initform t
    :documentation "Boolean determines is worker runing or should be stopped")))

(defgeneric sleep-down (pool-worker)
  (:documentation "Set worker to sleep state until any code wake up it"))

(defgeneric wake-up (pool-worker function)
  (:documentation "Wake up sleeping worker"))

(defgeneric stop (pool-worker)
  (:documentation "Stop execution of worker"))

(defgeneric join-worker-thread (pool-worker)
  (:documentation "Join to worker thread and await for it's termination"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass thread-pool ()
  ((name
    :initarg :name
    :initform (error "Name must be provided")
    :documentation "Thread pool's name")
   (f
    :initform 0)
   (f-lock
    :initform (make-lock))
   (f2
    :initform 0)
   (f2-lock
    :initform (make-lock))
   (min-size
    :type integer
    :initarg :min-size
    :initform 1
    :documentation "Minimum number of spawned threads")
   (max-size
    :type (or integer null)
    :initarg :max-size
    :initform 1
    :documentation "Maximum number of spawned threads")
   (keep-alive-time
    :type (integer 1 *)
    :initform 60
    :initarg :keep-alive-time
    :documentation "Additional threads alive time, in seconds")
   (jobs-queue
    :type blocking-queue
    :initform (make-blocking-queue)
    :documentation "Queue of jobs to execute")
   (idle-workers
    :type blocking-stack
    :initform (make-blocking-stack)
    :documentation "Queue of idle workers")
   (workers-count-lock
    :initform (make-lock "workers count")
    :documentation "Lock used in updates of workers-count")
   (workers-count
    :type (integer 0 *)
    :initform 0
    :documentation "Count of all spawned workers")
   (workers-set
    :type blocking-hash-set
    :initform (make-blocking-hash-set)
    :documentation "All spawned workers set")
   (running-p
    :type boolean
    :initform nil
    :documentation "Boolean determines is pool running or should be stopped")))

(defgeneric increase-workers (thread-pool)
  (:documentation "Increase workers count"))

(defgeneric decrease-workers (thread-pool)
  (:documentation "Decrease workers count"))

(defgeneric create-pool-worker (thread-pool)
  (:documentation "Create new pool worker when jobs will execute"))

(defgeneric remove-pool-worker (thread-pool pool-worker)
  (:documentation "Stop and remove pool worker from pool"))

(defgeneric start-pool (thread-pool)
  (:documentation "Start pool, means spawn all necessary workers determined by min-size"))

(defgeneric stop-pool (thread-pool)
  (:documentation "Stop pool and all spawned workers"))

(defgeneric join-pool (thread-pool)
  (:documentation "Join to pool's workers and await it's termination"))

(defgeneric execute (thread-pool &rest functions)
  (:documentation "Execute function in any idle worker"))

(defgeneric pool-worker-finished (thread-pool pool-worker result)
  (:documentation "Callback on finish worker call"))

(defgeneric pool-worker-terminated (thread-pool pool-worker)
  (:documentation "Callback on terminating worker thread"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sleep-down ((pool-worker pool-worker))
  (with-slots (lock condition)
      pool-worker
    (with-lock-held (lock)
      (condition-wait condition lock))))

(defmethod wake-up ((pool-worker pool-worker) function)
  (with-slots (execute-function condition)
      pool-worker
    (setf execute-function function)
    (condition-notify condition)))

(defmethod stop ((pool-worker pool-worker))
  (with-slots (lock condition running-p)
      pool-worker
    (with-lock-held (lock)
      (setf running-p nil))
    (condition-notify condition)))

(defmethod join-worker-thread ((pool-worker pool-worker))
  (with-slots (thread)
      pool-worker
    (join-thread thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spawn-pool-worker (thread-pool)
  (let ((pool-worker (make-instance 'pool-worker)))
    (with-slots (execute-function thread lock last-used-time running-p)
	pool-worker
      (with-slots (name (thread-pool-running-p running-p) f f-lock f2 f2-lock)
	  thread-pool
	(setf thread
	      (new-thread (format nil "~a pool worker" name)
		(iter
		  (while (or (and running-p thread-pool-running-p) execute-function))
		  (when (not execute-function)
		    (sleep-down pool-worker))
		  (let ((result (if execute-function
				    (handler-case
					(funcall execute-function)
				      (error (condition) condition))
				    'no-execution)))
		    (with-lock-held (f2-lock)
		      (incf f2))
		    (when execute-function
		      (with-lock-held (f-lock)
			(incf f)))
		    (setf execute-function nil)
		    (setf last-used-time (get-universal-time))
		    (pool-worker-finished thread-pool pool-worker result)))
		(pool-worker-terminated thread-pool pool-worker)))))
    pool-worker))

(defmethod initialize-instance :after ((thread-pool thread-pool) &key)
  (with-slots (min-size max-size)
      thread-pool
    (when (and max-size (< max-size min-size))
      (setf max-size min-size))))

(defun make-fixed-thread-pool (name &optional &key (size 1))
  (make-instance 'thread-pool :name name :min-size size))

(defun make-cached-thread-pool (name &optional &key (size 1) (max-size 2) (keep-alive-time 60))
  (make-instance 'thread-pool :name name :min-size size :max-size max-size :keep-alive-time keep-alive-time))

(defmethod increase-workers ((thread-pool thread-pool))
  (with-slots (workers-count-lock workers-count)
      thread-pool
    (with-lock-held (workers-count-lock)
      (incf workers-count))))

(defmethod decrease-workers ((thread-pool thread-pool))
  (with-slots (workers-count-lock workers-count)
      thread-pool
    (with-lock-held (workers-count-lock)
      (decf workers-count))))

(defmethod create-pool-worker ((thread-pool thread-pool))
  (with-slots (idle-workers workers-set)
      thread-pool
    (increase-workers thread-pool)
    (handler-case
	(let ((pool-worker (spawn-pool-worker thread-pool)))
	  (push-object idle-workers pool-worker)
 	  (add-object workers-set pool-worker)
	  pool-worker)
      (error () (decrease-workers thread-pool)))))

(defmethod remove-pool-worker ((thread-pool thread-pool) (pool-worker pool-worker))
  (with-slots (idle-workers workers-set)
      thread-pool
    (stop pool-worker)
    (remove-object idle-workers pool-worker)
    (remove-object workers-set pool-worker)
    (decrease-workers thread-pool)))

(defmethod start-pool ((thread-pool thread-pool))
  (with-slots (running-p min-size)
      thread-pool
    (unless running-p
      (setf running-p t)
      (iter (for i from 1 to min-size)
	    (collect (create-pool-worker thread-pool))))))

(defmethod stop-pool ((thread-pool thread-pool))
  (with-slots (running-p workers-set)
      thread-pool
    (setf running-p nil)
    (iter (for pool-worker in (keys workers-set))
	  (stop pool-worker))))

(defmethod join-pool ((thread-pool thread-pool))
  (with-slots (workers-set f f2)
      thread-pool
    (iter (while (> (size workers-set) 0))
	  (iter (for pool-worker in (keys workers-set))
		(join-worker-thread pool-worker)))
    (format t "F: ~a~%" f)
    (format t "F2: ~a~%" f2)
    ))

(defun execute-single (thread-pool function)
  (with-slots (running-p jobs-queue idle-workers)
      thread-pool
    (when running-p
      (let ((pool-worker (pop-object idle-workers)))
	(if pool-worker
	    (wake-up pool-worker function)
	    (enqueue jobs-queue function))
	pool-worker))))

(defmethod execute ((thread-pool thread-pool) &rest functions)
  (with-slots (running-p jobs-queue idle-workers workers-count-lock workers-count max-size)
      thread-pool
    (iter (for function in functions)
	  #+nil(when (and running-p
		     (or (not max-size)
			 (< (with-lock-held (workers-count-lock) workers-count) max-size))
		     (empty-p idle-workers))
	    (create-pool-worker thread-pool))
	  (accumulate (execute-single thread-pool function) by #'and initial-value t))))

(defmethod pool-worker-finished ((thread-pool thread-pool) (pool-worker pool-worker) result)
  (with-slots (min-size keep-alive-time jobs-queue running-p
			idle-workers workers-count)
      thread-pool
    (let ((function (dequeue jobs-queue)))
      (if function
	  (wake-up pool-worker function)
	  (push-object idle-workers pool-worker)))
    #+nil(when (and running-p (> workers-count min-size) (not (empty-p idle-workers)))
      (let ((worker-candidates (filter idle-workers
				       #'(lambda (pool-worker)
					   (and (> workers-count min-size)
						(> (get-universal-time)
						   (+ (slot-value pool-worker 'last-used-time)
						      keep-alive-time)))))))
	(iter (for pool-worker-candidate in worker-candidates)
	      (while (> workers-count min-size))
	      (stop pool-worker-candidate))))))

(defmethod pool-worker-terminated ((thread-pool thread-pool) (pool-worker pool-worker))
  (with-slots (workers-set)
      thread-pool
    (remove-pool-worker thread-pool pool-worker)))
