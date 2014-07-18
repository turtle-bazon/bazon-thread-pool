;;; -*- lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pool-worker ()
  ((job-function
    :initform nil
    :documentation "")
   (lock
    :initform (make-lock "pool-worker-lock")
    :documentation "Worker lock (used in condition-wait)")
   (worker-idle
    :initform (make-condition-variable :name "worker-idle")
    :documentation "")
   (job-available
    :initform (make-condition-variable :name "job-available")
    :documentation "")
   (thread
    :documentation "Thread, that holds parallel process to execute code")
   (last-used-time
    :type integer
    :initform (get-universal-time)
    :documentation "Worker's last used time (to determine inactive workers)")
   (running-p
    :type boolean
    :initform t
    :documentation "Boolean determines is worker running or should be stopped")))

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
   (manager-thread
    :documentation "")
   (jobs-queue
    :type blocking-queue
    :initform (make-queue)
    :documentation "Queue of jobs to execute")
   (jobs-queue-lock
    :initform (make-lock "jobs-queue-lock")
    :documentation "Lock for jobs queue")
   (jobs-queue-empty
    :initform (make-condition-variable :name "jobs-queue-empty")
    :documentation "")
   (jobs-queue-not-empty
    :initform (make-condition-variable :name "jobs-queue-not-empty")
    :documentation "")
   (workers-set
    :type hash-set
    :initform (make-hash-set)
    :documentation "All spawned workers set")
   (idle-workers
    :initform nil
    :documentation "")
   (workers-lock
    :initform (make-lock "workers")
    :documentation "Lock used to update workers-count and workers-set")
   (idle-workers-available
    :initform (make-condition-variable :name "idle-workers-available")
    :documentation "")
   (workers-empty
    :initform (make-condition-variable :name "workers-empty")
    :documentation "")
   (shutdown-initiated-p
    :type boolean
    :initform nil
    :documentation "")
   (running-p
    :type boolean
    :initform nil
    :documentation "Boolean determines is pool running or should be stopped")))

(defgeneric create-pool-worker (thread-pool suffix)
  (:documentation "Create new pool worker when jobs will execute"))

(defgeneric start-pool (thread-pool)
  (:documentation "Start pool, means spawn all necessary workers determined by min-size"))

(defgeneric stop-pool (thread-pool)
  (:documentation "Stop pool and all spawned workers"))

(defgeneric join-pool (thread-pool)
  (:documentation "Join to pool's workers and await it's termination"))

(defgeneric execute (thread-pool &rest functions)
  (:documentation "Execute function in any idle worker"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pool-worker ()
  (make-instance 'pool-worker))

(defmethod stop ((pool-worker pool-worker))
  (with-slots (lock worker-idle thread job-function job-available running-p)
      pool-worker
    (with-lock-held (lock)
      (setf running-p nil)
      (iter (while job-function)
	    (condition-wait worker-idle lock))
      (setf job-function 'skip)
      (condition-notify job-available))))

(defmethod join-worker-thread ((pool-worker pool-worker))
  (with-slots (thread)
      pool-worker
    (join-thread thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spawn-pool-worker (thread-pool suffix)
  (with-slots (name
	       workers-set idle-workers workers-lock idle-workers-available workers-empty
	       running-p)
      thread-pool
    (let ((pool-worker (make-pool-worker)))
      (with-slots (thread job-function lock worker-idle job-available
			  last-used-time (worker-running-p running-p))
	  pool-worker
	(setf thread
	      (new-thread (format nil "~a~a pool worker" name suffix)
		(iter (while (and running-p worker-running-p))
		      (with-lock-held (lock)
			(iter (while (not job-function))
			      (condition-wait job-available lock))
			(unless (eq job-function 'skip)
			  (handler-case
			      (funcall job-function)
			    (error (condition) condition))
			  (setf last-used-time (get-universal-time))
			  (setf job-function nil)
			  (with-lock-held (workers-lock)
                            (push pool-worker idle-workers)
                            (condition-notify idle-workers-available))
			  (condition-notify worker-idle))))
		(with-lock-held (workers-lock)
                  (remove-object workers-set pool-worker)
                  (setf idle-workers (remove pool-worker idle-workers))
		  (when (= (size workers-set) 0)
                    (condition-notify workers-empty))))))
      pool-worker)))

(defmethod initialize-instance :after ((thread-pool thread-pool) &key)
  (with-slots (min-size max-size)
      thread-pool
    (when (and max-size (< max-size min-size))
      (setf max-size min-size))))

(defun make-fixed-thread-pool (name &optional &key (size 1))
  (make-instance 'thread-pool :name name :min-size size))

(defun make-cached-thread-pool (name &optional &key (size 1) (max-size 2) (keep-alive-time 60))
  (make-instance 'thread-pool :name name :min-size size :max-size max-size
		 :keep-alive-time keep-alive-time))

(defmethod create-pool-worker ((thread-pool thread-pool) suffix)
  (with-slots (workers-set idle-workers idle-workers-available)
      thread-pool
    (let ((pool-worker (spawn-pool-worker thread-pool suffix)))
      (handler-case
	  (progn
	    (add-object workers-set pool-worker)
	    (push pool-worker idle-workers)
	    (condition-notify idle-workers-available)
	    pool-worker)
	(error () (stop pool-worker))))))

(defmethod start-pool ((thread-pool thread-pool))
  (with-slots (min-size max-size manager-thread
			jobs-queue jobs-queue-lock jobs-queue-empty jobs-queue-not-empty
			workers-set idle-workers workers-lock idle-workers-available
			shutdown-initiated-p running-p)
      thread-pool
    (unless running-p
      (setf shutdown-initiated-p nil)
      (setf running-p t)
      (with-lock-held (workers-lock)
	(iter (for i from 1 to min-size)
	      (create-pool-worker thread-pool "")))
      (setf manager-thread
	    (new-thread "Thread pool manager"
	      (iter (while running-p)
		    (let ((free-worker (with-lock-held (workers-lock)
                                         (iter (while (and running-p (eq idle-workers nil)))
					       (if (< (size workers-set) max-size)
						   (create-pool-worker thread-pool "-add")
						   (condition-wait idle-workers-available workers-lock)))
                                         (pop idle-workers)))
			  (next-job (with-lock-held (jobs-queue-lock)
				      (iter (while (and running-p (= (size jobs-queue) 0)))
					    (condition-wait jobs-queue-not-empty jobs-queue-lock))
				      (dequeue jobs-queue))))
		      (when (and free-worker next-job)
			(with-slots (job-function lock job-available)
			    free-worker
			  (with-lock-held (lock)
			    (setf job-function next-job)
			    (condition-notify job-available))))
		      (with-lock-held (jobs-queue-lock)
			(when (= (size jobs-queue) 0)
                          (condition-notify jobs-queue-empty))))))))))

(defmethod stop-pool ((thread-pool thread-pool))
  (with-slots (jobs-queue jobs-queue-lock jobs-queue-empty jobs-queue-not-empty
			  workers-set idle-workers-available workers-lock workers-empty
			  shutdown-initiated-p running-p)
      thread-pool
    (setf shutdown-initiated-p t)
    (with-lock-held (jobs-queue-lock)
      (iter (while (> (size jobs-queue) 0))
	    (condition-wait jobs-queue-empty jobs-queue-lock)))
    (setf running-p nil)
    (iter (for pool-worker in (with-lock-held (workers-lock)
				(keys workers-set)))
	  (stop pool-worker))
    (with-lock-held (workers-lock)
      (iter (while (> (size workers-set) 0))
	    (condition-wait workers-empty workers-lock)))
    (condition-notify idle-workers-available)
    (condition-notify jobs-queue-not-empty)))

(defmethod join-pool ((thread-pool thread-pool))
  (with-slots (manager-thread)
      thread-pool
    (join-thread manager-thread)))

(defmethod execute ((thread-pool thread-pool) &rest functions)
  (with-slots (max-size
	       jobs-queue jobs-queue-lock jobs-queue-not-empty
	       shutdown-initiated-p running-p)
      thread-pool
    (unless shutdown-initiated-p
      (with-lock-held (jobs-queue-lock)
        (iter (for function in functions)
	      (enqueue jobs-queue function)
	      (condition-notify jobs-queue-not-empty))))))
