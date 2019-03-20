;;; -*- lisp -*-

(in-package :ru.bazon.thread-pool)

(defclass thread-pool ()
  ((name
    :initarg :name
    :initform (error "Name must be provided")
    :documentation "Thread pool's name")
   (size
    :type integer
    :initarg :size
    :initform 1
    :documentation "Minimum number of spawned threads")
   (max-size
    :type (or integer null)
    :initarg :max-size
    :initform 0
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
    :initform (make-instance 'blocking-queue :back-queue (make-instance 'simple-queue))
    :documentation "Queue of jobs to execute")
   (workers-set
    :type hash-set
    :initform (make-instance 'hash-set)
    :documentation "All spawned workers set")
   (workers-lock
    :initform (make-lock "workers")
    :documentation "Lock used to update workers-count and workers-set")
   (running-p
    :type boolean
    :initform nil
    :documentation "Boolean determines is pool running or should be stopped")))

(defgeneric start-pool (thread-pool)
  (:documentation "Start pool, means spawn all necessary workers determined by min-size"))

(defgeneric stop-pool (thread-pool)
  (:documentation "Stop pool and all spawned workers"))

(defgeneric join-pool (thread-pool)
  (:documentation "Join to pool's workers and await it's termination"))

(defgeneric execute (thread-pool &rest functions)
  (:documentation "Execute function in any idle worker"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil(defun spawn-pool-worker (thread-pool suffix)
  (with-slots (name
	       workers-set idle-workers workers-lock idle-workers-available workers-empty
	       min-size min-used-time keep-alive-time running-p)
      thread-pool
    (let ((pool-worker (make-pool-worker)))
      (with-slots (thread job-function lock worker-idle job-available
			  last-used-time (worker-running-p running-p))
	  pool-worker
	(setf thread
              (make-thread
               (lambda ()
                 (iter (while (and running-p worker-running-p))
                       (with-lock-held (lock)
                         (iter (while (not job-function))
                               (condition-wait job-available lock))
                         (unless (eq job-function 'skip)
                           (handler-case
                               (funcall job-function)
                             (error (condition) condition))
                           (let ((now (get-universal-time)))
                             (setf last-used-time now)
                             (setf job-function nil)
                             (with-lock-held (workers-lock)
                               (when (> (- now min-used-time) keep-alive-time)
                                 (iter (for idle-worker in idle-workers)
                                       (with-slots ((iw-last-used-time last-used-time)
                                                    (iw-running-p running-p)
                                                    (iw-job-function job-function)
                                                    (iw-job-available job-available))
                                           idle-worker
                                         (when (and (> (- now iw-last-used-time) keep-alive-time)
                                                    (> (size workers-set) min-size))
                                           (setf iw-running-p nil)
                                           (setf iw-job-function 'skip)
                                           (condition-notify iw-job-available)))))
                               (push pool-worker idle-workers)
                               (condition-notify idle-workers-available)
                               (condition-notify worker-idle))))))
                 (with-lock-held (workers-lock)
                   (remove-object workers-set pool-worker)
                   (setf idle-workers (remove pool-worker idle-workers))
                   (when (= (size workers-set) 0)
                     (condition-notify workers-empty))))
               :name (format nil "~a~a pool worker" name suffix))))
      pool-worker)))

(defmethod initialize-instance :after ((thread-pool thread-pool) &key)
  (with-slots (size max-size)
      thread-pool
    (when (and size (< max-size size))
      (setf max-size size))))

(defun make-fixed-thread-pool (name &optional &key (size 1))
  (make-instance 'thread-pool :name name :size size))

(defun make-cached-thread-pool (name &optional &key (size 1) (max-size 2)
                                                 (keep-alive-time 60))
  (make-instance 'thread-pool :name name :size size :max-size max-size
		 :keep-alive-time keep-alive-time))

(defmethod start-pool ((thread-pool thread-pool))
  (with-slots (name size max-size
               workers-set workers-lock running-p)
      thread-pool
    (unless running-p
      (setf running-p t)
      (with-lock-held (workers-lock)
        (clear workers-set)
	(iter (for i from 1 to size)
              (for pw = (make-instance
                         'pool-worker
                         :thread-pool thread-pool
                         :name (format nil "thread-pool ~a worker ~a" name i)))
	      (add-object workers-set pw))))))

(defmethod stop-pool ((thread-pool thread-pool))
  (with-slots (jobs-queue workers-set workers-lock running-p)
      thread-pool
    (setf running-p nil)
    (with-lock-held (workers-lock)
      (iter (with pw-it = (iterator workers-set))
            (while (it-next pw-it))
            (for pool-worker = (it-current pw-it))
            (stop pool-worker)
            (enqueue-object jobs-queue nil)))))

(defmethod join-pool ((thread-pool thread-pool))
  (with-slots (manager-thread workers-set)
      thread-pool
    #+nil(join-thread manager-thread)
    (iter (with ws-it = (iterator workers-set))
          (while (it-next ws-it))
          (for pool-worker = (it-current ws-it))
          (join-worker-thread pool-worker))))

(defmethod execute ((thread-pool thread-pool) &rest functions)
  (with-slots (jobs-queue running-p)
      thread-pool
    (when running-p
      (iter (for function in functions)
            (enqueue-object jobs-queue function)))))
