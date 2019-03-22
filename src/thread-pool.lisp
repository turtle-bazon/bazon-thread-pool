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
   (jobs-queue
    :type blocking-queue
    :initform (make-instance 'blocking-queue :back-queue (make-instance 'simple-queue))
    :documentation "Queue of jobs to execute")
   (worker-next-id
    :type integer
    :documentation "Next id for pool-worker")
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

(defun create-worker (thread-pool index)
  (with-slots (name)
      thread-pool
    (make-instance
     'pool-worker
     :thread-pool thread-pool
     :name (format nil "thread-pool ~a worker ~a" name index))))

(defmethod start-pool ((thread-pool thread-pool))
  (with-slots (name size max-size worker-next-id
               workers-set workers-lock running-p)
      thread-pool
    (unless running-p
      (setf running-p t)
      (with-lock-held (workers-lock)
        (clear workers-set)
	(iter (for i from 1 to size)
              (for pw = (create-worker thread-pool i))
	      (add-object workers-set pw))
        (setf worker-next-id (+ size 1)))
      t)))

(defmethod stop-pool ((thread-pool thread-pool))
  (with-slots (jobs-queue workers-set workers-lock running-p)
      thread-pool
    (setf running-p nil)
    (with-lock-held (workers-lock)
      (iter (with pw-it = (iterator workers-set))
            (while (it-next pw-it))
            (for pool-worker = (it-current pw-it))
            (stop-worker pool-worker))
      (iter (repeat (size workers-set))
            (enqueue-object jobs-queue nil)))))

(defmethod join-pool ((thread-pool thread-pool))
  (with-slots (workers-set)
      thread-pool
    (iter (with ws-it = (iterator workers-set))
          (while (it-next ws-it))
          (for pool-worker = (it-current ws-it))
          (join-worker pool-worker))))

(defun increase-workers (thread-pool deficit-count)
  (with-slots (name max-size worker-next-id workers-set)
      thread-pool
    (iter (repeat deficit-count)
          (for i from worker-next-id)
          (while (<= i max-size))
          (for pw = (create-worker thread-pool i))
          (incf worker-next-id)
          (add-object workers-set pw))))

(defun decrease-workers (thread-pool proficit-count)
  (with-slots (workers-set keep-alive-time)
      thread-pool
    (iter (with removed-count = 0)
          (with pw-it = (iterator workers-set))
          (while (< removed-count proficit-count))
          (while (it-next pw-it))
          (for pool-worker = (it-current pw-it))
          (with-slots (name state last-used-time)
              pool-worker
            (unless (eq :executing-job state)
              (let ((now (get-universal-time)))
                (when (> (- now last-used-time) keep-alive-time)
                  (terminate-worker pool-worker)
                  (remove-object workers-set pw-it)
                  (incf removed-count))))))))

(defmethod execute ((thread-pool thread-pool) &rest functions)
  (with-slots (size max-size jobs-queue workers-set workers-lock running-p)
      thread-pool
    (when running-p
      (with-lock-held (workers-lock)
        (when (and (> max-size size)
                   (> (size jobs-queue) 0)
                   (< (size workers-set) max-size))
          (increase-workers thread-pool (size jobs-queue)))
        (when (and (> max-size size)
                   (= (size jobs-queue) 0)
                   (> (size workers-set) size))
          (decrease-workers thread-pool (- (size workers-set) size))))
      (iter (for function in functions)
            (enqueue-object jobs-queue function)))))
