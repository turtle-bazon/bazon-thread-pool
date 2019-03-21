;;; -*- lisp -*-

(in-package :ru.bazon.thread-pool)

(deftype pool-worker-state () '(member :awaiting-job :executing-job))

(defclass pool-worker ()
  ((name
    :initarg :name
    :documentation "Name of pool worker")
   (thread-pool
    :initarg :thread-pool
    :documentation "Thread pool owning that worker")
   (thread
    :documentation "Thread, that holds parallel process to execute code")
   (state
    :type pool-worker-state
    :initform :awaiting-job
    :documentation "Workers state, :awaiting-job or :executing-job")
   (last-used-time
    :type integer
    :initform (get-universal-time)
    :documentation "Worker's last used time (to determine inactive workers)")
   (running-p
    :type boolean
    :initform t
    :documentation "Boolean determines is worker running or should be stopped")))

(defgeneric stop-worker (pool-worker)
  (:documentation "Stop execution of worker"))

(defgeneric terminate-worker (pool-worker)
  (:documentation "Terminate execution of worker (force thread destroy)"))

(defgeneric join-worker (pool-worker)
  (:documentation "Join to worker thread and await for it's termination"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pool-worker-thread (pool-worker)
  (lambda ()
    (with-slots ((worker-name name) thread-pool state last-used-time)
        pool-worker
      (with-slots (jobs-queue workers-set workers-lock running-p)
          thread-pool
        (iter (for next-job = (dequeue-object jobs-queue))
              (while (or running-p next-job))
              (when next-job
                (setf state :executing-job)
                (setf last-used-time (get-universal-time))
                (handler-case
                    (funcall next-job)
                  (error (condition) condition))
                (setf last-used-time (get-universal-time))
                (setf state :awaiting-job)))
        (with-lock-held (workers-lock)
          (remove-object workers-set pool-worker))))))

(defmethod initialize-instance :after ((pool-worker pool-worker) &key)
  (with-slots (name thread)
      pool-worker
    (setf thread (make-thread (pool-worker-thread pool-worker) :name name))))

(defmethod stop-worker ((pool-worker pool-worker))
  (with-slots (running-p)
      pool-worker
    (setf running-p nil)))

(defmethod terminate-worker ((pool-worker pool-worker))
  (with-slots (thread running-p)
      pool-worker
    (setf running-p nil)
    (destroy-thread thread)
    (iter (while (thread-alive-p thread)))))

(defmethod join-worker ((pool-worker pool-worker))
  (with-slots (thread)
      pool-worker
    (join-thread thread)))

