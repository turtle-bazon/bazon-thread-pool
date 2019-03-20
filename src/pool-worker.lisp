;;; -*- lisp -*-

(in-package :ru.bazon.thread-pool)

(defclass pool-worker ()
  ((name
    :initarg :name
    :documentation "Name of pool worker")
   (thread-pool
    :initarg :thread-pool
    :documentation "Thread pool owning that worker")
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

(defun pool-worker-thread (pool-worker)
  (lambda ()
    (with-slots ((worker-name name) thread-pool last-used-time)
        pool-worker
      (with-slots (jobs-queue workers-set workers-lock running-p)
          thread-pool
        (iter (for next-job = (dequeue-object jobs-queue))
              (while (or running-p next-job))
              (when next-job
                (format t "Execute in worker: ~a~&" worker-name)
                (handler-case
                    (funcall next-job)
                  (error (condition) condition))
                (let ((now (get-universal-time)))
                  (setf last-used-time now))))
        (with-lock-held (workers-lock)
          (remove-object workers-set pool-worker)
          (format t "Worker removed from set: ~a~&" pool-worker))))))

(defmethod initialize-instance :after ((pool-worker pool-worker) &key)
  (with-slots (name thread)
      pool-worker
    (setf thread (make-thread (pool-worker-thread pool-worker) :name name))))

(defmethod stop ((pool-worker pool-worker))
  (with-slots (lock thread running-p)
      pool-worker
    (setf running-p nil)))

(defmethod join-worker-thread ((pool-worker pool-worker))
  (with-slots (thread)
      pool-worker
    (join-thread thread)))

