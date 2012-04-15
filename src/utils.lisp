;;;; -*- mode: lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;; Simplify thread creation

(defmacro new-thread (name &body body)
  `(make-thread
    #'(lambda ()
	,@body)
    :name ,name))

;;;;;;; A simple, but efficient, queue implementation, by Paul Graham, ANSI Common Lisp
;;;;;;; Modified to be blockable at simultaneous access

(defclass blocking-queue ()
  ((queue
    :initform (cons nil nil))
   (size
    :type integer
    :initform 0)
   (lock
    :initform (make-lock "blocking-queue"))))

(defgeneric make-blocking-queue ()
  (:documentation ""))

(defgeneric enqueue (object blocking-queue)
  (:documentation ""))

(defgeneric dequeue (blocking-queue)
  (:documentation ""))

(defgeneric peek-queue (blocking-queue)
  (:documentation ""))

(defgeneric empty-queue-p (blocking-queue)
  (:documentation ""))

(defgeneric size (blocking-queue)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-blocking-queue ()
  (make-instance 'blocking-queue))

(defmethod enqueue (object (blocking-queue blocking-queue))
  (with-slots (queue size lock)
      blocking-queue
    (with-lock-held (lock)
      (if (null (car queue))
	  (setf (cdr queue) (setf (car queue) (cons object nil)))
	  (setf (cdr (cdr queue)) (cons object nil)
		(cdr queue) (cdr (cdr queue))))
      (incf size)
      (car queue))))

(defmethod dequeue ((blocking-queue blocking-queue))
  (with-slots (queue size lock)
      blocking-queue
    (with-lock-held (lock)
      (when (not (= size 0))
	(decf size))
      (pop (car queue)))))

(defmethod peek-queue ((blocking-queue blocking-queue))
  (car (car (slot-value blocking-queue 'queue))))

(defmethod empty-queue-p ((blocking-queue blocking-queue))
  (null (car (slot-value blocking-queue 'queue))))

(defmethod size ((blocking-queue blocking-queue))
  (slot-value blocking-queue 'size))
