;;;; -*- mode: lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;; Simplify thread creation

(defmacro new-thread (name &body body)
  `(make-thread
    #'(lambda ()
	,@body)
    :name ,name))

;;;;;;; Synchronized collection

(defclass collection ()
  ())

(defgeneric clear (collection)
  (:documentation ""))

(defgeneric add-object (collection object)
  (:documentation ""))

(defgeneric push-object (collection object)
  (:documentation ""))

(defgeneric pop-object (collection)
  (:documentation ""))

(defgeneric peek-object (collection)
  (:documentation ""))

(defgeneric remove-object (collection object)
  (:documentation ""))

(defgeneric filter (collection condition)
  (:documentation ""))

(defgeneric size (collection)
  (:documentation ""))

(defgeneric empty-p (collection)
  (:documentation ""))

(defgeneric keys (collection)
  (:documentation ""))

;;;;;;; A simple, but efficient, queue implementation, by Paul Graham, ANSI Common Lisp

(defclass queue ()
  ((_queue
    :initform (cons nil nil))
   (count
    :initform 0)))

(defgeneric enqueue (queue object)
  (:documentation ""))

(defgeneric dequeue (queue)
  (:documentation ""))

(defgeneric size (queue)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-queue ()
  (make-instance 'queue))

(defmethod enqueue ((queue queue) object)
  (with-slots (_queue count)
      queue
    (if (null (car _queue))
	(setf (cdr _queue) (setf (car _queue) (cons object nil)))
	(setf (cdr (cdr _queue)) (cons object nil)
	      (cdr _queue) (cdr (cdr _queue))))
    (incf count)
    (car _queue)))

(defmethod dequeue ((queue queue))
  (with-slots (_queue count)
      queue
    (let ((queue-head (pop (car _queue))))
      (when (> count 0)
	(decf count))
      queue-head)))

(defmethod size ((queue queue))
  (with-slots (count)
      queue
    count))

;;;;;;; Blocking queue that blocks current thread on enqueue and dequeue (see docs)

(defclass blocking-queue (queue)
  ((max-size
    :initform nil
    :initarg :max-size)
   (internal-queue
    :initform (make-queue))
   (lock
    :initform (make-lock))
   (not-empty-condition
    :initform (make-condition-variable :name "not-empty-condition"))
   (not-full-condition
    :initform (make-condition-variable :name "not-full-condition"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-blocking-queue (&optional queue-max-size)
  (make-instance 'blocking-queue :max-size queue-max-size))

(defmethod enqueue ((blocking-queue blocking-queue) object)
  (with-slots (max-size internal-queue lock not-empty-condition not-full-condition)
      blocking-queue
    (with-lock-held (lock)
      (when max-size
	(iter (while (= (size internal-queue) max-size))
	      (condition-wait not-full-condition lock)))
      (let ((enqueue-result (enqueue internal-queue object)))
	(condition-notify not-empty-condition)
	enqueue-result))))

(defmethod dequeue ((blocking-queue blocking-queue))
  (with-slots (internal-queue lock not-empty-condition not-full-condition)
      blocking-queue
    (with-lock-held (lock)
      (iter (while (= (size internal-queue) 0))
	    (condition-wait not-empty-condition lock))
      (let ((dequeue-result (dequeue internal-queue)))
	(condition-notify not-full-condition)
	dequeue-result))))

(defmethod size ((blocking-queue blocking-queue))
  (with-slots (internal-queue lock)
      blocking-queue
    (with-lock-held (lock)
      (size internal-queue))))

;;;;;;; Hashset

(defclass hash-set (collection)
  ((hash-table
    :initform (make-hash-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hash-set ()
  (make-instance 'hash-set))

(defmethod clear ((hash-set hash-set))
  (with-slots (hash-table)
      hash-set
    (clrhash hash-table)))

(defmethod add-object ((hash-set hash-set) object)
  (with-slots (hash-table)
      hash-set
    (setf (gethash object hash-table) t)))

(defmethod remove-object ((hash-set hash-set) object)
  (with-slots (hash-table)
      hash-set
    (remhash object hash-table)))

(defmethod size ((hash-set hash-set))
  (with-slots (hash-table)
      hash-set
    (hash-table-count hash-table)))

(defmethod keys ((hash-set hash-set))
  (with-slots (hash-table)
      hash-set
    (iter (for (key _) in-hashtable hash-table)
	  (collect key))))
