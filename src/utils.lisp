;;;; -*- mode: lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;; Simplify thread creation

(defmacro new-thread (name &body body)
  `(make-thread
    #'(lambda ()
	,@body)
    :name ,name))

;;;;;;; Blocking collection

(defclass blocking-collection ()
  ((lock
    :initform (make-lock))))

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

(defgeneric size (collection)
  (:documentation ""))

(defgeneric empty-p (collection)
  (:documentation ""))

(defgeneric keys (collection)
  (:documentation ""))

;;;;;;; A simple, but efficient, queue implementation, by Paul Graham, ANSI Common Lisp
;;;;;;; Modified to be blockable at simultaneous access

(defclass blocking-queue (blocking-collection)
  ((queue
    :initform (cons nil nil))))

(defgeneric make-blocking-queue ()
  (:documentation ""))

(defgeneric enqueue (object blocking-queue)
  (:documentation ""))

(defgeneric dequeue (blocking-queue)
  (:documentation ""))

(defgeneric peek-queue (blocking-queue)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-blocking-queue ()
  (make-instance 'blocking-queue))

(defmethod enqueue ((blocking-queue blocking-queue) object)
  (with-slots (queue lock)
      blocking-queue
    (with-lock-held (lock)
      (if (null (car queue))
	  (setf (cdr queue) (setf (car queue) (cons object nil)))
	  (setf (cdr (cdr queue)) (cons object nil)
		(cdr queue) (cdr (cdr queue))))
      (car queue))))

(defmethod dequeue ((blocking-queue blocking-queue))
  (with-slots (queue lock)
      blocking-queue
    (with-lock-held (lock)
      (pop (car queue)))))

(defmethod peek-queue ((blocking-queue blocking-queue))
  (car (car (slot-value blocking-queue 'queue))))

(defmethod empty-p ((blocking-queue blocking-queue))
  (null (car (slot-value blocking-queue 'queue))))

;;;;;;; Blocking hashset

(defclass blocking-hash-set (blocking-collection)
  ((hash-table
    :initform (make-hash-table))))

(defgeneric make-blocking-hash-set ()
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-blocking-hash-set ()
  (make-instance 'blocking-hash-set))

(defmethod clear ((blocking-hash-set blocking-hash-set))
  (with-slots (hash-table lock)
      blocking-hash-set
    (with-lock-held (lock)
      (clrhash hash-table))))

(defmethod add-object ((blocking-hash-set blocking-hash-set) object)
  (with-slots (hash-table lock)
      blocking-hash-set
    (with-lock-held (lock)
      (setf (gethash object hash-table) t))))

(defmethod remove-object ((blocking-hash-set blocking-hash-set) object)
  (with-slots (hash-table lock)
      blocking-hash-set
    (with-lock-held (lock)
      (remhash object hash-table))))

(defmethod size ((blocking-hash-set blocking-hash-set))
  (with-slots (hash-table)
      blocking-hash-set
    (hash-table-count hash-table)))

(defmethod keys ((blocking-hash-set blocking-hash-set))
  (with-slots (hash-table)
      blocking-hash-set
    (iter (for (key _) in-hashtable hash-table)
	  (collect key))))

;;;;;;; Blocking stack

(defclass blocking-stack (blocking-collection)
  ((stack
    :initform nil)))

(defgeneric make-blocking-stack ()
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-blocking-stack ()
  (make-instance 'blocking-stack))

(defmethod clear ((blocking-stack blocking-stack))
  (with-slots (stack lock)
      blocking-stack
    (with-lock-held (lock)
      (setf stack nil))))

(defmethod push-object ((blocking-stack blocking-stack) object)
  (with-slots (stack lock)
      blocking-stack
    (with-lock-held (lock)
      (push object stack))))

(defmethod pop-object ((blocking-stack blocking-stack))
  (with-slots (stack lock)
      blocking-stack
    (with-lock-held (lock)
      (pop stack))))

(defmethod peek-object ((blocking-stack blocking-stack))
  (with-slots (stack)
      blocking-stack
    (car stack)))

(defmethod remove-object ((blocking-stack blocking-stack) object)
  (with-slots (stack lock)
      blocking-stack
    (with-lock-held (lock)
      (setf stack (remove object stack)))))

(defmethod empty-p ((blocking-stack blocking-stack))
  (with-slots (stack)
      blocking-stack
    (eq stack nil)))
