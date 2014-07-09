;;;; -*- mode: lisp -*-

(in-package :ru.bazon.enhanced-thread-pool)

;;;;;;; Simplify thread creation

(defmacro new-thread (name &body body)
  `(make-thread
    #'(lambda ()
	,@body)
    :name ,name))

;;;;;;; Synchronized collection

(defclass synchronized-collection ()
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

(defgeneric filter (collection condition)
  (:documentation ""))

(defgeneric size (collection)
  (:documentation ""))

(defgeneric empty-p (collection)
  (:documentation ""))

(defgeneric keys (collection)
  (:documentation ""))

;;;;;;; A simple, but efficient, queue implementation, by Paul Graham, ANSI Common Lisp
;;;;;;; Modified to be synchronized at simultaneous access

(defclass synchronized-queue (synchronized-collection)
  ((queue
    :initform (cons nil nil))))

(defgeneric make-synchronized-queue ()
  (:documentation ""))

(defgeneric enqueue (object synchronized-queue)
  (:documentation ""))

(defgeneric dequeue (synchronized-queue)
  (:documentation ""))

(defgeneric peek-queue (synchronized-queue)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-synchronized-queue ()
  (make-instance 'synchronized-queue))

(defmethod enqueue ((synchronized-queue synchronized-queue) object)
  (with-slots (queue lock)
      synchronized-queue
    (with-lock-held (lock)
      (if (null (car queue))
	  (setf (cdr queue) (setf (car queue) (cons object nil)))
	  (setf (cdr (cdr queue)) (cons object nil)
		(cdr queue) (cdr (cdr queue))))
      (car queue))))

(defmethod dequeue ((synchronized-queue synchronized-queue))
  (with-slots (queue lock)
      synchronized-queue
    (with-lock-held (lock)
      (pop (car queue)))))

(defmethod peek-queue ((synchronized-queue synchronized-queue))
  (car (car (slot-value synchronized-queue 'queue))))

(defmethod empty-p ((synchronized-queue synchronized-queue))
  (null (car (slot-value synchronized-queue 'queue))))

;;;;;;; Synchronized hashset

(defclass synchronized-hash-set (synchronized-collection)
  ((hash-table
    :initform (make-hash-table))))

(defgeneric make-synchronized-hash-set ()
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-synchronized-hash-set ()
  (make-instance 'synchronized-hash-set))

(defmethod clear ((synchronized-hash-set synchronized-hash-set))
  (with-slots (hash-table lock)
      synchronized-hash-set
    (with-lock-held (lock)
      (clrhash hash-table))))

(defmethod add-object ((synchronized-hash-set synchronized-hash-set) object)
  (with-slots (hash-table lock)
      synchronized-hash-set
    (with-lock-held (lock)
      (setf (gethash object hash-table) t))))

(defmethod remove-object ((synchronized-hash-set synchronized-hash-set) object)
  (with-slots (hash-table lock)
      synchronized-hash-set
    (with-lock-held (lock)
      (remhash object hash-table))))

(defmethod size ((synchronized-hash-set synchronized-hash-set))
  (with-slots (hash-table)
      synchronized-hash-set
    (hash-table-count hash-table)))

(defmethod keys ((synchronized-hash-set synchronized-hash-set))
  (with-slots (hash-table)
      synchronized-hash-set
    (iter (for (key _) in-hashtable hash-table)
	  (collect key))))

;;;;;;; Synchronized stack

(defclass synchronized-stack (synchronized-collection)
  ((stack
    :initform nil)))

(defgeneric make-synchronized-stack ()
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-synchronized-stack ()
  (make-instance 'synchronized-stack))

(defmethod clear ((synchronized-stack synchronized-stack))
  (with-slots (stack lock)
      synchronized-stack
    (with-lock-held (lock)
      (setf stack nil))))

(defmethod push-object ((synchronized-stack synchronized-stack) object)
  (with-slots (stack lock)
      synchronized-stack
    (with-lock-held (lock)
      (push object stack))))

(defmethod pop-object ((synchronized-stack synchronized-stack))
  (with-slots (stack lock)
      synchronized-stack
    (with-lock-held (lock)
      (pop stack))))

(defmethod peek-object ((synchronized-stack synchronized-stack))
  (with-slots (stack)
      synchronized-stack
    (car stack)))

(defmethod remove-object ((synchronized-stack synchronized-stack) object)
  (with-slots (stack lock)
      synchronized-stack
    (with-lock-held (lock)
      (setf stack (remove object stack)))))

(defmethod filter ((synchronized-stack synchronized-stack) condition)
  (with-slots (stack lock)
      synchronized-stack
    (remove-if-not condition stack)))

(defmethod empty-p ((synchronized-stack synchronized-stack))
  (with-slots (stack)
      synchronized-stack
    (eq stack nil)))
