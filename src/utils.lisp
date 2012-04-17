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
   (lock
    :initform (make-lock "blocking-queue-lock"))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-blocking-queue ()
  (make-instance 'blocking-queue))

(defmethod enqueue (object (blocking-queue blocking-queue))
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

(defmethod empty-queue-p ((blocking-queue blocking-queue))
  (null (car (slot-value blocking-queue 'queue))))

;;;;;;; Blocking hashset

(defclass blocking-hash-set ()
  ((hash-table
    :initform (make-hash-table))
   (lock
    :initform (make-lock "has-set-lock"))))

(defgeneric make-blocking-hash-set ()
  (:documentation ""))

(defgeneric add-object (object blocking-hash-set)
  (:documentation ""))

(defgeneric remove-object (object blocking-hash-set)
  (:documentation ""))

(defgeneric size (blocking-hash-set)
  (:documentation ""))

(defgeneric each (blocking-hash-set function)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-blocking-hash-set ()
  (make-instance 'blocking-hash-set))

(defmethod add-object (object (blocking-hash-set blocking-hash-set))
  (with-slots (hash-table lock)
      blocking-hash-set
    (setf (gethash object hash-table) t)))

(defmethod remove-object (object (blocking-hash-set blocking-hash-set))
  (with-slots (hash-table lock)
      blocking-hash-set
    (remhash object hash-table)))

(defmethod size ((blocking-hash-set blocking-hash-set))
  (with-slots (hash-table)
      blocking-hash-set
    (hash-table-count hash-table)))

(defmethod each ((blocking-hash-set blocking-hash-set) function)
  (with-slots (hash-table lock)
      blocking-hash-set
    (iter (for (key _) in-hashtable hash-table)
          (funcall function key))))