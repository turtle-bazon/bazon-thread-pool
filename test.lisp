(asdf:load-system :enhanced-thread-pool)
(in-package :enhanced-thread-pool)
(defvar *pool* (make-instance 'thread-pool :name "test" :min-size 2))
(start-pool *pool*)
(execute *pool*
         #'(lambda ()
             (sleep 3)
             (format t "Finished~%")))
(format t "1~%")
(execute *pool*
         #'(lambda ()
             (sleep 3)
             (format t "Finished~%")))
(format t "2~%")
(execute *pool*
         #'(lambda ()
             (sleep 3)
             (format t "Finished~%")))
(format t "3~%")
(execute *pool*
         #'(lambda ()
             (sleep 3)
             (format t "Finished~%")))
(format t "4~%")
(stop-pool *pool*)
(format t "5~%")

