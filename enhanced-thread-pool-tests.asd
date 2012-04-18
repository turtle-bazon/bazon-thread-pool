;;; -*- lisp -*-

(defsystem :enhanced-thread-pool-tests
  :name "enhanced-thread-pool-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description ""
  :depends-on (:enhanced-thread-pool :lift :bordeaux-threads :iterate)
  :components ((:module tests
                        :components
			((:file "package")
			 (:file "enhanced-thread-pool-tests" :depends-on ("package"))))))
