;;; -*- lisp -*-

(defsystem :bazon-thread-pool-tests
  :name "bazon-thread-pool-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description ""
  :depends-on (:bazon-thread-pool :lift :bordeaux-threads :iterate)
  :components ((:module tests
                        :components
			((:file "package")
			 (:file "bazon-thread-pool-tests" :depends-on ("package"))))))
