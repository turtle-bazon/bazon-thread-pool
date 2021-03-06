;;; -*- lisp -*-

(defsystem :bazon-thread-pool
  :name "bazon-thread-pool"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description ""
  :depends-on (:bazon-collections
               :bordeaux-threads
               :iterate)
  :components ((:module src
                        :components
			((:file "package")
                         (:file "pool-worker"
                          :depends-on ("package"))
			 (:file "thread-pool"
                          :depends-on ("package"
                                       "pool-worker")))))
  :in-order-to ((test-op (test-op bazon-thread-pool-tests)))
  :perform (test-op :after (op c)
		    (funcall
		     (intern (symbol-name '#:run-all-tests)
			     :bazon-thread-pool-tests))))
