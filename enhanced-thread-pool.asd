;;; -*- lisp -*-

(defsystem :enhanced-thread-pool
  :name "enhanced-thread-pool"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description ""
  :depends-on (:bordeaux-threads :iterate)
  :components ((:module src
                        :components
			((:file "package")
			 (:file "utils" :depends-on ("package"))
			 (:file "enhanced-thread-pool" :depends-on ("package"
								    "utils"))))))
