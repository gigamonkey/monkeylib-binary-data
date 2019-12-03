;;
;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;

(asdf:defsystem binary-io
  :description "Library for reading and writing binary data."
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :serial t
  :components ((:file "packages")
               (:file "binary-data")
               (:file "common-datatypes")
               #+sbcl (:file "mmap"))
  :depends-on (alexandria ieee-floats #+sbcl sb-posix)
  :in-order-to ((test-op (test-op :binary-io/test))))

(asdf:defsystem binary-io/test
  :description "Test suite for binary-io"
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :depends-on (:1am :binary-io)
  :components ((:file "tests"))
  :perform (test-op (o c)
                    (uiop:symbol-call :1am '#:run)))
