;;
;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.binary-data
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components
  ((:file "packages")
   (:file "binary-data" :depends-on ("packages"))
   (:file "common-datatypes" :depends-on ("packages" "binary-data")))
  :depends-on (alexandria))

