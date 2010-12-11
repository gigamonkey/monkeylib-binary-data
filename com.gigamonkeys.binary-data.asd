;;
;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;
;; Hacked on by Cyrus Harmon
;;

(asdf:defsystem com.gigamonkeys.binary-data
  :name "binary-data"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0?"
  :components
  ((:file "packages")
   (:file "binary-data" :depends-on ("packages"))
   (:file "common-datatypes" :depends-on ("packages" "binary-data")))
  :depends-on (:com.gigamonkeys.macro-utilities))

