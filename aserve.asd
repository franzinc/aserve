;;; ASD file contributed by james anderson <james.anderson@setf.de>
(in-package :cl-user)

(defvar *loadswitch* :compile-if-needed)
(defparameter *aserve-root* (directory-namestring *load-pathname*))

(unless (find-class 'asdf::cl-file nil)
  (defclass asdf::cl-file (asdf:cl-source-file) ())
  (defmethod asdf:source-file-type ((c asdf::cl-file) (s asdf:module)) "cl"))

(asdf:defsystem :aserve
  :components
  ;; this list is in cl/src/sys/make.cl as well... keep in sync
  ((:module "htmlgen" :components ((:cl-file "htmlgen")
                                   (:static-file "ChangeLog")))
   (:cl-file "packages")
   (:cl-file "macs")
   (:cl-file "queue")
   (:cl-file "main")
   (:cl-file "headers")
   (:cl-file "parse")
   (:cl-file "decode")
   (:cl-file "publish")
   (:cl-file "authorize")
   (:cl-file "log" )
   (:cl-file "client")
   (:cl-file "proxy")
   (:cl-file "cgi")
   (:cl-file "chunker")
   (:cl-file "playback")

   (:static-file "README.md")
   (:static-file "ChangeLog")
   (:static-file "license-lgpl.txt")
   (:static-file "LICENSE")
   (:static-file "load"))
  :serial t)

 
