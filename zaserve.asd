;;; ASD file contributed by james anderson <james.anderson@setf.de>
(in-package :cl-user)

(defvar *loadswitch* :compile-if-needed)
(defparameter *aserve-root* (directory-namestring *load-pathname*))

(unless (find-class 'asdf::cl-file nil)
  (defclass asdf::cl-file (asdf:cl-source-file) ())
  (defmethod asdf:source-file-type ((c asdf::cl-file) (s asdf:module)) "cl"))

(defun check-platform-compatibilty ()
  (unless (or (member :ccl *features*)
	      (member :sbcl *features*))
    (error "

SORRY:
=====

This version of AllegroServe, which uses the `zacl' compatibility
layer (instead of this old acl-compat), is not currently supported on
~a. Please consider contributing, or requesting, a port of zacl for ~a.


"
	   (lisp-implementation-type) (lisp-implementation-type))))

#-allegro
(asdf:defsystem :zaserve
  :depends-on (:zacl)
  :version "1.3.65"
  :name "AllegroServe"
  :author "John K. Foderaro"
  :licence "LLGPL"
  :components
  ;; this list is in load.cl as well... keep in sync
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
   #+include-playback (:cl-file "playback")

   (:static-file "README.md")
   (:static-file "ChangeLog")
   (:static-file "license-lgpl.txt")
   (:static-file "LICENSE")
   (:static-file "load"))
  :perform (asdf:load-op :before (op paserve)
			 (check-platform-compatibilty))
  :serial t)

 

