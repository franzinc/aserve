;;; ASD file contributed by james anderson <james.anderson@setf.de>
(in-package :cl-user)

(defvar *loadswitch* :compile-if-needed)
(defparameter *aserve-root* (directory-namestring *load-pathname*))


#+allegro
(asdf:defsystem :zaserve
    :name "AllegroServe (portable)"
    :author "John K. Foderaro"
    :version "1.3.65"
    :licence "LLGPL"
    :default-component-class cl-source-file.cl
    :components ((:cl-file "require-original-aserve")))


#-allegro
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

#-(or zacl allegro)
(defpackage :zacl-reader (:export #:cl-file))

#-allegro
(asdf:defsystem
 :zaserve
 :depends-on (:zacl)
 :defsystem-depends-on (:zacl)
 :version "1.3.65"
 :name "AllegroServe"
 :author "John K. Foderaro"
 :licence "LLGPL"
 :components
 ;; this list is in load.cl as well... keep in sync
 ((:module "htmlgen" :components ((zacl-reader:cl-file "htmlgen")
                                  (:static-file "ChangeLog")))
  (zacl-reader:cl-file "packages")
  (zacl-reader:cl-file "macs")
  (zacl-reader:cl-file "queue")
  (zacl-reader:cl-file "main")
  (zacl-reader:cl-file "headers")
  (zacl-reader:cl-file "parse")
  (zacl-reader:cl-file "decode")
  (zacl-reader:cl-file "publish")
  (zacl-reader:cl-file "authorize")
  (zacl-reader:cl-file "log" )
  (zacl-reader:cl-file "client")
  (zacl-reader:cl-file "proxy")
  (zacl-reader:cl-file "cgi")
  (zacl-reader:cl-file "chunker")
  #+include-playback (zacl-reader:cl-file "playback")

  (:static-file "README.md")
  (:static-file "ChangeLog")
  (:static-file "license-lgpl.txt")
  (:static-file "LICENSE")
  (:static-file "load"))
 :perform (asdf:load-op :before (op zaserve)
			(check-platform-compatibilty))
 :serial t)



