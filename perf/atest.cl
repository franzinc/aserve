
(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (require :shell)
  (use-package :excl.shell)
  (require :tester)
  (require :aserve)
  (use-package :net.aserve)
  (use-package :net.html.generator))

(in-package :cl-user)

(defvar *port-file*
    (or (sys:getenv "ATEST_PORT_FILE")
	(die "ATEST_PORT_FILE is not defined")))
(defvar *pid-file*
    (or (sys:getenv "ATEST_PID_FILE")
	(die "ATEST_PID_FILE is not defined")))

(setf (file-contents *pid-file*)
  (format nil "~d" (excl::getpid)))

(defvar *my-server* (start :port nil))

(defvar *my-port*
    (let ((server-socket (slot-value *my-server* 'net.aserve::socket)))
      (socket:local-port server-socket)))

(setf (file-contents *port-file*)
  (format nil "~d" *my-port*))

(when (not (probe-file "testfile"))
  (shell "dd if=/dev/urandom of=testfile bs=1M count=1"))

(publish-file :path "/testfile"
	      :server *my-server*
	      :file (merge-pathnames "testfile")
	      :content-type "application/octet-stream"
	      :cache-p nil
	      :compress nil)

(loop (sleep 3600))
