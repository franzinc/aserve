;; wrktest.cl
;;
;;    Usage: Load uncompiled into a developer lisp.
;;
;;           Define a test-set if the default set isn't desired
;;              (see *default-test-set* below)
;;
;;           Call comp-test to run the same test against all the
;;           lisps in one test-set and capture the results in
;;           a separate file for each lisp in the test-set.
;;
;;           Each test includes an execution of wrk to generate
;;           fetch requests against the aserve in that lisp.
;;           The lisps in the test-set are tested sequentially,
;;           not in parallel.
;;
;;
;; A test-set is a list of dotted pairs specifying the tests to run.
;; The car of each element of the list is the pathname of a lisp in which
;; to run aserve. The cdr is the name of the file to receive the test results.
;; wrktest.cl (this file) will be loaded into each of the specified lisps,
;; with command-line arguments to cause a test run, and the output captured in
;; the associated file.

;; *default-test-set* is the test set to be used if the call to comp-test
;; doesn't specify something else. The current value assumes there are lisp
;; executables named mlisp10s and mlisp11s, and captures the test results
;; in local files named 10.out and 11.out, respectively.
;; The test output will include the name of the exeutable, information about
;; the lisp version and where the executable is located.

#|-----------------------------------------------------------------------------

Here's an example of output from (comp-test :duration 3):

executable mlisp10s finished, ret=0
>>> Testing version 10.1 SMP from /net/walter/acl/rfr/10.1/acl.s.64/src/
>>> hiper-socket is  stream-socket
>>> After pre-test resize-areas
>>> Running wrk -c 10 -t 10 -d 3s --latency http://localhost:46305/testfile
>>> After test, before global gc
>>> After test and global gc
>>> Test finished:
Running 3s test @ http://localhost:46305/testfile
  10 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.13ms   10.72ms 214.35ms   98.10%
    Req/Sec   187.04     48.19   434.00     91.73%
  Latency Distribution
     50%    4.97ms
     75%    5.70ms
     90%    6.55ms
     99%   40.63ms
  4991 requests in 3.10s, 4.88GB read
Requests/sec:   1610.71
Transfer/sec:      1.57GB

-----
executable mlisp11s finished, ret=0
>>> Testing version 11.0 SMP from /net/fire/acl/rfr/11.0/acl.s.64/src/
>>> hiper-socket is  stream-socket
>>> After pre-test resize-areas
>>> Running wrk -c 10 -t 10 -d 3s --latency http://localhost:39943/testfile
>>> After test, before global gc
>>> After test and global gc
>>> Test finished:
Running 3s test @ http://localhost:39943/testfile
  10 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.35ms    4.88ms  68.82ms   97.83%
    Req/Sec   168.03     30.37   343.00     95.39%
  Latency Distribution
     50%    5.59ms
     75%    6.34ms
     90%    7.06ms
     99%   32.33ms
  5093 requests in 3.10s, 4.98GB read
Requests/sec:   1642.98
Transfer/sec:      1.61GB

-----

------------------------------------------------------------------------------|#


(defparameter *default-test-set*
    '((mlisp10s . "10.out")
      (mlisp11s . "11.out")))
;;

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

(defvar *my-server*)

(defun start-test-server (&key (logfile "aserve.log"))
  (let ((my-server (start :port nil)))

    (setf (vhost-log-stream (wserver-default-vhost my-server))
      (open logfile :direction :output :if-does-not-exist :create :if-exists :replace))

    
    (when (not (probe-file "testfile"))
      (shell "dd if=/dev/urandom of=testfile bs=1M count=1"))

    (publish-file :path "/testfile"
		  :server my-server
		  :file (merge-pathnames "testfile")
		  :content-type "application/octet-stream"
		  :cache-p nil
		  :compress nil)
    my-server))

(defun run-test (&key (connections 10) (threads 10) (duration 30) (gc-print nil) (gc-stats nil))
  (format t ">>> Testing version ~a~:[~; SMP~] from ~a~%"
	  excl::*pretty-common-lisp-version-number*
	  (featurep :smp)
	  (truename #P"sys:"))
  
  (setq excl::*hiper-socket-is-stream-socket* t)
  (setq *my-server* (start-test-server))
  
  (format t ">>> hiper-socket is ~:[not ~;~] stream-socket~%"
          excl::*hiper-socket-is-stream-socket*)
  
  (dotimes (i 5)
    (gc))
  (gc t)
    (dotimes (i 5)
    (gc))
  (sys:resize-areas :old (ash 1 24)
		    :new (* 3 (ash 1 19))
		    :verbose t)

  (format t ">>> After pre-test resize-areas~%")
  
  (room t)
  (let ((test-command
	 (format nil "wrk -c ~a -t ~a -d ~as --latency http://localhost:~a/testfile"
		 connections threads duration
		 (socket:local-port (slot-value *my-server* 'net.aserve::socket))))
	test-result)

    (format t ">>> Running ~a~%" test-command)

    (let ((old-print (sys:gc-parameter :print))
	  (old-stats (sys:gc-parameter :stats)))
      (setf (sys:gc-parameter :print) gc-print
	    (sys:gc-parameter :stats) gc-stats)
      (time (setq test-result (command-output test-command :whole t)))
      
      (format t ">>> After test, before global gc~%")

      (room t)
      (gc t)

      (format t ">>> After test and global gc~%")

      (room t)
      (setf (sys:gc-parameter :print) old-print
	    (sys:gc-parameter :stats) old-stats))

    (format t ">>> Test finished:~%~a~%>>>---~%" test-result)))

;; The next form checks the command-line args to see if this is the child lisp
;; that's supposed to run the test, and if so, what the test parameters should be.
;;
;; The recognized arguments are
;;    run-test           if present, this is the child and should run the test
;;    no-exit            if present, the child process doesn't exit after the test
;;    connections xxx    pass xxx to wrk as connections argument
;;    threads     xxx    pass xxx to wrk as threads argument
;;    duration    xxx    pass xxx to wrk as duration argument
;;    gc-print    xxx    set (gc-parameter :print) to xxx during test
;;    gc-stats    xxx    set (gc-parameter :stats) to xxx during test

(do ((arglist (cdr (sys:command-line-arguments :application t)) (cdr arglist))
     (run-test nil)
     (connections nil)
     (threads nil)
     (duration nil)
     (gc-print nil)
     (gc-stats nil)
     (no-exit nil)
     (dup-arg-fmt "Duplicate parameter: ~a~%")
     (missing-value-fmt "value missing for parameter: ~a~%"))
    ((null arglist)
     (when run-test
       ;; this is the child: run a test and maybe exit
       (run-test :connections (or connections 10)
		 :threads (or threads 10)
		 :duration (or duration 30))
       (unless no-exit
	 (exit 0))))
  (let ((arg (intern (car arglist))))
    (macrolet ((arg-value (name &optional boolean)
		 `(if* ,name
		     then (format t dup-arg-fmt arg)
			  (exit 1)
		   elseif (null (cdr arglist))
		     then (format t missing-value-fmt arg)
			  (exit 1)
		     else (pop arglist)
			  ,(if* boolean
			      then '(not (string-equal "nil" (car arglist)))
			      else '(car arglist)))))
      (case arg
	(connections (setq connections (arg-value connections)))
	(threads (setq threads (arg-value threads)))
	(duration (setq duration (arg-value duration)))
	(gc-print (setq gc-print (arg-value gc-print :boolean)))
	(gc-stats (setq gc-stats (arg-value gc-stats :boolean)))
	(run-test
	 (if* run-test
	    then (format t "Duplicate parameter: run-test~%")
		 (exit 1)
	    else (setq run-test t)))
	(no-exit
	 (if* no-exit
	    then (format t "Duplicate parameter: no-exit~%")
	    else (setq no-exit t)))
	(t
	 (format t "Unexpected parameter: ~a~%" arg)
	 (exit 1))))))

;; The rest of this file will not be read by the child lisp unless
;; no-exit was specified.

;; comp-test is the function that runs a set of tests and captures the output
;; for analysis. It takes the following keyword arguments:
;;
;;  test-set        defaults to the value of *default-test-set*
;;    specifies the lisps to be tested and where the output is saved.
;;
;;  duration        defaults to 30
;;    specifies the duration argument for the execution of wrk

(defun comp-test (&key (test-set *default-test-set*)
		       (duration 30))
		       
  (dolist (exec.log test-set)
    (multiple-value-bind (out err ret)
	(command-output
	 (format nil "~a -L wrktest.cl -- duration ~a run-test > ~a"
		 (car exec.log)
		 duration
		 (cdr exec.log))
	 :whole t)
      (declare (ignore out err))
      (format t "executable ~a finished, ret=~a~%" (car exec.log) ret)
      (when (eq 0 ret)
	(extract (cdr exec.log))))))

;; The following functions are used to display a simple summary of one test's results.

(defun next-marker-line (file)
  (let ((marks 0))
  (loop
    (let ((c (read-char file nil nil)))
      (unless c
	(return nil))
      (if* (eq c #\>)
	 then (incf marks)
	      (when (eq marks 3)
		(return (read-line file nil nil)))
	 else (setq marks 0))))))

(defun extract (filename)
  (with-open-file (f filename)
    (do ((marker-line (next-marker-line f) (next-marker-line f)))
	((null marker-line)
	 (format t "-----~%"))
      (format t ">>>~a~%" marker-line)
      (when (string-equal marker-line " Test finished:")
	(return)))
    (do ((line (read-line f nil nil) (read-line f nil nil)))
	((or (null line) (string-equal line ">>>---"))
	 (format t "-----~%"))
      (format t "~a~%" line))))

