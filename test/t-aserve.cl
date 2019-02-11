;; -*- mode: common-lisp; package: net.aserve.test -*-
;;
;; t-aserve.cl
;;
;; See the file LICENSE for the full license governing this code.
;;
;;

;; Description:
;;  
;; This test suite for aserve can be run in several different test modes.
;;
;; THIS FILE MUST BE COMPILED WITH THE CURRENT DIRECTORY 
;;  set to the aserve source directory,
;;  ie ./examples/ should be the aserve/examples/ directory.
;; 
;;  (test-aserve test-timeouts)  -- Run tests in one server
;;  (test-aserve-n :n n option...)   -- Run tests in n servers
;;        n=nil or 0 --- just like test-aserve in this thread
;;        n>0  --- run n servers in n new threads
;;        n=1 is like n=nil or 0 but leaves the initial listener 
;;            free for console interactions
;;
;; WHEN WRITING A NEW TEST:
;;  - Do not use global variables specific to a test. When stress-aserve
;;    is running, several instances of the same test will be running in
;;    the same Lisp address space.  Each instance has its own test config
;;    accessed with the asc macro;  it can be used to hold or point to 
;;    private test data.
;;  - Do not use constant file names for the same reason as above.
;;    Use the value of (asc index) to generate a name based on the
;;    test instance.
;;
;; SPECIAL TEST MODE OPTION FOR STRESS TESTS:
;; (pause-test-on-break onoff)  When called with non-nil, run the tests with
;;    :notrap and if any thread calls break, signal all other threads to stop 
;;    as soon as possible.   See stopper functions for details of when and how.
;;  Also enabled by setting ASERVE_PAUSE_ON_BREAK.
;;   This option is most useful when running the tests from an interactive
;;   top-level.
;; Running with this option enabled in -batch mode will only yield a 
;;   backtrace of the failing thread, and the run will be cancelled with 
;;   Lisp exit.  
;; In addition, this option should be disabled before running webactions 
;;   tests because these depend on :notrap being nil for non-stop operation.
;;   This is done automatically in a normal test run

(eval-when (compile load eval)
  (require :tester)
  (require :aserve)
  )

;; This hack is needed to allow running with older versions of aserve.
(defvar user::*default-log-wserver-name* t)

(eval-when (compile load eval)
  (when (not (boundp  'net.aserve::*log-wserver-name*))
    ;; With older aserve, there is no wserver-name slot, so the messages
    ;; will be harder to identify.
    (defvar net.aserve::*log-wserver-name* nil)
    (defun net.aserve::wserver-name (x) (declare (ignore x)) "")
    (setq user::*default-log-wserver-name* nil)
    )
  (set 'excl::*aclssl-verbose* t)
  )


(defpackage :net.aserve.test
  (:use :common-lisp :excl :net.html.generator :net.aserve 
	:net.aserve.client
	:util.test)
  (:import-from :net.aserve #:*log-wserver-name* #:wserver-name #:logmess)
  )

(in-package :net.aserve.test)

(eval-when (compile eval load)
  (defvar *aserve-examples-directory*
      (or (probe-file "aserve/examples/")
	  (probe-file "examples/")
	  (error "Could not find the aserve examples directory.")))
  )

(defvar *aserve-test-dir* 
    ;; the full path to aserve/test directory with no trailing slash
    (namestring (pathname-as-file (path-namestring *load-pathname*)))
  )



(defparameter *aserve-set-full-debug*  nil) ; :all and :notrap are useful
(when *aserve-set-full-debug*
  (apply #'net.aserve::debug-on *aserve-set-full-debug*))

; to trap errors when they happen uncomment this
#+ignore 
(setq util.test:*break-on-test-failures* t
      util.test:*error-protect-tests*   nil)


; Set to nil before loading the test to prevent the test from auto-running
; or set to the number of threads/servers to run simultaneously.
; Default is to run one server in the main thread.
(defvar user::*do-aserve-test* 0)

; if true run timeout test
(defvar *test-timeouts* nil)

(defvar-nonbindable *asc-stop-mode*
    ;; Global stop mode ->  (indicator stop-flag stop-gate)
    ;; stop-flag -> nil  
    ;;           -> :kill     Throw to asc-stopper-catch or kill the current process.
    ;;           -> :pause    Sleep forever or wait for gate to open.
    ;; indicator starts as nil; is set to t when error break is entered.
    nil)

(defvar *aserve-test-config* nil)

(defclass aserve-test-config ()
  ;; Keep a separate test config for each server.
  (
   (name    :reader asc-name :initarg :name :initform "")
   (index   :reader asc-index :initarg :index :initform "")
   (x-proxy :accessor asc-x-proxy 
	    ;; when true x-do-http-request will go through a proxy
	    :initform nil)
   (x-ssl   :accessor asc-x-ssl   
	    ;; true when we want to do ssl client calls
	    :initform nil)
   (x-compress :accessor asc-x-compress
	       ;; true when compressing
	       :initform nil)
   (proxy-wserver :accessor asc-proxy-wserver :initform nil)
   (test-timeouts :accessor asc-test-timeouts :initarg :test-timeouts
		  :initform *test-timeouts*)
   (save-x-proxy  :accessor asc-save-x-proxy 
		  ;; stack of old values
		  :initform nil)
   (save-proxy-wserver :accessor asc-save-proxy-wserver
		       ;; stack of old values
		       :initform nil)
   (wserver :accessor asc-wserver :initform nil)
   (done    :accessor asc-done    :initform nil) 
   ))

(defmacro asc (slot &aux (accessor
			  (read-from-string
			   (format nil "~A::asc-~A "
				   (package-name 
				    (find-package :net.aserve.test))
				   slot))))
  `(,accessor *aserve-test-config*))

(defun set-stop-mode (value &optional gated)
  ;; Set the stop flag.
  (ecase value 
    ((:pause :kill)  
     (setq *asc-stop-mode* (list nil value (when gated (mp:make-gate nil)))))
    ((nil)
     (when (and (consp *asc-stop-mode*) (third *asc-stop-mode*))
       (mp:open-gate (third *asc-stop-mode*)))
     (setq *asc-stop-mode* nil))))

(defun set-stop-indicator (&rest args)
  (declare (ignore args))
  (when (consp *asc-stop-mode*) (setf (car *asc-stop-mode*) t)))

(def-fwrapper wrapper-with-check-stopper (&rest args)
  (declare (ignorable args))
  ;; Generic fwrapper that can be wrapped around any function where a stopper
  ;;  check could be useful.
  (net.aserve.test::asc-check-stopper)
  (multiple-value-prog1 (call-next-fwrapper)
    (net.aserve.test::asc-check-stopper)))

(defun pause-test-on-break (onoff &optional gated)
  (ecase onoff
    ((:pause :kill)
     (format t "~&; ASERVE TEST - Enabling pause-test-on-break ~S ~S" onoff gated)
     (set-stop-mode onoff gated)
     (net.aserve::debug-on :notrap)
     (fwrap 'util.test::test-check :aserve-stopper 'wrapper-with-check-stopper)
     (setq excl::*internal-invoke-debugger-hook* 'set-stop-indicator))
    ((nil)
     (format t "~&; ASERVE TEST - Disabling pause-test-on-break.")
     (set-stop-mode nil)
     (net.aserve::debug-off :notrap)
     (funwrap 'util.test::test-check :aserve-stopper)
     (setq excl::*internal-invoke-debugger-hook* nil))))

(defvar *asc-stopper-catch* nil)

(defun asc-check-stopper (&optional from-where &aux stopper stop-flag stop-gate)
  ;; Check the stopper and pause, throw, kill or wait as required.
  (when (consp (setq stopper *asc-stop-mode*))
    (when (first stopper)
      (setq stop-flag (second stopper))
      (setq stop-gate (third stopper))
      (case stop-flag
	(:kill (if *asc-stopper-catch* 
		   (throw 'asc-stopper-catch from-where)
		 (mp:process-kill mp:*current-process*)))
	(otherwise (if stop-gate
		       (mp:process-wait "stopper" #'mp:gate-open-p stop-gate) 
		     (sleep most-positive-fixnum))))
      )))

(defmacro with-stopper-catch (name &body body)
  `(let ((*asc-stopper-catch* ,name))
     (catch 'asc-stopper-catch ,@body)))
  

(defmacro with-stopper-checks (options &body body)
  ;; Insert stopper checks beween forms in body.
  ;; Options may be needed in the future for more detailed checks.
  (declare (ignore options))
  `(multiple-value-prog1 
       (progn ,@(mapcan (lambda (form) `((asc-check-stopper) ,form)) body))
       (asc-check-stopper)))



; remember where we were loaded from so we can run manually
(defparameter *aserve-load-truename* *load-pathname*)

(defvar *aserve-test-configs* nil)

(defun asp-index ()
  (if *log-wserver-name*
      #-(version>= 10 1)
      (format nil "~A[Bix=~A]"  ;; bug24565
	      (mp:process-name mp:*current-process*)
	      (sys::thread-bindstack-index
	       (mp:process-thread mp:*current-process*)))
      #+(version>= 10 1)
      (format nil "~A[~A]" 
	      (mp:process-name mp:*current-process*)
	      (mp:process-sequence mp:*current-process*)) ;; bug24565
    ""))

(defun user::test-aserve-n (&key (n
				  ;; In 10.0 beta testing, ran into malloc issues creating
				  ;; a lot of threads. These minimal settings should allow
				  ;; tests to run successfully on all platforms. [bug23208]
				  #+(and smp 64bit) 3
				  #-(and smp 64bit) 2
				  )
				 (test-timeouts *test-timeouts*) (delay 0) logs
				 (direct t) (proxy t) (proxyproxy t) (ssl t)
				 (proxy-auth t)
				 (name "ast") (log-name nil l-n-p)
				 (wait t) ; wait for tests to finish
				 (exit nil) ; ignored if wait=nil
				 (pause-on-break (when (excl.osi:getenv "ASERVE_PAUSE_ON_BREAK") :pause))
			    &aux wname)
  (unwind-protect
      (let ()
	(when pause-on-break 
	  (pause-test-on-break pause-on-break))	
	(typecase n
	  ((integer 0) nil)
	  (null (return-from user::test-aserve-n nil))
	  (otherwise 
	   ;; In case someone sets *do-aserve-test* to t.
	   (setq n 0)))
	(setq *aserve-test-configs* (make-array (if (eql n 0) 1 n)))
  
	(format t "~&; *hiper-socket-is-stream-socket* = ~S~%" 
		(let (v) 
		  (or (ignore-errors (setq v (eval 'excl::*hiper-socket-is-stream-socket*)) t)
		      (setq v :undefined))
		  v))
  
	(case n
	  (0
	   ;; In simple one-thread test, log server name only if requested
	   ;; explicitly.
	   (if l-n-p
	       (setq *log-wserver-name* log-name)
	     (setq *log-wserver-name* nil))
	   (with-stopper-catch
	       :single-test-instance
	     (test-aserve test-timeouts :direct direct :proxy proxy :proxyproxy proxyproxy 
			  :ssl ssl :proxy-auth proxy-auth)))
	  (otherwise
	   (let ((procs '()))
	     (when (cond (l-n-p (setq *log-wserver-name* log-name))
			 ((eql n 1) (setq *log-wserver-name* nil))
			 (t  (setq *log-wserver-name* user::*default-log-wserver-name*)))
	       (when (boundp 'util.test::*test-report-thread*)
		 (set 'util.test::*test-report-thread* t)))	 
	     (dotimes (i n)
	       (push
		(mp:process-run-function
		    (setq wname (format nil "~A~A" i name))
		  (lambda (i name)
		    (let* (os
			   clean 
			   (*standard-output*
			    (if logs
				(setq os
				  (open (format nil "~A~A.log" logs i) :direction :output
					:if-exists :supersede))
			      *standard-output*))
			   (*aserve-test-config* 
			    (setf (aref *aserve-test-configs* i)
			      (make-instance 'aserve-test-config
				:name name :index i
				:test-timeouts test-timeouts)))
			   (*wserver* (apply #'make-instance 'wserver 
					     (when user::*default-log-wserver-name* (list :name name)))))
		      (unwind-protect
			  (let ()
			    (asc-format "~&~%============ STARTING SERVER ~A ~A ~%~%" i name)
			    (setf (asc wserver) *wserver*)
			    (with-stopper-catch
				wname
			      (test-aserve test-timeouts
					   :direct direct :proxy proxy :proxyproxy proxyproxy :ssl ssl
					   :proxy-auth proxy-auth
					   )
			      (setq clean t)))
			(asc-format "~&~%============ ENDING SERVER ~A ~A ~A ~%~%" i name 
				    (if clean "normally" "ABRUPTLY"))
			(when os (close os))
			(setf (asc done) (if clean :clean :abrupt))
			(dotimes (j (length *aserve-test-configs*)
				   (format *initial-terminal-io* "~&~%~%ALL SERVERS ENDED~%~%"))
			  (or (asc-done (aref *aserve-test-configs* j)) (return)))
			)))
		  i wname)
		procs)
	       ;; should be able to handle a delay = 0
	       (sleep delay))

	     (when wait
	       (dolist (p procs)
		 (mp:process-wait
		  (format nil "waiting for ~a"
			  (mp:process-name p))
		  (lambda (p) (eq :terminated (mp::process-state p)))
		  p))))))
	(when wait
	  (let ((code (+ util.test::*test-unexpected-failures*
			 util.test::*test-errors*)))
	    (if* exit
	       then (exit code :quiet t)
	       else code))))
    (cond ((null pause-on-break))
	  (wait (pause-test-on-break nil))
	  (t 	(format t "~&; test-aserve-n: pause-test-on-break is left enabled.~%")))))


(defvar *asc-lock* (mp:make-process-lock :name "asc"))
(defun asc-format (fmt &rest args)
  (mp:with-process-lock
   (*asc-lock*)
   (cond
    ((or (equal 0 (search "~&~%" fmt)) (equal 0 (search "~%~%" fmt)))
     (format t "~&~%~A ~A" (asp-index) (apply #'format nil (subseq fmt 4) args)))
    ((or (equal 0 (search "~&" fmt)) (equal 0 (search "~%" fmt)))
     (format t "~&~A ~A" (asp-index) (apply #'format nil (subseq fmt 2) args)))
    (t (format t "~&~A ~A~%" (asp-index) (apply #'format nil fmt args))))))
			

(defun test-aserve (test-timeouts &key (direct t) (proxy t) (proxyproxy t) (ssl t)
				       (proxy-auth t))
  ;; run the allegroserve tests three ways:
  ;;  1. normally
  ;   2. through an allegroserve proxy to test the proxy
  ;;  3. through ssl (if ssl module present)
  ;;
  ;; tests are run on a variety of threads, so we have to 
  ;; account for those other thread errors separately.
  (setq util.test::*test-errors* 0
        util.test::*test-successes* 0
	util.test::*test-unexpected-failures* 0)
  (with-tests (:name "aserve")
    (let* ((*wserver* *wserver*)
	   (port (start-aserve-running))
           (https nil))
      (asc-format "server started on port ~d" port)
      (unwind-protect 
	  (labels ((do-tests ()
		     ; run test with and without compression
		     ; accepted by do-http-request
		     (dolist (cv (if* (member :zlib-deflate *features*)
				    then '(nil t)
				    else '(nil)))
		       (let ((prev (asc x-compress)))
			 (unwind-protect
			     (progn
			       (setf (asc x-compress) cv)
			       (format t "~2%Compress ~s~2%" cv)
			       (do-tests-inner))
			   (setf (asc x-compress) prev)))))
		   (do-tests-inner ()
		     (with-stopper-checks
			 nil
		       (test-publish-file port nil) ; no compression
		       #+unix
		       (test-publish-file port t) ; with compression
		       (test-publish-directory port)
		       (test-publish-computed port)
		       (test-publish-multi port)
		       (test-publish-prefix port)
		       (test-put-patch port)
		       (test-authorization port)
		       (test-encoding)
		       (test-truncated-stream)
		       (test-forms port)
		       (test-get-request-body-incr port)
		       (test-request-character-encoding port)
		       (test-client port)
		       (test-cgi port)
		       (test-http-copy-file port)
		       (test-client-unicode-content-length)
		       (test-expect-header-responses)
		       (test-retry-on-timeout port)
		       (test-chunked-request port https)
		       (test-chunked-request-set-trailers port https)
		       (test-chunked-request-set-trailers-while-debugging port https)
		       (test-server-request-body port :https https)
		       (test-request-uri port https)
		       (test-spr44282)
                     
		       (if* (member :ics *features*)
			  then (test-international port)
			       (test-spr27296))
		       (if* test-timeouts 
			  then (test-timeouts port))
		       (test-body-in-get-request :port port :ssl https)	 ;;; rfe15456
		       )))
	    
	    
	    (with-stopper-checks
		nil
	    (if*  direct
	       then (asc-format "~%~%===== test direct ~%~%")
		    (do-tests))
	    
	    (if* proxy-auth
	       then (asc-format "~%~%===== test proxy authorization ~%~%")
		    (test-proxy-auth))
	    
	    (if* proxy
	       then (asc-format "~%~%===== test through proxy ~%~%")
		    (start-proxy-running t)
		    (do-tests))
	    
	    (if* proxyproxy
	       then (asc-format "~%~%===== test through proxy to proxy~%~%")
		    (start-proxy-running t)
		    (do-tests))
	    
	    (if* ssl
	       then (asc-format "~%>> checking to see if ssl is present~%~%")
		    (if* (errorset (as-require :ssl))
		       then ; we have ssl capability, run tests through ssl
			    (stop-proxy-running)
			    (stop-proxy-running)
			    (stop-aserve-running)
			    (asc-format "~%~%===== test through ssl ~%~%")
			    (setq port (start-aserve-running 
					(merge-pathnames 
					 "server.pem" *aserve-load-truename*)))
                            (setq https t)
			    (do-tests)
		       else (asc-format "~%>> it isn't so ssl tests skipped~%~%")))
	    )) ;;; end body of unwind-protect 
	; cleanup forms:
	(stop-aserve-running)
	(stop-proxy-running)
	(stop-proxy-running)
	))
    
    ;; independent test of a stream used in aserve
    (test-force-output-prepend-stream)
    
    ;; test the caching object
    (test-caching)
    
    (if* (and ssl (errorset (as-require :ssl)))
       then 
	    (test-aserve-extra-ssl)
	    (test-aserve-ssl-redirect) 
	    )
    )
  (if* (or (> util.test::*test-errors* 0)
	   (> util.test::*test-successes* 0)
	   (> util.test::*test-unexpected-failures* 0))
     then (asc-format "Test information from other threads:")
	  (asc-format "Errors:    ~d" util.test::*test-errors*)
	  (asc-format "Successes: ~d~%" util.test::*test-successes*)
	  (asc-format "Unexpected failures: ~d~%" 
		      util.test::*test-unexpected-failures*)))
    

(defun log-wserver-name (&optional  (from "") (wserver *wserver*) &aux (*wserver* wserver))
  (logmess (format nil "~A~Awserver name is now ~A at port ~A" 
		   (or from "") (if from " " "") (wserver-name wserver)
		   (socket::local-port (net.aserve::wserver-socket wserver))
		   )))

(defun start-aserve-running (&optional ssl (test-ssl t))
  ;; start aserve, return the port on which we've started aserve
  (let ((wserver (start :port nil :server :new :ssl-args (and ssl (list :certificate ssl)) 
			:test-ssl test-ssl
	
			:listeners 20 ; must be at least 3 for keep-alive to be possible
			)))		; let the system pick a port
    (log-wserver-name "" wserver)
    (setq *wserver* wserver)
    (when *aserve-set-full-debug*
      (apply #'net.aserve::debug-on *aserve-set-full-debug*))
    (unpublish :all t) ; flush anything published
    (setf (asc x-ssl) ssl)
    (socket::local-port (net.aserve::wserver-socket wserver))
    ))

(defun stop-aserve-running ()
  (shutdown))


(defun start-proxy-running (proxy)
  ;; start another web server to be the proxy
  (push (asc proxy-wserver) (asc save-proxy-wserver))
  
  (setf (asc proxy-wserver) (start :server :new 
			       :port nil 
			       :proxy proxy
			       :proxy-proxy (asc x-proxy)))
  (let ((*wserver* (asc proxy-wserver)))
    (when *aserve-set-full-debug*
      (apply #'net.aserve::debug-on *aserve-set-full-debug*))
    (log-wserver-name "proxy"))
  
  (push (asc x-proxy) (asc save-x-proxy))
  (setf (asc x-proxy) (format nil "localhost:~d" 
			  (socket:local-port
			   (wserver-socket (asc proxy-wserver)))))
  )


(defun stop-proxy-running ()
  (if* (asc proxy-wserver)
     then (shutdown :server (asc proxy-wserver))
	  (setf (asc proxy-wserver) (pop (asc save-proxy-wserver))))
  (setf (asc x-proxy) (pop (asc save-x-proxy))))

	  

  


(defun x-do-http-request (uri &rest args)
  ;; add a proxy arg
  (apply #'do-http-request uri :proxy (asc x-proxy) 
	 :compress (asc x-compress)
	 :ssl (asc x-ssl) args))



(defun x-make-http-client-request (uri &rest args)
  ;; add a proxy arg
  (apply #'make-http-client-request uri :proxy (asc x-proxy) 
	 :compress (asc x-compress)
	 :ssl (asc x-ssl) args))



(defun chunk-encode-string (text &optional trailers)
  "Encodes TEXT using HTTP/1.1 chunk encoding.
TRAILERS is an alist of headers, both keys and values should be strings.
Returns a vector."
  (with-output-to-buffer (result)
    (let ((chunker
           (make-instance 'net.aserve::chunking-stream
             :output-handle result))) 
      (setf (chunking-stream-trailers chunker) trailers)
      (unwind-protect 
          (write-string text chunker)
        (close chunker)))))



(defmacro values2 (form)
  ;; return the second value
  (let ((v1 (gensym))
	(v2 (gensym)))
    `(multiple-value-bind (,v1 ,v2) ,form
       (declare (ignore ,v1))
       ,v2)))

;-------- publish-file tests

(defun build-dummy-file (length line-length name compress)
  ;; write a dummy file named  name  (if name isn't nil)
  ;; of a given   length   with spaces every line-length characters
  ;; Return the string holding the contents of the file.
  (let ((strp (make-string-output-stream))
	(result))
    (dotimes (i length)
      (write-char (code-char (+ #.(char-code #\a) (mod i 26))) strp)
      (if* (zerop (mod (1+ i) line-length))
	 then ; newlines cause a problem due to dos/unix differences.
	      ; so let's just use space
	      (write-char #\space strp)))
    (setq result (get-output-stream-string strp))
    (if* name
       then (with-open-file (p name :direction :output
			     :if-exists :supersede)
	      (write-sequence result p)))
    (if* compress
       then
	    #+unix(run-shell-command (format nil "gzip -c ~a > ~a.gz"
					     name name))
	    nil
       else #+unix(ignore-errors
		   ;; get rid of the compressed file if it exists
		   (delete-file (format nil "~a.gz" name)))
	    nil)
    
    result))
  

(defun test-publish-file (port compress)
  (let (dummy-1-contents 
	(dummy-1-name (format nil "x~Axaservetest.txt" (asc index)))
	dummy-2-contents
	(dummy-2-name (format nil "x~Ax2aservetest.txt" (asc index)))
	(prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name)
			      port))
	(reps 0)
	(got-reps nil))
    
    (setq dummy-1-contents (build-dummy-file 8055 70 dummy-1-name
					     compress))

    
    ;; basic publish file test
    ;;
    ;; publish a file and retrieve it.
    ;; the result will be the same since given that we know the
    ;; length of the file, chunking won't be needed
    ;; 
    (let ((ent (publish-file :path "/frob" :file dummy-1-name
			     :content-type "text/plain"
			     :cache-p t
			     :hook #'(lambda (req ent extra)
				       (declare (ignore req ent extra))
				       (setq got-reps (or got-reps 0))
				       (incf got-reps))
			     :headers '((:testhead . "testval"))
			     :compress compress
			     )))
      (test nil (net.aserve::contents ent)) ; nothing cached yet

      ;; 
      (dolist (cur-prefix (list prefix-local prefix-dns))
	;* don't specify keep-alive unless you're willing
	;  to close the resulting socket
	(dolist (keep-alive '(nil))
	  (dolist (protocol '(:http/1.0 :http/1.1))
	    (asc-format "test 1 - ~s" (list keep-alive protocol))
	    (multiple-value-bind (body code headers)
		(x-do-http-request (format nil "~a/frob" cur-prefix)
				   :protocol protocol
				   :keep-alive keep-alive)
	      (incf reps)
	      (test 200 code)
	      (test (format nil "text/plain")
		    (cdr (assoc :content-type headers :test #'eq))
		    :test #'equal)
	      
	      (test "testval"
		    (cdr (assoc :testhead headers :test #'equal))
		    :test #'equal)
	      
	      #+ignore (if* (eq protocol :http/1.1)
			  then (test "chunked"
				     (cdr (assoc :transfer-encoding headers 
						 :test #'eq))
				     :test #'equalp))
	      (test dummy-1-contents body :test #'equal)))))
      
      ;; stuff should be cached by now
      ;; but when doing a compressed retrieval caching isn't done
      (if* (not compress)
	 then (test t (not (null (net.aserve::contents ent)))))
      )

    (test reps got-reps)  ; verify hook function worked

    (setq dummy-2-contents (build-dummy-file 8055 65 dummy-2-name
					     compress))

    
    ;; preload publish file test
    ;;
    ;; publish a file and retrieve it.
    ;; ** Preload this time **
    ;;
    ;; the result will be the same since given that we know the
    ;; length of the file, chunking won't be needed
    ;; 
    (publish-file :path "/frob2" :file dummy-2-name
		  :content-type "text/plain"
		  :preload t
		  :headers '((:testhead . "testval"))
		  )
    
    ;; publish with no preload and no cache
    (publish-file :path "/frob2-npl" :file dummy-2-name
		  :content-type "text/plain"
		  :preload nil)
    

    ;; 
    (dolist (cur-prefix (list prefix-local prefix-dns))
      (dolist (keep-alive '(nil))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (asc-format "test 2 - ~s" (list keep-alive protocol))
	  (multiple-value-bind (body code headers)
	      (x-do-http-request (format nil "~a/frob2" cur-prefix)
				 :protocol protocol
				 :keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain")
		  (cdr (assoc :content-type headers :test #'eq))
		  :test #'equal)
	    (test "testval"
		    (cdr (assoc :testhead headers :test #'equal))
		    :test #'equal)
	    #+ignore (if* (eq protocol :http/1.1)
			then (test "chunked"
				   (cdr (assoc :transfer-encoding headers 
					       :test #'eq))
				   :test #'equalp))
	    (test dummy-2-contents body :test #'equal))
	  
	  ; try partial gets
	  (multiple-value-bind (body code headers)
	      (x-do-http-request (format nil "~a/frob2-npl" cur-prefix)
				 :protocol protocol
				 :keep-alive keep-alive
				 :headers '((:range . "bytes=100-400"))
				 )
	    (test 206 code)
	    (test "text/plain"
		  (cdr (assoc :content-type headers :test #'eq))
		  :test #'equal)	    
	    (if* (not compress)
	       then (test (subseq dummy-2-contents 100 401)
			  body :test #'equal)
	    
		    (test "bytes 100-400/8178"
			  (cdr (assoc :content-range headers :test #'eq))
			  :test #'equal))
	    
	    )
	  )))

    
    ;;;; remove published file test
    ;;
    ; verify it's still there
    (test 200 (values2 (x-do-http-request (format nil "~a/frob" prefix-local))))
    (test 200 (values2 (x-do-http-request (format nil "~a/frob" prefix-dns))))
    
    ; check that skip-body works
    (test nil (values (x-do-http-request (format nil "~a/frob" prefix-local)
					 :skip-body t)))
    
    ; remove it
    (publish-file :path "/frob" :remove t)
    
    ; verify that it's not there:
    (test 404 (values2 (x-do-http-request (format nil "~a/frob" prefix-local))))
    (test 404 (values2 (x-do-http-request (format nil "~a/frob" prefix-dns))))
    
    ;; likewise for frob2
    
    ; verify it's still there
    (test 200 (values2 (x-do-http-request (format nil "~a/frob2" prefix-local))))
    (test 200 (values2 (x-do-http-request (format nil "~a/frob2" prefix-dns))))
    
    ; remove it
    (publish-file :path "/frob2" :remove t)
    
    ; verify that it's not there:
    (test 404 (values2 (x-do-http-request (format nil "~a/frob2" prefix-local))))
    (test 404 (values2 (x-do-http-request (format nil "~a/frob2" prefix-dns))))
    
    

    
    ;; now add different files for localhost and the dns names
    ;; and verify that we get served different files based on
    ;; the virtual host we choose
    (publish-file :path "/checkit" 
		  :host "localhost"
		  :file dummy-1-name
		  :content-type "text/plain")
    
    (publish-file :path "/checkit" 
		  :host (long-site-name)
		  :file dummy-2-name
		  :content-type "text/plain")
    
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/checkit" prefix-local))
      (declare (ignore headers))
      (test 200 (and :df-test code))
      (test dummy-1-contents body :test #'equal))
    
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/checkit" prefix-dns))
      (declare (ignore headers))
      (test 200 (and :df-test code))
      (test dummy-2-contents body :test #'equal))

    ;; remove the localhost one
    (publish-file :path "/checkit" 
		  :host "localhost"
		  :remove t)
    ; verify it's gone:
    (test 404 (values2 (x-do-http-request (format nil "~a/checkit" 
						  prefix-local))))
    ; but the the dns one is still there
    (test 200 (values2 (x-do-http-request (format nil "~a/checkit" prefix-dns))))
    
    ; remove the dns one
    (publish-file :path "/checkit" 
		  :host (long-site-name)
		  :remove t)
    
    ; verify it's gone too
    (test 404 (values2 (x-do-http-request (format nil "~a/checkit" 
						  prefix-dns))))

    
    

    (setq dummy-1-contents (build-dummy-file 432 23 dummy-1-name
					     compress))
    
    ; test caching and auto uncaching and recaching
    (let ((ent (publish-file :path "/check-uncache"
			     :file dummy-1-name
			     :cache-p t)))

      ; verify nothing cached right now
      (if* (not compress)
	 then (test nil (and :second (net.aserve::contents ent))))
      
      (let ((body2 (x-do-http-request (format nil "~a/check-uncache" 
					      prefix-local))))
	
	; verify result was correct
	(test dummy-1-contents body2 :test #'equal)

	; verify that something's cached.
	(if* (not compress)
	   then (test t (not (null (and :second (net.aserve::contents ent))))))

	; overwrite dummy file with new contents
	(sleep 2) ; pause to get file write date to noticably advance
	(setq dummy-1-contents (build-dummy-file 555 44 dummy-1-name
						 compress))
	
	; verify that the contents are in fact different
	(test nil (equal dummy-1-contents body2))

	; now do the same request.. but we should get new things back
	; since the last modified time of the file
	(setq body2
	  (x-do-http-request (format nil "~a/check-uncache" prefix-local)))
	; verify that we did get the new stuff back.
	
	(test t (equal dummy-1-contents body2))))
    
    ; rewrite file with different contents
    
    
    
    
    ; cleanup
    (delete-file dummy-1-name)
    (delete-file dummy-2-name)
    ))
    



(defun test-publish-computed (port)
  ;; test publishing computed entities
  (let ((dummy-1-content (build-dummy-file 0 50 nil nil))
	(dummy-2-content (build-dummy-file 1 50 nil nil))
	(dummy-3-content (build-dummy-file 100 50 nil nil))
	(dummy-4-content (build-dummy-file 1000 50 nil nil))
	(dummy-5-content (build-dummy-file 10000 50 nil nil))
	(dummy-6-content (build-dummy-file 100000 50 nil nil))
	
	(prefix-local (format nil "http://localhost:~a" port))
	)

    ;;
    ;; publish strings of various sizes using various protocols
    ;; verify that chunking is turned on when we select http/1.1
    ;; 
    (dolist (pair `(("/dum1" ,dummy-1-content)
		    ("/dum2" ,dummy-2-content)
		    ("/dum3" ,dummy-3-content)
		    ("/dum4" ,dummy-4-content)
		    ("/dum5" ,dummy-5-content)
		    ("/dum6" ,dummy-6-content)))

      (let ((this (cadr pair)))
	;; to make a separate binding for each function
	(publish :path (car pair) 
		 :content-type "text/plain"
		 :headers '((:testhead . "testval"))
		 :function
		 #'(lambda (req ent)
		     (with-http-response (req ent)
		       (with-http-body (req ent)
			 (write-sequence this *html-stream*))))))
      (dolist (keep-alive '(nil))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (multiple-value-bind (body code headers)
	      (x-do-http-request (format nil "~a~a" prefix-local (car pair))
				 :protocol protocol
				 :keep-alive keep-alive)
	    (test 200 code)
	    (test "testval"
		    (cdr (assoc :testhead headers :test #'equal))
		    :test #'equal)
	    (test (format nil "text/plain" port)
		  (cdr (assoc :content-type headers :test #'eq))
		  :test #'equal)
	    (if* (and (eq protocol :http/1.1)
		      (null (asc x-proxy))
		      (null (asc x-ssl))
		      )
	       then (test "chunked"
			  (cdr (assoc :transfer-encoding headers 
				      :test #'eq))
			  :test #'equalp))
	    (test (cadr pair) body :test #'equal)))))
    
    
    ;; test whether we can read urls with space in them
    (publish :path "/foo bar baz"
	     :content-type "text/plain"
	     :function
	     #'(lambda (req ent)
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (write-sequence "foo" *html-stream*)))))
    (multiple-value-bind (body code)
	(x-do-http-request (format nil "~a/foo%20bar%20baz" prefix-local))
      (test 200 code)
      (test "foo" body :test #'equal))

    
    ;; test we can send non-standard headers back and forth
    
    (publish :path "/unusual-headers"
	     :content-type "text/plain"
	     :function 
	     #'(lambda (req ent)
		 
		 (test "booboo" (header-slot-value req :frobfrob)
		       :test #'equal)
		 (with-http-response (req ent)
		   (setf (reply-header-slot-value req :snortsnort)
		     "zipzip")
		   (with-http-body (req ent)
		     (html "foo the bar")))))
    
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/unusual-headers" prefix-local)
			   :headers '(("frobfrob" . "booboo")))
      (declare (ignore body))
      
      (test 200 code)
      (test "zipzip" (cdr (assoc :|snortsnort| headers :test #'equalp))
	    :test #'equal))
    
    ;; test that if an error occurs we don't send out the
    ;; header before we encounter the error
    (publish :path "/error-computed-error"
	 :content-type "text/plain"
	 :function
	 #'(lambda (req ent)
	     ;; this will get an error after the header is geneated
	     ;; but before the first body output is done
	     ;; this will test the delayed header send.
	     ;; the user should see a 500 "internal server error"
	     (handler-case
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     ; make an error
		     (let ((a (+ 1 2 3 :bogus)))
		       (+ a a))
		     (html "done")))
	       (error (c)
		 (with-http-response (req ent 
					  :response 
					  *response-internal-server-error*
					  :content-type
					  "text/html")
		   (with-http-body (req ent)
		     (html (:head (:title "Internal Server Error"))
			   (:body "As expected this entity caused error " 
				  (:princ c)))))))))
    
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/error-computed-error" prefix-local)
			   :headers '(("frobfrob" . "booboo")))
      (declare (ignore body headers))
      
      (test 500 code))
    
    ))


(defun test-authorization (port)
  (let ((prefix-local  (format nil "http://localhost:~a" port))
        (prefix-dns    (format nil "http://~a:~a" (long-site-name) port))
        (long-password "abcdefghijklmnopqrstuvwxyz1234567890"))

    ;; manual authorization testing
    ;; basic authorization
    ;;
    (publish :path "/secret"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 (multiple-value-bind (name password) (get-basic-authorization req)
                   (if* (and (equal name "foo")
                             (or (equal password "bar")
                                 (equal password long-password)))
		      then (with-http-response (req ent)
			     (with-http-body (req ent)
			       (html (:head (:title "Secret page"))
				     (:body "You made it to the secret page"))))
		      else
			   (with-http-response (req ent :response 
						    *response-unauthorized*)
			     (set-basic-authorization req
						      "secretserver")
			     (with-http-body (req ent)))))))
    
    ; no dice with no password
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/secret" prefix-local))
      (declare (ignore body))
      (test 401 code)
      ; verify that we are asking for the right realm
      (test "Basic realm=\"secretserver\""
	    (cdr (assoc :www-authenticate headers :test #'eq))
	    :test #'equal))
  
    
    ; good password
    (test 200
	  (values2 (x-do-http-request (format nil "~a/secret" prefix-local)
				      :basic-authorization '("foo" . "bar"))))

    ; long password
    (test 200
          (values2 (x-do-http-request (format nil "~a/secret" prefix-local)
                                      :basic-authorization `("foo" . ,long-password))))
    
    ; bad password
    (test 401
	  (values2 (x-do-http-request (format nil "~a/secret" prefix-local)
				      :basic-authorization '("xxfoo" . "bar"))))
    
    ; auth via userinfo
    (test 200
	  (values2 (x-do-http-request (format nil "http://foo:bar@localhost:~a/secret" port))))

    ; long password auth via userinfo
    (test 200
	  (values2 (x-do-http-request (format nil "http://foo:~a@localhost:~a/secret" long-password port))))


    ; test conflicting auth headers.
    (let ((bad-auth '("bad" "password"))
          (bad-auth-encoded (net.aserve::base64-encode "bad:password"))
          (good-auth '("foo" . "bar"))
          (good-auth-encoded (net.aserve::base64-encode "foo:bar")))

      ;; good password first.
      (test-error (x-do-http-request (format nil "~a/secret" prefix-local)
                                     :basic-authorization good-auth
                                     :headers (list (cons "Authorization"
                                                          (format nil "Basic ~a" bad-auth-encoded)))))

      ;; good password second.
      (test-error (x-do-http-request (format nil "~a/secret" prefix-local)
                                     :basic-authorization bad-auth
                                     :headers (list (cons "Authorization"
                                                          (format nil "Basic ~a" good-auth-encoded)))))

      ;; all auth's simultaneously (equal)
      (test 200
        (values2 (x-do-http-request (format nil "http://foo:bar@localhost:~a/secret" port)
                                    :basic-authorization good-auth
                                    :headers (list (cons "Authorization"
                                                         (format nil "Basic ~a" good-auth-encoded))))))

      ;; all auth's simultaneously (conflicting)
      (test-error (x-do-http-request (format nil "http://foos:barx@localhost:~a/secret" port)
                                     :basic-authorization good-auth
                                     :headers (list (cons "Authorization"
                                                          (format nil "Basic ~a" bad-auth-encoded)))))

      
      )
    
    ;; manual authorization testing, testing via ip address
    
    (publish :path "/local-secret"
	     ;; this only "works" if we reference via localhost
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 (let ((net-address (ash (socket:remote-host
					  (request-socket req))
					 -24)))
		   (if* (equal net-address 127)
		      then (with-http-response (req ent)
			     (with-http-body (req ent)
			       (html (:head (:title "Secret page"))
				     (:body (:b "Congratulations. ")
					    "You are on the local network"))))
		      else (failed-request req)))))
    
    (test 200
	  (values2 (x-do-http-request (format nil "~a/local-secret"
					      prefix-local))))
    
    (test 404
	  (values2 (x-do-http-request (format nil "~a/local-secret"
					      prefix-dns))))
    
    
    ;;
    ;; password authorizer class
    ;;
    (publish :path "/secret-auth"
	     :content-type "text/html"
	     :authorizer (make-instance 'password-authorizer
			   :allowed '(("foo2" . "bar2")
				      ("foo3" . "bar3")
				      )
			   :realm  "SecretAuth")
	     :function
	     #'(lambda (req ent)
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html (:head (:title "Secret page"))
			   (:body "You made it to the secret page"))))))
    
    (multiple-value-bind (body ccode headers)
	(x-do-http-request (format nil "~a/secret-auth" prefix-local))
      (declare (ignore body))
      (test 401 ccode)
      (test "Basic realm=\"SecretAuth\""
	    (cdr (assoc :www-authenticate headers :test #'eq))
	    :test #'equal))
    
    (test 200
	  (values2 (x-do-http-request (format nil "~a/secret-auth" prefix-local)
				      :basic-authorization '("foo2" . "bar2"))))
    
    (test 200
	  (values2 (x-do-http-request (format nil "~a/secret-auth" prefix-local)
				      :basic-authorization '("foo3" . "bar3"))))
    
    (test 401
	  (values2 (x-do-http-request (format nil "~a/secret-auth" prefix-local)
				      :basic-authorization '("foo4" . "bar4"))))
    

    ;;
    ;; location authorizers
    ;; 
    (let ((loca (make-instance 'location-authorizer
		  :patterns nil)))
      (publish :path "/secret-loc-auth"
	       :content-type "text/html"
	       :authorizer loca
	       :function
	       #'(lambda (req ent)
		   (with-http-response (req ent)
		     (with-http-body (req ent)
		       (html (:head (:title "Secret page"))
			     (:body "You made it to the secret page"))))))
      
      ;; with a nil pattern list this should accept connections
      ;; from anywhere
      
      (test 200
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-local))))
      (test 200
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-dns))))
      
      ; now deny all
      (setf (location-authorizer-patterns loca) '(:deny)) 
      
      (test 404
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-local))))
      
      (test 404
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-dns))))
      
      
      ;; accept from localhost only
      (setf (location-authorizer-patterns loca) 
	'((:accept "127.0" 8)
	  :deny))
      (test 200
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-local))))
      
      (test 404
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-dns))))
      
      ;; accept from dns name only 
      
      (setf (location-authorizer-patterns loca) 
	`((:accept ,(long-site-name))
	  :deny))
      
      (test 404
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-local))))
      
      (test 200
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-dns))))
      
      
      ;; deny dns and accept all others
      (setf (location-authorizer-patterns loca) 
	`((:deny ,(long-site-name))
	  :accept))
      
      (test 200
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-local))))
      
      (test 404
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-dns))))
      
      
      ;; deny localhost and accept all others
      (setf (location-authorizer-patterns loca) 
	'((:deny "127.0" 8)
	  :accept))
      
      (test 404
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-local))))
      
      (test 200
	    (values2 (x-do-http-request (format nil "~a/secret-loc-auth"
						prefix-dns))))
      
    

      ;; function authorizer 
      (let ((funa (make-instance 'function-authorizer
		    :function #'(lambda (req ent auth)
				  (declare (ignore ent auth))
				  ;; authorized if the uri 
				  ;; has a 'foo' in it
				  (if* (search "foo" 
					       (net.uri:uri-path
						(request-uri req)))
				     then t
				     else :deny)))))
	(publish :path "/func-auth-foo"
		 :content-type "text/html"
		 :authorizer funa
		 :function #'(lambda (req ent)
			       (with-http-response (req ent)
				 (with-http-body (req ent)
				   (html "foo")))))
	(publish :path "/func-auth-foo"
		 :content-type "text/html"
		 :authorizer funa
		 :function #'(lambda (req ent)
			       (with-http-response (req ent)
				 (with-http-body (req ent)
				   (html "foo")))))
	
	(test 200 (values2 
		   (x-do-http-request (format nil "~a/func-auth-foo" 
					      prefix-local))))
	(test 404 (values2 
		   (x-do-http-request (format nil "~a/func-auth-bar" 
					      prefix-local))))
	
	))))

(defvar *as-test-lock* (mp:make-process-lock :name "aseflock"))

(defun as-find-external-format (&rest args)
  (mp:with-process-lock 
   ;; 2010-12 mm: Use a process-lock to avoid conditionalizing on smp...
   ;; We need a lock to avoid races in find-external-format.
   ;; If and when ef code is fixed, this lock could be removed [see rfe10327].
   (*as-test-lock*)
   (apply #'find-external-format args)))

(defun as-require (&rest args)
  (mp:with-process-lock
      ;; 2010-12 mm: Use a process-lock to avoid conditionalizing on smp...
      ;; We need a lock to avoid race on require.
      ;; If and when require code is fixed, this lock could be removed.
      (*as-test-lock*)
    (apply #'require args)))

(defun test-encoding ()
  ;; test the encoding and decoding
  (let ((str1 (make-string 256))
	(str2 (make-string 256)))
    (dotimes (i 256)
      (setf (schar str1 i) (code-char i))
      (setf (schar str2 i) (code-char (mod (+ i 10) 256))))
    
    (let ((query `(("foo bar" . "baz")
		   (,str1 . "a b c d")
		   ("efffg" . ,str2))))
      (test (form-urlencoded-to-query
	     (query-to-form-urlencoded query :external-format :latin1-base)
	     :external-format :latin1-base)
	    query
	    :test #'equal)))
  #+(and allegro ics (version>= 6 0))
  (let* ((str1 (coerce '(#\hiragana_letter_a #\hiragana_letter_i
			 #\hiragana_letter_u)
		       'string))
	 (str2 (coerce '(#\katakana_letter_a #\katakana_letter_i
			 #\katakana_letter_u)
		       'string))
	 (query `(("bazzer" . ,str1)
		  (,str2 . "berry"))))
    (dolist (ef (list (as-find-external-format :utf8)
		       (as-find-external-format :shiftjis)
		       ;; 6.0 beta didn't have an ef for unicode.
		       (if* (as-find-external-format :unicode :errorp nil)
			    thenret
			    else (as-find-external-format :utf8))
		       (as-find-external-format :euc)))
      (test (form-urlencoded-to-query
	     (query-to-form-urlencoded query :external-format ef)
	     :external-format ef)
	    query
	    :test #'equal)
      (test str1
	    (uridecode-string (uriencode-string str1 :external-format ef)
			      :external-format ef)
	    :test #'string=))))
    
    
(defun test-forms (port)
  ;; test encoding and decoding info
  ;;
  ;; we can send the info as a uri query or as the body of a post
  ;;
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(uri-var-vals '(("marketing" . "sammy c")
			("sales" . "masako o")
			("tech group" . "A Whole Big <Group> of Folks?")))
	(post-var-vals
	 '(("cessna" . "good#")
	   ("piper"  . "better###")
	   ("grumman" . "best<>###")))
	(req-query-res)
	)
    

    ;;-------------------------

    (publish :path "/form-tester-both"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 ;; get both uri and post
		 (if* (eql (request-method req) :post)
		    then (test "application/x-www-form-urlencoded"
			       (header-slot-value req :content-type)
			       :test #'equal))
		 (setq req-query-res (request-query req))
		 
		 ;; also test here the setf'ing of query values
		 (test nil (request-query-value "flurber" req))
                 (if* (eql (request-method req) :get)
                    then (test "sammy c" (request-query-value "marketing" req)
                                         :test #'equal))
		 (setf (request-query-value "flurber" req) "ziftp")
		 (test "ziftp" (request-query-value "flurber" req)
		       :test #'equal)
		 
		 
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "hi")))))

    
    ;; send query only on uri
    (x-do-http-request (format nil "~a/form-tester-both?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals)))
    
    (test nil (set-difference uri-var-vals req-query-res
			      :test #'equal))
    
    
    ; - use query arg, but place a part of the query directly into
    ; the uri to test merging (rfe12963)
    (destructuring-bind (var . val) (first uri-var-vals)
      (declare (ignore var val))
      (x-do-http-request (format nil "~a/form-tester-both?~a" prefix-local
                                 (net.aserve:query-to-form-urlencoded
                                  (subseq uri-var-vals 0 1)))
                         :query (rest uri-var-vals)))
			     
    (test nil (set-difference uri-var-vals req-query-res
			      :test #'equal))  
    
    
    

    ;; send query only on post
    (x-do-http-request (format nil "~a/form-tester-both" 
			     prefix-local)
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference post-var-vals req-query-res
			      :test #'equal))
    
    
    (x-do-http-request (format nil "~a/form-tester-both" 
			     prefix-local)
      :method :post
      :query post-var-vals)
    
    (test nil (set-difference post-var-vals req-query-res
			      :test #'equal))
    
    
    ;; send query on both uri and post
    (x-do-http-request (format nil "~a/form-tester-both?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals))
	
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference (append post-var-vals 
				      uri-var-vals)
			      req-query-res
			      :test #'equal))
    
    
    ;;------------------------------------
    
    ;; only check uri
    
    (publish :path "/form-tester-uri"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 ;; get both uri and post
		 (setq req-query-res (request-query req :post nil))
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "hi")))))
    
    
    (x-do-http-request (format nil "~a/form-tester-uri?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals)))
    
    (test nil (set-difference uri-var-vals req-query-res
			      :test #'equal))
    
    
    
    ;; send query only on post
    (x-do-http-request (format nil "~a/form-tester-uri" 
			     prefix-local)
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil req-query-res)
    
    
    ;; send query on both uri and post
    (x-do-http-request (format nil "~a/form-tester-uri?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals))
	
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference uri-var-vals
			      req-query-res
			      :test #'equal))
    
    ;;-------------------------
    
    ; only check post
    
    (publish :path "/form-tester-post"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 ;; get both uri and post
		 (setq req-query-res (request-query req :uri nil))
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "hi")))))
    

    (x-do-http-request (format nil "~a/form-tester-post?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals)))
    
    (test nil  req-query-res)
    
    
    
    ;; send query only on post
    (x-do-http-request (format nil "~a/form-tester-post" 
			     prefix-local)
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference req-query-res post-var-vals :test #'equal))
    
    
    ;; send query on both uri and post
    (x-do-http-request (format nil "~a/form-tester-post?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals))
	
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference post-var-vals req-query-res :test #'equal))

    ;
    ; test that we can do get-request-body more than once
    ;
    (publish :path "/get-request-body-tester"
	     :content-type "text/plain"
	     :function
	     #'(lambda (req ent)
		 
		 (with-http-response (req ent)
		   (test t 
			 (equal (get-request-body req)
				"foo and bar"))
		   (test t 
			 (equal (get-request-body req)
				"foo and bar"))
		   (with-http-body (req ent)))))
    (x-do-http-request (format nil "~a/get-request-body-tester" 
			     prefix-local)
      :method :post
      :content "foo and bar"
      :content-type "text/plain")
    
    ))

(defvar *get-request-body-incr-value*
    (let* ((size 16000)
	   (vec (make-array size :element-type '(unsigned-byte 8))))
      (dotimes (i size) (setf (aref vec i) (random 255)))
      vec))

(defun test-get-request-body-incr (port)
  (net.aserve:publish 
   :path "/get-request-body-incr-test"
   :content-type "text/plain"
   :function #'(lambda (req ent)
		 (let (bufs
		       got-zero
		       after-zero)
				    
		   (get-request-body-incremental
		    req #'(lambda (buffer size)
			    (if* got-zero
			       then ; never should get
                                    ; callback after zero
				    (setq after-zero t))
			    (if* (eql size 0)
			       then (setq got-zero t)
			       else (push (subseq buffer 0 size) bufs))))

                   ; test that the callback function got a zero size
		   (test t got-zero)
		 
                   ; test that the zero was the last value sent the
                   ; callback function
		   (test nil after-zero)
		 
                   ; test that the callback function received
                   ; the correct value
		   (let ((res (apply #'concatenate
				     'vector
				     (nreverse bufs))))
		     (test t
                           (equalp res *get-request-body-incr-value*))))
				  

                 ; response doesn't matter, we'e done the testing already
   
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (net.html.generator:html "foo the bar")
		     ))))
  
  (let ((prefix-local (format nil "http://localhost:~a" port)))
    (x-do-http-request (format nil "~a/get-request-body-incr-test" 
			       prefix-local)
		       :method :put
		       :content *get-request-body-incr-value*
		       :content-type "application/binary")
    )

  (let ((tfile (format nil "~a/test~A-computed-content" *aserve-test-dir* (asc index))))
    (with-open-file  (p tfile :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (write-sequence *get-request-body-incr-value* p))
    (let ((prefix-local (format nil "http://localhost:~a" port)))
      ;; We append ?use=<tag> here just to make it clear in the log
      ;; file which request failed, should we see a failure.
      ;; The query here will have no effect at the http server.
      (x-do-http-request (format nil "~a/get-request-body-incr-test?use=file-computed-content"
                                 prefix-local)
                         :method :put
                         :content (make-instance 'file-computed-content
                                    :filename tfile)
                         :content-type "application/binary")
      ;; Test pathname is automatically wrapped in file-computed-content.
      (x-do-http-request (format nil "~a/get-request-body-incr-test?use=pathname"
                                 prefix-local)
                         :method :put
                         :content (pathname tfile)
                         :content-type "application/binary")
      ;; Test chunked body request by using the stream-computed-content.
      (with-open-file (stream tfile :direction :input :element-type '(unsigned-byte 8))
        (x-do-http-request (format nil "~a/get-request-body-incr-test?use=stream-computed-content"
                                   prefix-local)
                           :method :put
                           :content (make-instance 'stream-computed-content :stream stream)
                           :content-type "application/binary"))
      ;; Test stream is automatically wrapped in stream-computed-content.
      (with-open-file (stream tfile :direction :input :element-type '(unsigned-byte 8))
        (x-do-http-request (format nil "~a/get-request-body-incr-test?use=stream"
                                   prefix-local)
                           :method :put
                           :content stream
                           :content-type "application/binary"))
      ;; Test connection is kept alive.
      (if* (and (not (asc x-proxy))
                (not (asc x-ssl)))
         then (multiple-value-bind (body code headers uri socket1)
                  (with-open-file (stream tfile :direction :input :element-type '(unsigned-byte 8))
                    (x-do-http-request (format nil "~a/get-request-body-incr-test?use=socket1"
                                               prefix-local)
                                       :method :put
                                       :content stream
                                       :content-type "application/binary"
                                       :keep-alive t))
                (declare (ignore body code headers uri))
                (test t (not (null socket1)) :fail-info "socket is nil")
                (multiple-value-bind (body code headers uri socket2)
                    (with-open-file (stream tfile :direction :input :element-type '(unsigned-byte 8))
                      (x-do-http-request (format nil "~a/get-request-body-incr-test?use=socket2"
                                                 prefix-local)
                                         :method :put
                                         :content stream
                                         :content-type "application/binary"
                                         :keep-alive t
                                         :connection socket1))
                  (declare (ignore body code headers uri))
                  (test socket1 socket2 :fail-info "socket is not reused"))))
      ;; Multiple values in 'Transfer-Encoding' header are legal
      ;; accoding to the MDN resource:
      ;;    https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Transfer-Encoding,
      ;; but aserve does not handle this, so error is expected.
      (with-open-file (stream tfile :direction :input :element-type '(unsigned-byte 8))
        (test-error
         (x-do-http-request (format nil "~a/get-request-body-incr-test?use=stream-error"
                                    prefix-local)
                            :method :put
                            :content stream
                            :content-type "application/binary"
                            :headers '(("Transfer-Encoding" . "gzip, chunked")))))
      ;; Test that error is thrown when attempting to use chunking with HTTP/1.0.
      (with-open-file (stream tfile :direction :input :element-type '(unsigned-byte 8))
        (test-error
         (x-do-http-request (format nil "~a/get-request-body-incr-test?use=stream-error"
                                    prefix-local)
                            :protocol :http/1.0
                            :method :put
                            :content stream
                            :content-type "application/binary")))
      (delete-file tfile)))
  )

(defun test-put-patch (port)
  (let ((prefix-local (format nil "http://localhost:~a" port))
        (content "blah blah blah"))
    (publish :path "/putpatch"
             :content-type "text/plain"
             :function #'(lambda (req ent)
                           (let ((want (intern (request-query-value "want" req)
                                               (find-package :keyword))))
                             (test want (request-method req)))
                           
                           (test content (get-request-body req) :test #'equal)
                           (with-http-response (req ent)
                             (with-http-body (req ent)
                               (net.html.generator:html "foo the bar")
                               ))))
    (dolist (method '(:put :patch))
      (x-do-http-request (format nil "~a/putpatch?want=~a" prefix-local method)
                         :method method
                         :content content
                         :content-type "text/plain"))))


(defun test-request-character-encoding (port)
  (let ((prefix-local (format nil "http://localhost:~a" port)))
    (publish :path "/charset"
	     :content-type "text/plain"
	     :function #'(lambda (req ent)
                           (let* ((check-ef (find-external-format (request-query-value "check" req)))
                                  (req-ef (net.aserve::request-character-encoding req :void)))
                             (test check-ef req-ef :fail-info (header-slot-value req :content-type))
                             (with-http-response (req ent)
                               (with-http-body (req ent)
                                 (html "foo"))))))
    (loop for (content-type check-charset) in
          '(("text/plain"                     :void)
            ("text/plain;charset=utf-8"       :utf-8)
            ("text/plain; charset=utf-8"      :utf-8)
            ("text/plain; charset=utf-8;"     :utf-8)
            ("text/plain; charset=utf-8; "    :utf-8)
            ("text/plain;charset=\"utf-8\""   :utf-8)
            ("text/plain; charset=\"utf-8\""  :utf-8)
            ("text/plain; charset=\"utf-8\";" :utf-8)
            ("text/plain; charset = utf-8;" :utf-8)
            ("text/plain; CHARSET = utf-8;" :utf-8)
            ("text/plain; CHARSET=UTF-8;"   :utf-8)
            ("text/plain; CHARSET = UTF-8;" :utf-8)
            ("text/plain;CHARSET    =     UTF-8   ;" :utf-8)
            ("text/plain; foo=bar;charset=\"utf-8\" ;x=y" :utf-8))
        do (multiple-value-bind (body code headers)
               (x-do-http-request (format nil "~a/charset?check=~A" prefix-local check-charset) 
                                  :headers (list (cons :content-type content-type)))
             (declare (ignore body headers))
             (test 200 code)))))
  
(defun test-client (port)
  ;; If the :ssl module has the :sni feature, this should succeed.
  ;; Otherwise it will signal an error.
  (let ((url "https://allegrograph.com/"))
    (if* (net.aserve.client::ssl-has-sni-p)
       then (test t (stringp (values (net.aserve.client:do-http-request url))))
       else (test-error (net.aserve.client:do-http-request url)
                        :condition-type 'error
                        :include-subtypes t)))
  
  (let ((prefix-local (format nil "http://localhost:~a" port)))
  
    ;; test redirection
    (publish :path "/redir-target"
	     :content-type "text/plain"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent)
			     (with-http-body (req ent)
			       (html "foo")))))
  
    (publish :path "/redir-to"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent
						    :response *response-found*)
			     (setf (reply-header-slot-value req :location) 
			       "redir-target")
			     (with-http-body (req ent)))))
    
    ; redirect to itself... danger danger!
    (publish :path "/redir-inf"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent
						    :response *response-found*)
			     (setf (reply-header-slot-value req :location) 
			       "redir-inf")
			     (with-http-body (req ent)))))
    
    ;; Test :no-keep-alive strategy added in rfe15225.
    (publish :path "/defaultkeepalive"
	     :function (lambda (req ent)
			 (with-http-response (req ent)
			   (with-http-body (req ent)))))
    (publish :path "/nokeepalive"
	     :function (lambda (req ent)
			 (with-http-response (req ent)
			   (push :no-keep-alive (request-reply-strategy req))
			   (with-http-body (req ent)))))
    
  
    ; first test target
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/redir-target" prefix-local))
      (declare (ignore body headers))
      (test 200 code))
  
    ; now test through redirect
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/redir-to" prefix-local))
      (declare (ignore body headers))
      (test 200 (and :second code)))
  
    ; now turn off redirect and test
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/redir-to" prefix-local) :redirect nil)
      (declare (ignore body headers))
      (test 302 (and :third code)))

    ; turn off with a zero repeat count
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/redir-to" prefix-local) :redirect 0)
      (declare (ignore body headers))
      (test 302 (and :fourth code)))

    
    ; self redirect, we test that we eventually give up
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/redir-inf" prefix-local))
      (declare (ignore body headers))
      (test 302 (and :fifth code)))
    
    
    
    ;; test that keepalive works
    (multiple-value-bind (body code1 headers uri socket)
	(x-do-http-request (format nil "~a/redir-target" prefix-local))
      (declare (ignore body headers uri))
      (test 200 code1)
      (test nil (and "no-keepalive" socket)))
    
    (multiple-value-bind (body code2 headers uri socket)
	(x-do-http-request (format nil "~a/redir-target" prefix-local)
			   :keep-alive t)
      (declare (ignore body headers uri))
      (test 200 code2)
      
      (if* (not (asc x-proxy))
         then (test t (not (null socket)) :fail-info "socket not kept alive"))
      
      (if* socket
         then ;; now reuse it
              ;; bug20222: give a chance for the server to close the
              ;; connection
              (sleep (random 1.0))
              (multiple-value-bind (body code3 headers uri socket2)
                  (x-do-http-request (format nil "~a/redir-target" prefix-local)
                                     :keep-alive t
                                     :connection socket)
                (declare (ignore body headers uri))
                (test 200 code3)
                (if* (and (not (asc x-proxy))
                          (not (asc x-ssl)))
                     then               ; reuse should happen
                     (test socket (and "reuse socket" socket2))
                     (asc-format "~%~%pause ~d seconds ....~%" 
                                 (+ net.aserve::*read-request-timeout* 10))
                     (force-output)
                     (sleep (+ net.aserve::*read-request-timeout* 10))
                                        ; now the server should have
                                        ; shut down the socket so
                                        ; reuse will not happen
                     (multiple-value-bind (body code4 headers uri socket3)
                         (x-do-http-request (format nil "~a/redir-target"
                                                    prefix-local)
                                            :connection socket2)
                       (declare (ignore body headers uri))
                       (test 200 code4)
                       (test t (and "not reuse socket"
                                    (not (eq socket2 socket3)))))))))
    
    (when (null (asc x-proxy))
      ;; Do not run this test through a proxy.
      ;; Test :no-keep-alive strategy added in rfe15225.
      ;;  When :no-keep-alive strategy is present, avoid sending either of
      ;;  the heders "Connection: close" or "Connection: Keep-Alive". 
      (multiple-value-bind (body code3 headers)
	  (x-do-http-request (format nil "~a/defaultkeepalive" prefix-local)
			     :keep-alive nil)
	(declare (ignore body))
	(test 200 code3 :fail-info "defaultkeepalive keep-alive=nil")
	(test "close" (cdr (assoc :connection headers)) :test #'string-equal
	      :fail-info "defaultkeepalive keep-alive=nil"))
      (multiple-value-bind (body code3 headers)
	  (x-do-http-request (format nil "~a/defaultkeepalive" prefix-local)
			     :keep-alive t)
	(declare (ignore body))
	(test 200 code3 :fail-info "defaultkeepalive keep-alive=t")
	(test "keep-alive" (cdr (assoc :connection headers)) :test #'string-equal
	      :fail-info "defaultkeepalive keep-alive=t"))
    
      (multiple-value-bind (body code3 headers)
	  (x-do-http-request (format nil "~a/nokeepalive" prefix-local))
	(declare (ignore body))
	(test 200 code3 :fail-info "nokeepalive keep-alive missing")
	(test nil (assoc :connection headers) :fail-info "nokeepalive keep-alive missing"))
      (multiple-value-bind (body code3 headers)
	  (x-do-http-request (format nil "~a/nokeepalive" prefix-local)
			     :keep-alive nil)
	(declare (ignore body))
	(test 200 code3 :fail-info "nokeepalive keep-alive=nil")
	(test nil (assoc :connection headers) :fail-info "nokeepalive keep-alive=nil"))
      (multiple-value-bind (body code3 headers)
	  (x-do-http-request (format nil "~a/nokeepalive" prefix-local)
			     :keep-alive t)
	(declare (ignore body))
	(test 200 code3 :fail-info "nokeepalive keep-alive=t")
	(test nil (assoc :connection headers) :fail-info "nokeepalive keep-alive=t"))
      )
    
    ))
  
  


;; proxy cache tests
;; (net.aserve.test::test-proxy-cache)
;;
(defun test-proxy-cache ()
  (let* ((*wserver* (start :port nil :server :new))
	 (proxy-wserver (start :port nil :server :new :proxy t :cache t))
	 (proxy-host)
	 (origin-server)
	 (pcache (net.aserve::wserver-pcache proxy-wserver))
	 (*print-level* 4) ; in case we see some errors
	 )
    (log-wserver-name "test-proxy-cache main")
    (log-wserver-name "test-proxy-cache proxy" proxy-wserver)
    
    (macrolet ((test-2 (res1 res2 form &key (test #'eql))
		 `(multiple-value-bind (v1 v2) ,form
		    (test ,res1 (and '(:first ,form) v1) :test ,test)
		    (test ,res2 (and '(:second ,form) v2) :test ,test))))
		 
		      
		 
    
      (setq proxy-host (format nil "localhost:~d"
			       (socket:local-port
				(net.aserve::wserver-socket proxy-wserver))))
    
      (setq origin-server
	(format nil "http://localhost:~d" (socket:local-port
					   (net.aserve::wserver-socket *wserver*))))

      (asc-format "server on port ~d, proxy server on port ~d"
	      (socket:local-port
	       (net.aserve::wserver-socket *wserver*))
	      (socket:local-port
	       (net.aserve::wserver-socket proxy-wserver)))

      (let ((tfile (format nil "aserve~Atest.xx" (asc index))))
	(with-open-file (p tfile :direction :output
			   :if-exists :supersede)
			(format p "foo"))
      
	(with-tests 
	 (:name "aserve-proxy-cache")
	 (unwind-protect
	     (progn
	       (publish-file  :path "/foo" :file tfile :cache-p t)

					; a miss
	       (test-2 "foo" 200
		       (do-http-request 
			(format nil "~a/foo" origin-server)
			:proxy proxy-host)
		       :test #'equal)
	      
	       (test 1 (net.aserve::pcache-r-miss pcache))
	      
					; a fast hit
	       (test-2 "foo" 200
		       (do-http-request 
			(format nil "~a/foo" origin-server)
			:proxy proxy-host)
		       :test #'equal)
	       (test 1 (net.aserve::pcache-r-fast-hit pcache))
	      
					; another fast hit
	       (test-2 "foo" 200
		       (do-http-request 
			(format nil "~a/foo" origin-server)
			:proxy proxy-host)
		       :test #'equal)
	       (test 2 (net.aserve::pcache-r-fast-hit pcache))
	  

	       (asc-format "sleeping for 10 secs.....")(force-output)
	       (sleep 10)
	      
					; entry no longer fresh so get a slow hit
	       (test-2 "foo" 200
		       (do-http-request 
			(format nil "~a/foo" origin-server)
			:proxy proxy-host)
		       :test #'equal)
	       (test 1 (net.aserve::pcache-r-slow-hit pcache))

					; entry now updated so we get a fast hit 
	       (test-2 "foo"  200
		       (do-http-request 
			(format nil "~a/foo" origin-server)
			:proxy proxy-host)
		       :test #'equal)
	      
	       (test 3 (net.aserve::pcache-r-fast-hit pcache))
	      
					; try flushing all to disk
	       (net.aserve::flush-memory-cache pcache 0)
	      
					; and retrieve from the disk
	       (test-2 "foo"  200
		       (do-http-request 
			(format nil "~a/foo" origin-server)
			:proxy proxy-host)
		       :test #'equal)
	       (test 4 (net.aserve::pcache-r-fast-hit pcache))
		
	       )
	    
	  
      

	   (ignore-errors (delete-file tfile))
	   (shutdown  :server proxy-wserver)
	   (shutdown  :server *wserver*)))))))

    
    
; publish-directory tests

(defun test-publish-directory (port)
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name)
			      port))
	(test-dir)
	(step 0)
	(got-reps nil))
    
    (multiple-value-bind (ok whole dir)
	(match-regexp "\\(.*[/\\]\\).*" (namestring *aserve-load-truename*))
      (declare (ignore whole))
      (if* (not ok) 
	 then (error "can't find the server.pem directory"))
      
      (setq test-dir dir))
      
	
    (publish-directory :prefix "/test-pd/"
		       :destination test-dir
		       :hook #'(lambda (req ent extra)
				       (declare (ignore req ent extra))
				       (setq got-reps (or got-reps 0))
				       (incf got-reps))
		       :headers '(("testvdir" . "testvval"))
		       :filter #'(lambda (req ent filename info)
				   (declare (ignore ent info))
				   (test t
					 (values 
					  (match-regexp "server.pem"
							filename))
					 :test #'equal)
				   (case step
				     (0 (failed-request req)
					t)
				     (1 nil))))
      
    ; in step 0 we have the filter return a 404 code
    (test 404 (values2 
	       (x-do-http-request (format nil "~a/test-pd/server.pem" 
					  prefix-local))))

    (test nil got-reps) ; hook didn't fire
    
    ; in step 1 we have it return the actual file
    (setq step 1)
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/test-pd/server.pem"
				   prefix-local))
      (declare (ignore body))
      (test 200 code)
      (test "testvval"
		    (cdr (assoc :testvdir headers :test #'equal))
		    :test #'equal))
    
    (test 1 got-reps)   ; hook fired
    
    ; remove entry so subsequent tests won't see it
    (publish-file :path "/test-pd/server.pem" :remove t)
      
    ; remove directory publish and see if that worked
    (publish-directory :prefix "/test-pd/" :remove t)
      
    ; now it shouldn't exist
    (test 404 (values2 
	       (x-do-http-request (format nil "~a/test-pd/server.pem" 
					  prefix-local))))
      
    ; test publish directory with virtual hosts
    (publish-directory :prefix "/test-foo/"
		       :destination test-dir
		       :host "localhost")
    ; so it will work with localhost
    (test 200 (values2
	       (x-do-http-request (format nil "~a/test-foo/server.pem"
					  prefix-local))))
      
    ; but not the dns name
    (test 404 (values2
	       (x-do-http-request (format nil "~a/test-foo/server.pem"
					  prefix-dns))))
    ; remove all refs
    (publish-directory :prefix "/test-foo/"
		       :host "localhost"
		       :remove t)
    (publish-file :path "/test-foo/server.pem" 
		  :host "localhost"
		  :remove t)
      
    ; now doesn't exist
    (test 404 (values2
	       (x-do-http-request (format nil "~a/test-foo/server.pem"
					  prefix-local))))

    ;; now try using the access control
    (publish-directory :prefix "/acc-test/"
		       :destination (concatenate 'string test-dir "testdir/")
		       :access-file "access.cl")
    
    
    ; forbidden to access this file
    (test 404
	  (values2 (x-do-http-request (format nil "~a/acc-test/access.cl"
					      prefix-local
					      ))))
    
    ; and this file
    (test 404
	  (values2 (x-do-http-request (format nil "~a/acc-test/bbb.ign"
					      prefix-local))))
    
    ; and any CVS file in this dir and those below
    (test 404
	  (values2 (x-do-http-request (format nil "~a/acc-test/CVS/Root"
					      prefix-local))))
    
    (test 404
	  (values2 (x-do-http-request (format nil "~a/acc-test/subc/ccc.html"
					      prefix-local))))
    
    ; subdir subd can't be accessed from this or any subdir
    ; due to :inherit in the access file
    (test 404
	  (values2 (x-do-http-request (format nil "~a/acc-test/subd/ddee.html"
					      prefix-local))))
    (test 404
	  (values2 (x-do-http-request (format nil "~a/acc-test/suba/subd/ddd.html"
					      prefix-local))))
    
    ; but this one is ok, and has content type specified by access file
    (multiple-value-bind (res code headers)
	(x-do-http-request (format nil "~a/acc-test/aaa.foo"
				   prefix-local))
      (declare (ignore res))
      (test 200 code)
      (test "foo/bar" (cdr (assoc :content-type headers :test #'eq)) 
	    :test #'equal))
    
    ; test getting mime type from the standard place since it isn't
    ; specified
    (multiple-value-bind (res code headers)
	(x-do-http-request (format nil "~a/acc-test/ccc.html"
				   prefix-local))
      (declare (ignore res))
      (test 200 code)
      (test "text/html" (cdr (assoc :content-type headers :test #'eq)) 
	    :test #'equal))
    
    ; now try full name mime type
    (multiple-value-bind (res code headers)
	(x-do-http-request (format nil "~a/acc-test/readme"
				   prefix-local))
      (declare (ignore res))
      (test 200 code)
      (test "frob/frib" (cdr (assoc :content-type headers :test #'eq)) 
	    :test #'equal))
    
    ; test blocking via ip address, can't access if not using localhost
    (test 404 (values2 (x-do-http-request (format nil "~a/acc-test/ccc.html"
						  prefix-dns))))
    
    
    ; now down a directory the ip restriction isn't inherited
    (test 200 (values2 (x-do-http-request 
			(format nil "~a/acc-test/suba/foo.html" prefix-dns))))
    (test 200 (values2 (x-do-http-request 
			(format nil "~a/acc-test/suba/foo.html" prefix-local))))
    ; this is blocked since we only match files named 'foo'
    (test 404 (values2 (x-do-http-request 
			(format nil "~a/acc-test/suba/access.cl" prefix-local))))
    
    ; and we can't go down another directory level since that's blocked
    (test 404 (values2 (x-do-http-request 
			(format nil "~a/acc-test/suba/subsuba/foo.html" prefix-local))))
    
    ;; now try password and ip authorized
    ; no password
    (test 401 (values2 (x-do-http-request 
			(format nil "~a/acc-test/subb/foo.html"
				prefix-local))))
    
    ; wrong ip but password ok
    (test 404 (values2 (x-do-http-request 
			(format nil "~a/acc-test/subb/foo.html"
				prefix-dns)
			:basic-authorization '("joe"  . "eoj")
			)))
    
    ; good password and ip
    (test 200 (values2 (x-do-http-request 
			(format nil "~a/acc-test/subb/foo.html"
				prefix-local)
			:basic-authorization '("joe"  . "eoj")
			)))
    
    ; try multilple directories
    (publish-directory :prefix "/multiple-test/"
		       :destination 
		       (list (concatenate 'string test-dir "testdir/")
			     (concatenate 'string test-dir "testdir2/")))
    
    ; from the first dir
    (test 200 (values2 (x-do-http-request 
			(format nil "~a/multiple-test/ccc.html"
				prefix-local))))
    ; from the second dir
    (test 200 (values2 (x-do-http-request 
			(format nil "~a/multiple-test/second.txt"
				prefix-local))))

    ;;
    ;; test redirects to the index file
    ;;
    (publish-directory :prefix "/redirnormal/"
		       :destination 
		       (concatenate 'string test-dir "testdir3/"))
    
    ; do a normal redirect to the index
    
    (multiple-value-bind (body retcode headers uri)
	(x-do-http-request 
	 (format nil "~a/redirnormal/"
		 prefix-local))
      (declare (ignore headers))
      (test (file-contents 
	     (concatenate 'string test-dir "testdir3/index.html")
	     :external-format :octets)
	    body
	    :test #'equal
	    )
      (test 200 retcode)
      (test "/redirnormal/index.html"
	    (net.uri:uri-path uri)
	    :test #'equal
	    ))
    
    ; do one directory level down redirect
    (multiple-value-bind (body retcode headers uri)
	(x-do-http-request 
	 (format nil "~a/redirnormal/subdir"
		 prefix-local))
      (declare (ignore headers))
      (test (file-contents 
	     (concatenate 'string test-dir "testdir3/subdir/index.html")
	     :external-format :octets)
	    body
	    :test #'equal
	    )
      (test 200 retcode)
      (test "/redirnormal/subdir/index.html"
	    (net.uri:uri-path uri)
	    :test #'equal
	    ))
    
    ; check that the first step of a uri not ending in /
    ; is to add the /
    
    (multiple-value-bind (body retcode headers uri)
	(x-do-http-request 
	 (format nil "~a/redirnormal/subdir"
		 prefix-local)
	 :redirect nil  ; don't auto redirect
	 )
      
      (declare (ignore body uri))
      (test 301 retcode)
      (test t (values (match-re "/redirnormal/subdir/$"
				(cdr (assoc :location headers))))))
    
    
    ; test where an internal redirect is done
    (publish-directory :prefix "/redirhidden/"
		       :destination 
		       (concatenate 'string test-dir "testdir3/")
		       
		       :hidden-index-redirect t
		       )
    
    ; simple
    (multiple-value-bind (body retcode headers uri)
	(x-do-http-request 
	 (format nil "~a/redirhidden/"
		 prefix-local))
      (declare (ignore headers))
      (test (file-contents 
	     (concatenate 'string test-dir "testdir3/index.html")
	     :external-format :octets
	     )
	    body
	    :test #'equal
	    )
      (test 200 retcode)
      (test "/redirhidden/"
	    (net.uri:uri-path uri)
	    :test #'equal
	    ))
    
    ; one level down
    (multiple-value-bind (body retcode headers uri)
	(x-do-http-request 
	 (format nil "~a/redirhidden/subdir"
		 prefix-local))
      (declare (ignore headers))
      (test (file-contents 
	     (concatenate 'string test-dir "testdir3/subdir/index.html")
	     :external-format :octets
	     )
	    body
	    :test #'equal
	    )
      (test 200 retcode)
      (test "/redirhidden/subdir/"
	    (net.uri:uri-path uri)
	    :test #'equal
	    ))
    
    ; one step one level done
    (multiple-value-bind (body retcode headers uri)
	(x-do-http-request 
	 (format nil "~a/redirhidden/subdir"
		 prefix-local)
	 :redirect nil  ; don't auto redirect
	 )
      
      (declare (ignore body uri))
      (test 301 retcode)
      (test t (values (match-re "/redirhidden/subdir/$"
				(cdr (assoc :location headers))))))
    
    
    ))


;; publish-multi tests
(defun test-publish-multi (port)
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(multifile (format nil "aserve~Amulti.xx" (asc index)))
	)
    (with-open-file (p multifile
		     :direction :output
		     :if-exists :supersede)
      (write-sequence "bar" p))
    (publish-multi :path "/multi-test"
		   :items (list '(:string "foo")
				multifile  ; file
				#'(lambda (req ent time value)
				    (declare (ignore req ent time value))
				    "baz")
				#'(lambda (req ent time value)
				    (declare (ignore req ent time value))
				    (string-to-octets "bof" 
						      :null-terminate nil))))
    
    
    (test "foobarbazbof" 
	  (values (x-do-http-request  (format nil "~a/multi-test" prefix-local)))
	  :test #'equal)
    
    (ignore-errors (delete-file multifile))
    ))
		   


;; publish-prefix tests
;;
(defun test-publish-prefix (port)
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name)
			      port))
	(got-here))
    (publish-prefix :prefix "/pptest"
		    :function
		    #'(lambda (req ent)
			(incf got-here)
			(with-http-response (req ent)
			  (with-http-body (req ent)
			    (html "foo"))))
		    :headers '((:testhead . "testval"))
		    )
    (dolist (prefix (list prefix-local prefix-dns))
      (setq got-here 0)
      (test 200 (values2
		 (x-do-http-request (format nil "~a/pptest"
					    prefix))))
      (test 1 got-here)
      (test 200 (values2
		 (x-do-http-request (format nil "~a/pptest/fred"
					    prefix))))
      (test 2 got-here)
      (multiple-value-bind (body code headers)
	  (x-do-http-request (format nil "~a/pptest#asdfasdf"
				     prefix))
	(declare (ignore body))
	(test 200 code)
	(test "testval"
	      (cdr (assoc :testhead headers :test #'equal))
	      :test #'equal))
      
      (test 3 got-here)
      (test 200 (values2
		 (x-do-http-request (format nil "~a/pptestasdfasdf#asdfasdf"
					    prefix))))
      
      (test 4 got-here)
      (test 404 (values2
		 (x-do-http-request (format nil "~a/pptes"
					    prefix))))
      (test 4 got-here))))
    
    
    
			


		   
    

		       
(defun test-cgi (port)
  ;; currently we only have a test program on unix since
  ;; that where our shell script works
  ;;
  (declare (ignorable port))
  #+(version>= 6 1)
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(error-buffer))
    (publish :path "/cgi-0"
	     :function #'(lambda (req ent)
			   (net.aserve:run-cgi-program 
			    req ent
			    #.(format nil "sh ~acgitest.sh"
				      *aserve-examples-directory*))))
    (publish :path "/cgi-1"
	     :function #'(lambda (req ent)
			   (net.aserve:run-cgi-program 
			    req ent
			    #.(format nil "sh ~acgitest.sh 1"
				      *aserve-examples-directory*))))
    (publish :path "/cgi-2"
	     :function #'(lambda (req ent)
			   (net.aserve:run-cgi-program 
			    req ent
			    #.(format nil "sh ~acgitest.sh 2"
				      *aserve-examples-directory*))))
    (publish :path "/cgi-3"
	     :function #'(lambda (req ent)
			   (net.aserve:run-cgi-program 
			    req ent
			    #.(format nil "sh ~acgitest.sh 3"
				      *aserve-examples-directory*))))
    
    ;; verify that the various headers work
    (test 200 (values2 
	       (x-do-http-request (format nil "~a/cgi-0"
					  prefix-local))))
    
    (test 200 (values2 
	       (x-do-http-request (format nil "~a/cgi-1"
					  prefix-local))))
    
    ; verify that a redirect is requested
    (multiple-value-bind (body code headers)
	(x-do-http-request (format nil "~a/cgi-2"
				   prefix-local)
			   :redirect nil)
      
      ; some /bin/sh's don't' support "-n" so accept either one.
      ; We don't want to search for something exact since if -n
      ; fails we get an appened lf, or cllf and we don't want to
      ; conditionalize on the machine's line ending convention.
      (test t (not (null (search "go to franz" body))))
      
      (test 301 code)
      (test "https://franz.com" (cdr (assoc :location headers))
	    :test #'equal)
      (test "123hellomac" (cdr (assoc :etag headers))
	    :test #'equal)
      )

    ; verify that the unauthorized response is made
    (test 401 (values2 
	       (x-do-http-request (format nil "~a/cgi-3"
					  prefix-local))))

    ; test error output processing
    (publish :path "/cgi-4"
	     :function #'(lambda (req ent)
			   (net.aserve:run-cgi-program 
			    req ent
			    #.(format nil "sh ~acgitest.sh 4"
				      *aserve-examples-directory*)
			    :error-output
			    #'(lambda (req ent stream)
				(declare (ignore req ent))
				(let (eof)
				  (loop
				    (let ((ch (read-char-no-hang stream 
								 nil :eof)))
	     
				      (if* (null ch) then (return))
	     
				      (if* (eq :eof ch) 
					 then (setq eof t)
					      (return))
	     
				      (vector-push-extend ch error-buffer)))
				  eof
				  )))))
    (setq error-buffer (make-array 10 
				   :element-type 'character
				   :adjustable t
				   :fill-pointer 0))
        
    (multiple-value-bind (body rescode)
	(x-do-http-request (format nil "~a/cgi-4" prefix-local))
      (test "okay" body :test #'equal-line)
      (test 200 rescode)
      (test "stuff-on-error-stream" error-buffer :test #'equal-line))
    ))
   
(defun equal-line (a b &aux (la (when (stringp a) (length a))) (lb (when (stringp b) (length b))))
  ;; 2010-12 mm: Add this hack to avoid unexplained inequality in Unix.
  (and (< 0 la) (eql #\newline (elt a (1- la))) (decf la))
  (and (< 0 lb) (eql #\newline (elt b (1- lb))) (decf lb))
  (and (eql la lb)
       (dotimes (i la t) (or (eql (elt a i) (elt b i)) (return nil)))))
	
(defun test-timeouts (port)
  ;; test aserve timing out when the client is non responsive
  (let (#+ignore (prefix-local (format nil "http://localhost:~a" port)))
    
    (if* (asc x-ssl) 
       then ; we don't get the same timeout behavior since we're
	    ; not directly connected to the server socket, so
	    ; don't try the tests
	    (return-from test-timeouts nil))
    
    (asc-format "timeout tests.. expect pauses")(force-output)
    
    ;; try making a connection and not sending any headers.
    ;; we should timeout
    (let ((sock (socket:make-socket :remote-host "localhost"
				    :remote-port port)))
      (unwind-protect
       (progn
       (format sock "GET /timeouttest HTTP/1.0~c~cfoo: bar~c~c"
	       #\return #\newline #\return #\newline)
       (force-output sock)
       
       ; try sending data periodically but in enough time to
       ; bypass the timeout.  This only works in the io-timeout
       ; situation.
       #+io-timeout
       (dotimes (i 3)
	 (sleep (max 1 (- net.aserve:*http-io-timeout* 10)))
	 (asc-format "send packet")(force-output)
	 (format sock "brap: brop~c~c" #\return #\newline)
	 (force-output sock)
	 )
       
       ; now sleep for longer than it should take for the timeout to occur
       (sleep (+ 3 (max *http-response-timeout* *http-io-timeout*)))
       (test-error
	;; now we should get a connection reset by peer
	(progn (format sock "brap: brop~c~c" #\return #\newline)
	       (force-output sock)
	       (format sock "brap: brop~c~c" #\return #\newline)
	       (force-output sock))
	:condition-type 'errno-stream-error
	))
       (ignore-errors (close sock :abort t))))))
       

(defun test-international (port)
  (declare (ignorable port))
  #+(and allegro ics (version>= 6 1))
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(Privyet! (coerce '(#\cyrillic_capital_letter_pe
			    #\cyrillic_small_letter_er
			    #\cyrillic_small_letter_i
			    #\cyrillic_small_letter_ve
			    #\cyrillic_small_letter_ie
			    #\cyrillic_small_letter_te
			    #\!)
			  'string)))
    (as-find-external-format :koi8-r) ;;; 2010-12 mm: make sure the ef arrives safely
    (publish 
     :path "/simple-form-itest"
     :function
     #'(lambda (req ent)
	 ; simulate starting aserve with :external-format :koi8-r arg
	 (let ((*default-aserve-external-format* :koi8-r))
	   (with-http-response (req ent)
	     (with-http-body (req ent :external-format :koi8-r)
	       (let ((text (request-query-value "text" req)))
		 (if* text
		    then (html
			  (:html
			   (:head (:title "result"))
			   (test Privyet! text :test #'string=)
			   (:body "test text: {" (:princ text) "}")))
		    else ;; filler -- test normally doesn't go here
			 (html
			  (:html
			   (:head (:title "foobar"))
			   (:body))))))))))
  
    (let* ((result
	    (x-do-http-request 
	     (format nil "~a/simple-form-itest?text=%F0%D2%C9%D7%C5%D4%21"
		     prefix-local)
	     :external-format :octets))
	   (begin (position #\{ result))
	   (end (position #\} result))
	   (test-string
	    (if* begin
	       then (octets-to-string
		     (string-to-octets (subseq result (1+ begin) end)
				       :external-format :octets)
		     :external-format :koi8-r))))
      (test t (not (null begin)))  ; verify we found begin 
      (test t (not (null end)))    ; and end markers
      (test Privyet! test-string :test #'string=))))



(defun test-spr27296 ()
  #+(and allegro ics)
  (let ((server (start :port nil :server :new
		       :external-format (crlf-base-ef :utf8)))
	(string (concatenate 'string
		  "<Name>B"
		  '(#\latin_small_letter_o_with_diaeresis
		    #\r
		    #\latin_small_letter_o_with_diaeresis)
		  "cz P"
		  '(#\latin_small_letter_e_with_acute)
		  "ter</Name>")))
    (log-wserver-name "test-spr27296" server)
    (publish :path "/spr27296"
	     :content-type "text/xml"
	     :server server
	     :function #'(lambda (req ent)
			   (test string
				 (get-request-body
				  req
				  :external-format (crlf-base-ef :utf8))
				 :test #'string=)
			   (with-http-response (req ent)
			     (with-http-body (req ent)))))
    (do-http-request (format nil "http://localhost:~d/spr27296"
			     (socket:local-port
			      (net.aserve::wserver-socket server)))
      :method :post
      :content string
      :external-format (crlf-base-ef :utf8))
    (shutdown :server server)))

(defun test-client-unicode-content-length ()
  ;; Older versions treated content-length as a character count rather
  ;; than byte count, which went wrong with multi-byte encodings.
  (let ((server (start :port nil :server :new
		       :external-format (crlf-base-ef :utf8))))
    (log-wserver-name "test-client-unicode-content-length" server)
    (publish-file :server server :path "/" :file (format nil "~a../test/testdir/unicode"
                                                         *aserve-examples-directory*))
    ;; Not timing out is the test.
    (do-http-request (format nil "http://localhost:~a/"
                             (socket:local-port
                              (wserver-socket server)))
      :external-format (crlf-base-ef :utf8) :keep-alive nil :timeout 3)
    (shutdown :server server)))

(defun test-http-copy-file (port)
  (let* ((reference-files '("sys:files.bu"
			    "sys:runtime.bu"
			    "sys:mlisp.dxl"
			    "sys:mlisp8.dxl"
			    "sys:alisp.dxl"
			    "sys:alisp8.dxl"
			    "sys:dcl.dxl"))
	 (url (format nil "http~a://localhost:~a/http-copy-file"
		      (if (asc x-ssl) "s" "") port))
	 (temp-file-name (sys:make-temp-file-name "temp")))

    (dolist (reference-file reference-files)
      (when (probe-file reference-file)
	(asc-format "~&~%======= test-http-copy-file: ~a~%" reference-file)
	(publish-file :path "/http-copy-file" :file reference-file
		      :content-type "application/octet-stream")
	(unwind-protect
	    (labels
		((progress (bytes-read total-size)
		   (asc-format "  copy progress: ~a ~a" bytes-read total-size)
		   (force-output t))
		 (doit (&rest args)
		   (asc-format "~&~%copying reference file~@[:~{ ~a~}~]~%"
			   args)
		   (let ((before (get-internal-real-time)))
		     (apply #'http-copy-file url temp-file-name args)
		     (asc-format "time = ~s msecs"
			     (- (get-internal-real-time) before)))
		   (asc-format "comparing ~a" temp-file-name)
		   (test t (excl::compare-files reference-file temp-file-name))
		   (delete-file temp-file-name)
		   (asc-format "compare finished")
		   ))
	      (doit :progress-function #'progress)
	      ;;*** no need to do this so many times:
	      ;;(doit)
	      ;;(doit :buffer-size 2048)
	      ;;(doit :buffer-size 4096)
	      (doit :buffer-size 8192)
	      (doit :protocol :http/1.0))
	  (ignore-errors (delete-file temp-file-name)))))))


(defun test-proxy-auth ()
  ;; we only test proxy authorization once, not once
  ;; for each kind of test we do 
  
  (let ((test-url
	 "http://franz.com/ftp/pri/layer/file-used-by-ws-test-suite.txt"))
    ;; allow all
    (start-proxy-running t)
    (test 200 (values2 (x-do-http-request test-url)))
    (stop-proxy-running)
  
    ;; allow from localhost
    (start-proxy-running (make-instance 'proxy-control
			   :location (make-instance 'location-authorizer
				       :patterns '((:accept "127.1")
						   :deny))))
    (test 200 (and :second (values2 (x-do-http-request test-url))))
    (stop-proxy-running)
  
  
    ;; disallow from localhost
    (start-proxy-running (make-instance 'proxy-control
			   :location (make-instance 'location-authorizer
				       :patterns '((:accept "192.168.111.222")
						   (:deny "127.1")
						   :deny))))
    (test 404 (and :second (values2 (x-do-http-request test-url))))
    (stop-proxy-running)
  
    ;; allow to franz.com
    (start-proxy-running
     (make-instance 'proxy-control
       :destinations '("franz.com"
		       ("franz.com" 333 80) ; 
		       ("www.cnn.com")	; no ports specified
		       )))
    (test 200 (and :third (values2 (x-do-http-request test-url))))
    (test 404 (and :fouth (values2 (x-do-http-request "http://www.noaa.gov"))))
    (test 404 (values2 (x-do-http-request "http://www.noaa.gov:444")))
    (test 200 (values2 (x-do-http-request test-url)))
    (test 404 (values2 (x-do-http-request "http://www.cnn.com")))
  
    (stop-proxy-running)
  
    (format t "proxy authorization using hash table for destination~%")
    ;; allow using a hash table
    (let ((ht (make-hash-table :test #'equalp)))
      (dolist (site '("franz.com" 
		      ("franz.com" 333 80)
		      ("www.cnn.com")))
		    
	(setf (gethash (if* (consp site) then (car site) else site) ht) 
	  (if* (consp site) 
	     then (cdr site) 
	     else t)))
    
      (start-proxy-running (make-instance 'proxy-control
			     :destinations ht))
    
      (test 200 (and :third (values2 (x-do-http-request test-url))))
      (test 404 (and :fouth (values2 (x-do-http-request "http://www.noaa.gov"))))
      (test 404 (values2 (x-do-http-request "http://www.noaa.gov:444")))
      (test 200 (values2 (x-do-http-request test-url)))
      (test 404 (values2 (x-do-http-request "http://www.cnn.com")))
    
      (stop-proxy-running))))


(defun test-expect-header-responses ()
  (let* ((server (start :port nil :server :new
			:external-format (crlf-base-ef :utf8)))
	 (base (format nil "http://localhost:~a" 
		       (socket:local-port (net.aserve::wserver-socket *wserver*)))))
    (unwind-protect
        ;; [bug23200] Avoid server leak in each test.
	(let ()
	  (log-wserver-name "test-expect-header-responses" server)
          ;; verify that a 100 Continue is sent only if valid user:pass is provided
	  (publish :path "/secret-auth"
		   :content-type "text/html"
		   :authorizer (make-instance 'password-authorizer
                                 :allowed '(("foo2" . "bar2"))
                                 :realm  "SecretAuth")
		   :function
		   #'(lambda (req ent)
		       (with-http-response (req ent)
                         (with-http-body (req ent)
                           (html (:head (:title "Secret page"))
                                 (:body "You made it to the secret page"))))))
          ;; verify 100 Continue is returned depending on request method.
	  (publish :path "/noauth"
		   :content-type "text/html"
		   :function
		   #'(lambda (req ent)
		       (with-http-response (req ent)
                         (with-http-body (req ent)
                           (html (:head (:title "Secret page"))
                                 (:body "You made it to the secret page"))))))

          ;; post methods manually send a 100-continue
          ;; put methods fetch from the body to trigger the auto 100-continue
          ;;     response if there's a 100-continue expect header.
	  (publish :path "/no-auto"
		   :content-type "text/html"
		   :will-handle-expect-continue t
		   :function
		   #'(lambda (req ent)
		       (with-http-response (req ent)
                         (with-http-body (req ent)
                           (case (request-method req)
                             (:post (when (request-has-continue-expectation req)
                                      (send-100-continue req))
                                    (html "post success"))
                             (:put ;; test that we will auto-send continue response.
                              ;; force a read of the request body
                              (request-query-value "body" req)
                              (html "put success"))
                             (t (html "non put/post success")))))))

          ;; The last two number are the no-proxy return code and
          ;; the proxy return code.  They differ when the
          ;; Expect: 100-continue
          ;; header is sent because when sent to a proxy it has no 
          ;; choice but to return  "100 continue".  If sent to the actual
          ;; server it can test if the desired resource even exists and 
          ;; then return the appropriate code and then send the "100 continue"
          ;; only if it does.

	  (let ((tests '(("/does-not-exist" :post nil t 404 100)
			 ("/secret-auth" :post nil t 401 100)
			 ("/secret-auth" :post ("foo2" . "wrongpass") t 401 100)
			 ("/secret-auth" :post ("foo2" . "bar2") t 100 100)
			 ("/secret-auth" :get ("foo2" . "bar2") t 200 200)
			 ("/noauth" :get nil t 200 200)
			 ("/noauth" :put nil t 100 100)
			 ("/no-auto" :get nil t 200 200)
			 ("/no-auto" :put nil t 100 100)
			 ("/no-auto" :put nil nil 200 200)
			 ("/no-auto" :post nil t 100 100)
			 ("/no-auto" :post nil nil 200 200)
			 )))
	    (loop for (path method auth expect-hdr expected-code
                            expected-proxy) in tests
                do (let* ((body-p (member method '(:put :post)))
                          (body (when body-p (query-to-form-urlencoded '(("body" . "stuff")))))
                          (creq (x-make-http-client-request
                                 (format nil "~a~a" base path)
                                 :method method
                                 ;;:ssl (asc x-ssl)
                                 :basic-authorization auth
                                 :content (when body-p body)
                                 :content-type (when body-p "application/x-www-form-urlencoded")
                                 :headers (when expect-hdr '(("Expect" . "100-continue"))))))
                     (unwind-protect 
                         (progn (read-client-response-headers creq)
                                (if*  (asc x-proxy)
                                   then (test expected-proxy (client-request-response-code creq))
                                   else (test expected-code (client-request-response-code creq)))
                                )
                       (client-request-close creq))))))
      (when server (shutdown :server server)))))
  


(defun test-retry-on-timeout (port)
  (let ((test-url (format nil "http://localhost:~a/3-times" port))
	(reset-url (format nil "http://localhost:~a/3-times/reset" port))
	(counter 0))
    ;; returns 2 408 responses, then returns a 200.
    (publish :path "/3-times"
	     :content-type "text/plain"
	     :function
	     #'(lambda (req ent)
		 (incf counter)
		 (if* (< counter 3)
		    then (with-http-response (req ent :response *response-request-timeout*)
			   (with-http-body (req ent)))
		    else (setf counter 0)
			 (with-http-response (req ent)
			   (with-http-body (req ent)
			     (html "Success"))))))
  
    (publish :path "/3-times/reset"
	     :content-type "text/plain"
	     :function
	     #'(lambda (req ent)
		 (setf counter 0)
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "OK")))))
    
    (flet ((test-code (expected-code &rest request-args)
	     (print (list :testing expected-code request-args))
	     (multiple-value-bind (resp code)
		 (apply #'x-do-http-request request-args)
	       (declare (ignore resp))
	       (test expected-code code))))

      ;; default retries in nil
      (test-code 408 test-url)
      (x-do-http-request reset-url)
      (test-code 408 test-url :retry-on-timeout 1)
      (x-do-http-request reset-url)
      (test-code 200 test-url :retry-on-timeout 3))))
  
(defun test-chunked-request (port https)
  ;; A bug in chunked SSL streams caused chunked request bodies
  ;; to not be read correctly by the server.
  (publish :path "/echo" :content-type "text/plain" 
           :function 
           #'(lambda (req ent)
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (write-sequence (get-request-body req) *html-stream*)
                   ;; Only available after get-request-body...
                   (let ((header (header-slot-value req :x-trailer-test)))
                     (test "trailer test" header :test #'string=))))))
  (let* ((url (format nil "http~@[s~*~]://localhost:~a/echo" https port))
         (req (x-make-http-client-request 
               url 
               :method :post
               :content-type "text/plain"
               :accept "text/plain"
               :content-length "i have no idea, sorry!"
               :content (chunk-encode-string 
                         "TEST" 
                         '(("x-trailer-test" . "trailer test")))
               :headers '((:transfer-encoding . "chunked"))
               :timeout 10)))
    (read-client-response-headers req)
    (test 200 (client-request-response-code req))
    (let ((body (read-response-body req)))
      (test "TEST" body :test #'string=))
    (net.aserve.client:client-request-close req)))

(defun test-chunked-request-set-trailers (port https &key verbose)
  (if* (asc x-proxy)
     then ;; Proxy uses HTTP/1.0 whereas trailers are HTTP/1.1
          (when verbose (format t "~&Skipping test: test-chunked-request-set-trailers because: x-proxy~%"))
     else (let ((test-body "AAAA")
                (test-trailer-value "trailer test"))
            (publish :path "/trailer-test" :content-type "text/plain"
                     :function
                     #'(lambda (req ent)
                         (with-http-response (req ent :trailers '((:x-trailer-test . "initial")))
                           (with-http-body (req ent)
                             (set-trailers req `((:x-trailer-test . ,test-trailer-value)))
                             (write-sequence test-body *html-stream*)))))
            (multiple-value-bind (body code headers)
                (x-do-http-request (format nil "http~@[s~*~]://localhost:~a/trailer-test" https port)
                                   :content-type "text/plain"
                                   :accept "text/plain"
                                   :headers '((:te . "trailers"))
                                   :timeout 10)
              (test 200 code)
              (test body test-body :test 'string=)
              (test test-trailer-value (cdr (assoc :x-trailer-test headers)) :test 'equal)))))

(defun test-chunked-request-set-trailers-while-debugging (port https)
  ;; :xmit-server-response-body will wrap the chunking-stream in a broadcast-stream
  ;; and set-trailers must work for that case too.
  (let ((net.aserve::*debug-current* '(:xmit-server-response-body)))
    (test-chunked-request-set-trailers port https)))

(defun test-server-request-body (port &key https verbose)
  ;; Test with-body-input-stream.  rfe14295
  (or port (setq port (start-aserve-running)))
  (flet ((stream-sender
	  (url 
	   &aux
	   (req (make-http-client-request
		 url :method :post :content-length (+ (* 30 50) 10)))
	   (sock (client-request-socket req))
	   b)
	  (dotimes (i 30)
	    (when verbose (format t "~&stream-sender: send random chars.~%"))
	    (dotimes (i 50) (write-char (code-char (+ 32 (random 28))) sock))
	    (force-output sock)
	    (sleep 0.5))
	  (dotimes (i 10) (write-char #\= sock))
	  (force-output sock)
	  (when verbose (format t "~&stream-sender: sent ==========.~%"))
	  (read-client-response-headers req)
	  (setq b (read-response-body req))
	  (test 200 (client-request-response-code req))
	  (when verbose (print (list :REPLY b)))
	  (client-request-close req)
	  b)
	 (stream-reader
	  (req ent &aux b)
	  (with-http-response
	   (req ent)
	   (with-http-body 
	    (req ent)
	    (when verbose (format t "~&stream-reader: started body.~%"))
	    (setq b (get-request-body req))
	    (when verbose (format t "~&stream-reader: received body.~%"))
	    (if (search "=========" b) (setq b "OK") (setq b (length b)))
	    (html (:html (:body (:princ b)))))))
	 (streaming-reader
	  (req ent &aux b clen)
	  (with-http-response
	   (req ent)
	   (with-http-body 
	    (req ent)
	    (when verbose (format t "~&stream-reader: started body.~%"))
	    (setq clen (header-slot-value req :content-length))
	    (when verbose (format t "~&stream-reader: Content-length is ~S.~%" clen))
	    (setq clen (ignore-errors (parse-integer (header-slot-value req :content-length))))
	    (when verbose (format t "~&stream-reader: expecting ~A bytes.~%" clen))

	    (net.aserve::with-body-input-stream
	     (s req)
	     (dotimes (i (* 2 (or clen 10000)) (setq b "OK"))
	       (when (eql 99 (mod i 100))
		 (when verbose (format t "~&stream-reader: after ~A bytes.~%" i)))
	       (or (setq b (read-byte s nil nil)) (return (setq b (list "after" i)))))
	     )

	    (when verbose (format t "~&stream-reader: received body ~S.~%" b))
	    (html (:html (:body (:princ b)))))))
	 (chunked-sender
	  (url 
	   &aux
	   (data (with-output-to-string
		   (s)
		   (dotimes (i 30)
		     (dotimes (i 50) (write-char (code-char (+ 32 (random 28))) s)))
		   (dotimes (i 10) (write-char #\= s))))
	   (req (make-http-client-request
		 url :method :post
		 :headers '((:transfer-encoding . "chunked"))
		 :content-length "i have no idea, sorry!"
		 :content (chunk-encode-string 
			   data
			   '(("x-trailer-test" . "trailer test")))
		 ))
	   b)
	  (when verbose (format t "~&stream-sender: sent ==========.~%"))
	  (read-client-response-headers req)
	  (setq b (read-response-body req))
	  (test 200 (client-request-response-code req))
	  (when verbose (print (list :REPLY b)))
	  (client-request-close req)
	  b)
	 )
    (if https (setq https "s") (setq https ""))
    (let (b)
      (format t "~&Test with-body-input-stream ...~%")

      (when verbose (format t "~&Basic stream reader test.~%"))
      (publish :path "/stream1" :function #'stream-reader)
      (sleep 1)
      (setq b (stream-sender (format nil "http~A://localhost:~A/stream1" https port)))
      (test nil (null (search "OK" b)))

      (when verbose (format t "~&Simple streaming reader test.~%"))
      (publish :path "/stream2" :function #'streaming-reader)
      (sleep 1)
      (setq b (stream-sender (format nil "http~A://localhost:~A/stream2" https port)))
      (test nil (null (search "after 1510" b)))

      (when verbose (format t "~&Chunked streaming reader test.~%"))
      (publish :path "/stream3" :function #'streaming-reader)
      (sleep 1)
      (setq b (chunked-sender (format nil "http~A://localhost:~A/stream3" https port)))
      (test nil (null (search "after 1510" b)))
      
      (format t "~&Done with test with-body-input-stream~%"))))

(defun test-request-uri (port https)
  (publish :path "/request-uri" :content-type "text/plain"
           :function
           #'(lambda (req ent)
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (net.uri:render-uri (request-uri req) *html-stream*)))))
  (let* ((uri (format nil 
                      "http~@[s~*~]://localhost:~a/request-uri" https port))
         (result (x-do-http-request uri)))
    (test uri result :test 'string=)))

(defun test-aserve-extra-ssl ()
  ;; tests run with no aserve server running
  (let ((seen-error-1 nil) ; normal ssl start
	(seen-error-2 nil) ; bogus pem file but don't test
	(seen-error-3 nil) ; bogus pem file and do test
	
	)
    (let (*wserver*)
      
      ; valid ssl startup
      (handler-case
	  (setq *wserver*   ; fix for bug24838
	    (net.aserve:start :ssl (merge-pathnames "server.pem" 
						    *aserve-load-truename*)
			      :server :new
			      :port nil
			      :test-ssl t))
	(error (c)
	  c
	  (setq seen-error-1 t)))
      
      (test nil seen-error-1)
      (and *wserver* (shutdown))
      
      (setq *wserver* nil)
      
      ;; bogus ssl cert but don't test
      (handler-case 
	  (setq *wserver*
	    (net.aserve:start :ssl (merge-pathnames "not-exist-server.pem" 
						    *aserve-load-truename*)
			      :port nil
			      :server :new
			      ))
	(error (c)
	  c
	  (setq seen-error-2 t)))
      
      (test nil seen-error-2)
      (and *wserver* (shutdown))
      
      (setq *wserver* nil)
      
      ;; bogus ssl cert and do test
      (handler-case 
	  (setq *wserver*
	    (net.aserve:start :ssl (merge-pathnames "not-exist-server.pem" 
						    *aserve-load-truename*)
			      :port nil
			      :server :new
			      :test-ssl t))
	(error (c)
	  c
	  (setq seen-error-3 t)))
      
      (test t seen-error-3)
      (and *wserver* (shutdown)))))

(defun test-aserve-ssl-redirect (&aux server1 server2 port1 port2 context test-dir text)
  (unwind-protect
      (let ()
	(setq context (socket:make-ssl-server-context 
		       :certificate (merge-pathnames "server.pem" *aserve-load-truename*)))
	(setq server1 (start :port nil :server :new :ssl-args (list :context context)))
	(setq port1 (socket::local-port (net.aserve::wserver-socket server1)))
	(setq server2 (start :port nil :server :new :ssl-args (list :context context)))
	(setq port2 (socket::local-port (net.aserve::wserver-socket server2)))
	(test nil (eql port1 port2) :fail-info (format nil "test-aserve-ssl-redirect ports ~A ~A" port1 port2))
	
	(multiple-value-bind (ok whole dir)
	    (match-regexp "\\(.*[/\\]\\).*" (namestring *aserve-load-truename*))
	  (declare (ignore whole))
	  (if* (not ok) 
	     then (error "can't find the server.pem directory"))
      
	  (setq test-dir dir))
	(publish-directory :server server1 :prefix "/"
			   :destination (concatenate 'string test-dir "testdir3/"))
	(test-no-error (setq text (do-http-request (format nil "https://localhost:~A/" port1))))
	(test 12 (search "topfile</body>" text))
	(shutdown :server server1)
	(setq server1 nil)
	(sleep 1)

	(publish-directory :server server2 :prefix "/"
			   :destination (concatenate 'string test-dir "testdir3/"))
	(test-no-error (setq text (do-http-request (format nil "https://localhost:~A/" port2))))
	(test 12 (search "topfile</body>" text))

	)
    (when server1 (shutdown :server server1))
    (when server2 (shutdown :server server2))
    ))


(defun test-force-output-prepend-stream ()
  ;; the prepend-stream is defined in aserve
  ;; we test here that force-output of such 
  ;; a stream will force-output the underlying
  ;; stream
  (let* ((passive (socket:make-socket :connect :passive))
         (sender (socket:make-socket :remote-host "127.0.0.1"
                                     :remote-port
                                     (socket:local-port passive)))
         (receiver (socket:accept-connection passive))
         (prepend (net.aserve::make-instance-prepend-stream+content+output-handle
                   "" ; no content
                   sender)))
    (unwind-protect 
        (progn
          (write-byte 1 prepend)
          (force-output prepend)
    
          (mp::with-timeout (10 (test nil "force-output of prepend-stream didn't work"))
      
            ;; read-byte will return immediately if the force-output
            ;; was successful
            (test 1 (read-byte receiver))))
      
      (close passive)
      (close sender)
      (close receiver))))


;; rfe15456
(defun test-body-in-get-request (&key port ssl debug (n 3)
				 &aux 
				 (port-num (or port (start-aserve-running ssl)))
				 (url (format nil "http~A://localhost:~A" 
					      (if ssl "s" "") port-num))
				 (test-body "the body")
				 rbody rc hdrs auri sock sock2)
			
  (declare (ignore hdrs auri))
  (publish-file :path "/junktest1" :file (format nil "~a/testdir/aaa.foo"
                                                 *aserve-test-dir*))
  (publish-file :path "/junktest2" :file (format nil "~a/testdir/readme"
                                                 *aserve-test-dir*))
  (publish :path "/junktest3"
	   :function (lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html (:princ (get-request-body req)))))))
  
  (asc-format "Begin test-body-in-get-request...")
  (when debug (net.aserve::debug-on debug))
  
  ;; Verify that junk between requests causes a server error response.

  (dotimes (i n (asc-format "   ~A test-body-in-get-request did not get second use of socket ..."
			    (if (asc x-proxy) "Via proxy" "Direct")))
    (multiple-value-setq (rbody rc hdrs auri sock)
      (x-do-http-request (format nil "~A/junktest1?a~A" url i)
			 :method :get :keep-alive t))
    (when (and sock (eql rc 200) (eql 0 (search "this file" rbody)))
      (write-sequence 
       (concatenate 'vector (mapcar #'char-int (concatenate 'list "aaaaa")))
       sock)
      (multiple-value-setq (rbody rc  hdrs auri sock2)
	(x-do-http-request (format nil "~A/junktest2?b" url)
			   :method :get :connection sock :keep-alive t))
      (when (eql rc 400)
	(test 400 rc :fail-info "test-body-in-get-request 3")
	(when sock2 (ignore-errors (close sock2)))
	(return)))
    (when sock (ignore-errors (close sock)))
    (when sock2 (ignore-errors (close sock2)))
    (sleep 2)
    )
  
  ;; Verify that body of get request is not treated as junk.
  (multiple-value-setq (rbody rc hdrs auri sock)
    (x-do-http-request (format nil "~A/junktest1?k" url)
		       :method :get :keep-alive t :content "body-of-request"))
  (test 200 rc :fail-info "test-body-in-get-request 4")
  (test 0 (search "this file" rbody) :fail-info "test-body-in-get-request 5")
  (multiple-value-setq (rbody)
    (x-do-http-request (format nil "~A/junktest2?m" url)
		       :method :get :connection sock))
  (test 200 rc :fail-info "test-body-in-get-request 6")
  (test 0 (search "sorry," rbody) :fail-info "test-body-in-get-request 7")
  
  ;; Verify that body went through.
  (multiple-value-setq (rbody rc hdrs auri sock)
    (x-do-http-request (format nil "~A/junktest3" url)
		       :method :get :content test-body))
  (test 200 rc :fail-info "test-body-in-get-request 8")
  (test test-body rbody :test #'equal :fail-info "test-body-in-get-request 9")
  
  (when debug (net.aserve::debug-off debug))
  (asc-format "Done with test-body-in-get-request...")
  
  nil)

(defun test-truncated-stream ()
  (let ((temp-file-name (sys:make-temp-file-name "temp")))
    (unwind-protect
         (progn
           (with-open-file (f temp-file-name
                              :direction :output
                              :if-exists :supersede)
             (write-sequence (make-string 2000 :initial-element #\a) f))
           (with-open-file (f temp-file-name)
             (let ((truncated-stream (net.aserve::make-instance-truncated-stream+byte-length+input-handle 50 f))
                   (buffer (make-array 256 :element-type '(unsigned-byte 8))))
               (test 4 (device-read truncated-stream buffer 0 4 nil)))))
      (delete-file temp-file-name))))
    
(defun test-spr44282 ()
  (test '(("bar" . " ") ("foo" . " ")) 
        (net.aserve::form-urlencoded-to-query "bar=+&foo=+")
        :test #'equal)
  (test '(("b ar" . " +") ("foo" . " +")) 
        (net.aserve::form-urlencoded-to-query "b+ar=+%2b&foo=+%2b")
        :test #'equal))



;;;; caching test

(defparameter *test-auto-cache-seconds* 10)
;; Delay long enough past lifetime to detect cache entry expiration reliably.
(defparameter *test-auto-cache-delay* 3)

(defmacro with-new-cache ((var &key (max-cache-size 1000000)
                                     auto-cache-codes 
                                     auto-cache-seconds) &body body)
  `(let ((,var (make-instance 'net.aserve.client:client-cache
                 :max-cache-size ,max-cache-size
                 :auto-cache-codes ,auto-cache-codes
                 :auto-cache-seconds ,auto-cache-seconds)))
     ,@body))

(defun test-caching (&key (cache-seconds *test-auto-cache-seconds*)
			  (cache-delay (+ cache-seconds *test-auto-cache-delay*)))
  (let ((port (start-aserve-running)))
    (unwind-protect
        (progn
          ;; test something that has no cache-control specifier
          ;; but is cached anyway
          (with-new-cache (cache)
            ;; all counters are zero
            
            (dopublish "/simple" :last-modified (get-universal-time)) 
            (dohttp "/simple" :cache cache :port port :expect 200 :name "aaa") 
            
            (check-cache cache "aa"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
                         
            
            ;; call again this time will validate cache result
            (dohttp "/simple" :cache cache :port port :expect 200 :name "bbb")
            (check-cache cache "bb"
                         :lookups 2
                         :alive   0
                         :revalidate 1
                         :validated 1))
          
          ;; test cache-control: max-age=N
          ;;
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/maxage" :last-modified (get-universal-time)
                       :headers `(("cache-control" . ,(format nil "private, max-age=~A" cache-seconds))))
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "ccc")
            (check-cache cache "cc"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; call again within cache-seconds seconds so it will be valid in the cache
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "ddd")
            (check-cache cache "dd"
                         :lookups 2
                         :alive   1
                         :revalidate 0
                         :validated 0)
            
            ;; now wait cache-delay seconds to ensure that it has expired in
            ;; the cache
            (sleep cache-delay)
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "eee")
            (check-cache cache "ee"
                         :lookups 3
                         :alive   1
                         :revalidate 1
                         :validated 1)
            
          
            )
          
          ;; test using an expires header
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/expire" :last-modified (get-universal-time)
                       :headers `(("expires" . 
                                             ,(net.aserve::universal-time-to-date 
                                               (+ (get-universal-time) cache-seconds)))))
            (dohttp "/expire" :cache cache :port port :expect 200 :name "fff")
            (check-cache cache "cc"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; call again within cache-seconds seconds so it will be valid in the cache
            (dohttp "/expire" :cache cache :port port :expect 200 :name "ggg")
            (check-cache cache "dd"
                         :lookups 2
                         :alive   1
                         :revalidate 0
                         :validated 0)
            
            ;; now wait cache-delay seconds to ensure that it has expired in
            ;; the cache
            (sleep cache-delay)
            (dohttp "/expire" :cache cache :port port :expect 200 :name "hhh")
            (check-cache cache "ee"
                         :lookups 3
                         :alive   1
                         :revalidate 1
                         :validated 1)
            
          
            )
          
          
          ;; we set max-age to cache-seconds and expires to 30 seconds in the future
          ;; we check that max-age takes precedence in ths case
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/expmax" :last-modified (get-universal-time)
                       :headers `(("cache-control" . ,(format nil "private, max-age=~A" cache-seconds))
                                  ("expires" . 
                                             ,(net.aserve::universal-time-to-date 
                                               (+ (get-universal-time) 30)))))
            (dohttp "/expmax" :cache cache :port port :expect 200 :name "ppp")
            (check-cache cache "ff"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; call again within cache-seconds seconds so it will be valid in the cache
            (dohttp "/expmax" :cache cache :port port :expect 200 :name "qqq")
            (check-cache cache "gg"
                         :lookups 2
                         :alive   1
                         :revalidate 0
                         :validated 0)
            
            ;; now wait cache-delay seconds to ensure that it has expired in
            ;; the cache
            (sleep cache-delay)
            (dohttp "/expmax" :cache cache :port port :expect 200 :name "rrr")
            (check-cache cache "hh"
                         :lookups 3
                         :alive   1
                         :revalidate 1
                         :validated 1)
            
          
            )

          
          ;; test that using a different accept header will not return a cached
          ;; entry with a different accept header
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/maxage" :last-modified (get-universal-time)
                       :headers `(("cache-control" . ,(format nil "private, max-age=~A" cache-seconds))))
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "ccc2"
                    :accept "one/two")
            (check-cache cache "cc2"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; call again within cache-seconds seconds so it will be valid in the cache
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "ddd2"
                    :accept "one/two")
            (check-cache cache "dd2"
                         :lookups 2
                         :alive   1
                         :revalidate 0
                         :validated 0)
            
            ;; call with a different accept header
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "ddd2"
                    :accept "three/four")
            (check-cache cache "dd3"
                         :lookups 3
                         :alive   1
                         :revalidate 0
                         :validated 0)
            
            
            ;; now wait cache-delay seconds to ensure that it has expired in
            ;; the cache
            (sleep cache-delay)
            (dohttp "/maxage" :cache cache :port port :expect 200 :name "eee"
                    :accept "one/two")
            (check-cache cache "ee2"
                         :lookups 4
                         :alive   1
                         :revalidate 1
                         :validated 1)
            
            )
          ;; cache-control: no-store
          ;;  does not let the response be stored in the cache
            
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/nostore" :last-modified (get-universal-time)
                       :headers `(("cache-control" . "no-store")
                                  ("expires" . 
                                             ,(net.aserve::universal-time-to-date 
                                               (+ (get-universal-time) cache-seconds)))))
            (dohttp "/nostore" :cache cache :port port :expect 200 :name "abbb")
            (check-cache cache "ii"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; call again quickly and note that the response wasn't cached
            (dohttp "/nostore" :cache cache :port port :expect 200 :name "bbbb")
            (check-cache cache "jj"
                         :lookups 2
                         :alive 0  
                         :revalidate 0
                         :validated 0)
            
            ;; now wait cache-delay seconds to ensure and try the call again.
            ;; verify not cached
            (sleep cache-delay)
            (dohttp "/nostore" :cache cache :port port :expect 200 :name "cbbb")
            (check-cache cache "kk"
                         :lookups 3
                         :alive   0
                         :revalidate 0
                         :validated 0)
            
          
            )
          
          
          ;; cache-control: no-cache allows you to cache
          ;; an item but you must validate on each access
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/nocache" :last-modified (get-universal-time)
                       :headers `(("expires" . 
                                             ,(net.aserve::universal-time-to-date 
                                               (+ (get-universal-time) cache-seconds)))
                                  ("cache-control" . "no-cache")))
            (dohttp "/nocache" :cache cache :port port :expect 200 :name "aa1")
            (check-cache cache "a1"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; call again within cache-seconds seconds so it will be valid in the cache
            ;; but we must revalidate anyway becuase of 'no-cache'
            (dohttp "/nocache" :cache cache :port port :expect 200 :name "aa2")
            (check-cache cache "a2"
                         :lookups 2
                         :alive   0
                         :revalidate 1
                         :validated 1)
            
            ;; now wait cache-delay seconds to ensure that it has expired in
            ;; the cache
            (sleep cache-delay)
            (dohttp "/nocache" :cache cache :port port :expect 200 :name "aa3")
            (check-cache cache "a3"
                         :lookups 3
                         :alive   0
                         :revalidate 2
                         :validated 2)
            
          
            )

          ;; test no-cache=headerfield  which allows certain
          ;; headers to not be stored in the cache.
          ;;
          (with-new-cache (cache)
            
            ;; set the max age so the page will be valid for cache-seconds seconds
            (dopublish "/nohead" :last-modified (get-universal-time)
                       :headers `(("cache-control"
				   . ,(format nil "private, max-age=~A, no-cache=fancy, no-cache=schmansy" 
					      cache-seconds))
                                  ("fancy" . "foo")
                                  ("schmansy" . "bar")
                                  ("flippy"  . "flop")))
            
            
            ;; when we do the http request it won't be in the
            ;; the cache so we will return all headers, when
            ;; we call it later and get the value from the
            ;; cache the fancy and schmansy headers will not be
            ;; present because we said they should not be cached.
            (multiple-value-bind (body code headers) 
                (dohttp "/nohead" :cache cache :port port 
                        :expect 200 
                        :name "ppp")
              (declare (ignore body code))
              (check-cache cache "ff"
                           :lookups 1
                           :alive   0
                           :revalidate 0
                           :validated 0)
              (test '(:fancy . "foo") (assoc :fancy headers)
                    :test #'equal
                    :fail-info "111")
              (test '(:schmansy . "bar") (assoc :schmansy headers)
                    :test #'equal
                    :fail-info "111")
              (test '(:flippy  . "flop") (assoc :flippy headers)
                    :test #'equal
                    :fail-info "111"))
            
            ;; call again within cache-seconds seconds so it will be valid in the cache
            (multiple-value-bind (body code headers)
                (dohttp "/nohead" :cache cache :port port :expect 200 :name "qqq")
              (declare (ignore body code))
              (check-cache cache "gg"
                           :lookups 2
                           :alive   1
                           :revalidate 0
                           :validated 0)
           
              (test 'nil (assoc :fancy headers)
                    :test #'equal
                    :fail-info "222")
              (test 'nil (assoc :schmansy headers)
                    :test #'equal
                    :fail-info "222")
              (test '(:flippy  . "flop") 
                    (assoc :flippy headers)
                    :test #'equal
                    :fail-info "222"))
            
            
            ;; now wait cache-delay seconds to ensure that it has expired in
            ;; the cache.  it will be validated and a new body
            ;; not returned so the old header with removed header
            ;; lines will still be in effect
            (sleep cache-delay)
            (multiple-value-bind (body code headers)
                (dohttp "/nohead" :cache cache :port port :expect 200 :name "rrr")
              (declare (ignore body code))
              (check-cache cache "hh"
                           :lookups 3
                           :alive   1
                           :revalidate 1
                           :validated 1)
          
              (test 'nil (assoc :fancy headers)
                    :test #'equal
                    :fail-info "333")
              (test 'nil 
                    (assoc :schmansy headers)
                    :test #'equal
                    :fail-info "333")
              (test '(:flippy  . "flop") 
                    (assoc :flippy headers :test #'equal)
                    :test #'equal
                    :fail-info "333")))

                    

          ;; test auto caching
          (with-new-cache (cache :auto-cache-codes '(200) :auto-cache-seconds cache-seconds)
            (dopublish "/autocache" :last-modified (- (get-universal-time) 60))
            (dohttp "/autocache" :cache cache :port port :expect 200)
            (check-cache cache "vv1"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            ;; still in cache
            (dohttp "/autocache" :cache cache :port port :expect 200)
            (check-cache cache "vv2"
                         :lookups 2
                         :alive   1
                         :revalidate 0
                         :validated 0)
            ;; will expire
            (sleep cache-delay)
            (dohttp "/autocache" :cache cache :port port :expect 200)
            (check-cache cache "vv3"
                         :lookups 3
                         :alive   1
                         :revalidate 1
                         :validated 1))
            
            
      
          ;; test cache flushing
          
          ;; first test that the cache size accounting is correct
          (with-new-cache (cache)
                  
            (dopublish-prefix "/bigret" 
                              :headers `(("cache-control" . ,(format nil "private, max-age=~A" cache-seconds))))
            ;; cache 5 objects of size 10000, 10001 .. 10004
                  
            (dotimes (i 5)
              (dohttp (format nil "/bigret~d" (+ i 10000))
                      :cache cache
                      :port port
                      :expect 200
                      :name "www"))
            (test (+ 10000 10001 10002 10003 10004)
                  (net.aserve.client::client-cache-cache-size cache)))
          
          ;; test that the cache flushing works
          (with-new-cache (cache :max-cache-size 500000)
            (dotimes (i 5)
              (dohttp (format nil "/bigret~d" (+ i 100000))
                      :cache cache
                      :port port
                      :expect 200
                      :name "xxx")
              ;; sleep one second to cause the last-used time in the cache
              ;; to vary by one second for each new item cached
              (sleep 1)
              )
            
            ;; will fit in cache size (500000) plus slop (100000)
            (test (+ 100000 100001 100002 100003 100004)
                  (net.aserve.client::client-cache-cache-size cache))
              
            ;; one more and that's too much, will have to flush a few.
            (dohttp (format nil "/bigret~d" 200000)
                    :cache cache
                    :port port
                    :expect 200
                    :name "zzz")
            
            ;; cache grew to 
            ;; (+ 100000 100001 100002 100003 100004 200000) == 700010
            ;; causing us to ask for it to be reduced below
            ;;  (- 500000 (max-cache-size) 100000 (slop)) = 400000
            ;; we cached items starting at 100000 so the will be
            ;; removed that order.
            ;;  we need to drop (- 700010 400000) = 300010 bytes
            ;; so we'll need to remove
            ;;   (+ 100000 100001 100002 100003)  == 400006 bytes
            ;; leaving (- 700010 400006) == 300004 bytes in the cache
            (test 300004 (net.aserve.client::client-cache-cache-size cache))
            nil
            )

          
          ;; test the flush-client-cache function
          (with-new-cache (cache)
            (dopublish "/checkflush" :last-modified (get-universal-time)
                       :headers `(("cache-control" . ,(format nil "max-age=~A" cache-seconds))))
            (dopublish "/checkflush2" :last-modified (get-universal-time)
                       :headers `(("cache-control" . ,(format nil "max-age=~A" cache-seconds))))
            ;; baseline
            (test 0 (net.aserve.client::client-cache-cache-size cache) 
                  :fail-info "dd1")
             
            (dohttp "/checkflush" :cache cache :port port :expect 200 
                    :name "cc1")
       
            ;; one thing cached of 20 bytes
             
            (test 20  (net.aserve.client:client-cache-cache-size cache) 
                  :fail-info "dd2")
             
            ;; it won't expire for cache-seconds seconds so this will not remove it
            (net.aserve.client:flush-client-cache cache :expired t)
             
            (test 20  (net.aserve.client:client-cache-cache-size cache)
                  :fail-info "dd3")
             
            ;; let it expire
            (sleep cache-delay)
             
            ;; cache a new url
            (dohttp "/checkflush2" :cache cache :port port :expect 200 
                    :name "cc1")
             

            ;; now we have two things cached == 40 bytes
            (test 40  (net.aserve.client:client-cache-cache-size cache) 
                  :fail-info "dd4")
             
            ;; get rid of expired ones
            (net.aserve.client:flush-client-cache cache :expired t)
             
            ;; and we're done to one
            (test 20  (net.aserve.client:client-cache-cache-size cache)
                  :fail-info "dd5")
             
            ;; get rid of everyone
            (net.aserve.client:flush-client-cache cache :all t)
             
            ;; and we're down to zero
            (test 0  (net.aserve.client:client-cache-cache-size cache) 
                  :fail-info "dd6"))
             

          ;; test caching of redirect responses without a cache-control
          ;; header.
          ;; We demonstrate using auto-caching how we can cause 
          ;; a redirect to be cached
          ;; for a certain period of time even if the returned
          ;; header doesn't specify a caching time.  This mirrors
          ;; a real world example we need to support.
          
          (with-new-cache (cache :auto-cache-codes 
                                 '(#.(net.aserve::response-number *response-moved-permanently*))
                                 
                                 :auto-cache-seconds cache-seconds)
            (dopublish "/redirit" :response *response-moved-permanently*
                       :headers '(("location" . "/redirit-target")))
            (dopublish "/redirit-target" :response *response-ok*
                       :headers `(("cache-control" . ,(format nil "public, max-age=~A" cache-seconds))))
            
            (dohttp "/redirit" :cache cache :port port :expect 200
                    :name "cp1")
            
            ;; we did two lookups (/redirit and /redirit-target) and
            ;; none were in the cache so zero alive
            (check-cache cache "cp2"
                         :lookups 2
                         :alive   0
                         :revalidate 0
                         :validated 0)
            
            (dohttp "/redirit" :cache cache :port port :expect 200
                    :name "cp3")
            
            ;; we did two more lookups (/redirit and /redirit-target)
            ;; and both were alive in the cache so no network
            ;; activity occurred.
            (check-cache cache "cp4"
                         :lookups 4
                         :alive   2
                         :revalidate 0
                         :validated 0)
            )
          
          ;; test that the accept header is sent with the revalidation
          (with-new-cache (cache :auto-cache-seconds cache-seconds
                                 :auto-cache-codes '(200))
            (publish :path "/auto-accept-test"
                     :function #'(lambda (req ent)
                                   (let ((retval (header-slot-value req :accept)))
                                     (with-http-response (req ent)
                                       (with-http-body (req ent)
                                         (format (request-reply-stream req) "~a" retval)
                                         (force-output (request-reply-stream req))
                                         )))))
            
            (test "foo/bar" (values (dohttp "/auto-accept-test" :port port
                                            :cache cache :accept "foo/bar"
                                            :expect 200
                                            :name "aa1"))
                  :test #'equal)
            (check-cache cache "aa2"
                         :lookups 1
                         :alive   0
                         :revalidate 0
                         :validated 0)
            
            (test "foo/bar" (values (dohttp "/auto-accept-test" :port port
                                            :cache cache :accept "foo/bar"
                                            :expect 200
                                            :name "aa2"))
                  :test #'equal)
            
            (check-cache cache "aa3"
                         :lookups 2
                         :alive   1
                         :revalidate 0
                         :validated 0)
            (sleep cache-delay)
            
            ;; force revalidation
            (test "foo/bar" (values (dohttp "/auto-accept-test" :port port
                                            :cache cache :accept "foo/bar"
                                            :expect 200
                                            :name "aa4"))
                  :test #'equal)
            
            (check-cache cache "aa5"
                         :lookups 3
                         :alive   1
                         :revalidate 1
                         :validated 0)
            
            ;; should be cached again
            (test "foo/bar" (values (dohttp "/auto-accept-test" :port port
                                            :cache cache :accept "foo/bar"
                                            :expect 200
                                            :name "aa6"))
                  :test #'equal)
            
            (check-cache cache "aa7"
                         :lookups 4
                         :alive   2
                         :revalidate 1
                         :validated 0)
            
            ))
      
      ;; cleanup
      (stop-aserve-running))))

(defun check-cache (cache name &key lookups alive revalidate validated)
  (test lookups    (net.aserve.client:client-cache-lookups cache) :fail-info name)
  (test alive      (net.aserve.client:client-cache-alive cache) :fail-info name)
  (test revalidate (net.aserve.client:client-cache-revalidate cache) :fail-info name)
  (test validated  (net.aserve.client:client-cache-validated cache) :fail-info name))

(defun dopublish (path &key last-modified headers (response *response-ok*))
  (let ((ent (publish  
              :path path
              :function #'(lambda (req ent)
                            (with-http-response (req ent :response response)
                              (with-http-body (req ent 
                                                   :headers headers)
                                  (dotimes (i 20)
                                    (write-char #\f net.aserve::*html-stream*))
                                  (force-output net.aserve::*html-stream*)))))))
                                         
    (if* last-modified
       then (setf (net.aserve::last-modified ent) last-modified))
    
    ))

(defun dopublish-prefix (path &key headers)
  (publish-prefix  
   :prefix path
   :function 
   #'(lambda (req ent)
       (let ((path (net.uri:uri-path (request-uri req))))
         (multiple-value-bind (ok whole size)
             (match-re "([0-9]+)$" path) 
           (declare (ignore whole))
           (if* ok
              then (setq size (parse-integer size))
              else (setq size 10))
                                    
           (with-http-response (req ent)
             (with-http-body (req ent 
                                  :headers headers)
                                                                     
               (dotimes (i size)
                 (write-char #\f net.aserve::*html-stream*))
                            
               (force-output net.aserve::*html-stream*))))))))
    
  

(defun dohttp (path &key (port 0) cache expect (name "not-given") (accept "*/*"))
  (multiple-value-bind (body code headers)
      (net.aserve.client:do-http-request (format nil "http://127.0.0.1:~d~a"
                                                 port
                                                 path)
        :cache cache
        :accept accept)
    (test expect code :fail-info name)
    
    (values body code headers)))

  


  
      

;; (net.aserve::debug-on :xmit)
;; (net.aserve::debug-off :body)

;; truncate long bodies
(let ((body-kinds (net.aserve::expand-kinds '(:body))))
  (defmethod net.aserve::log1* :around (logger category level message)
    (call-next-method logger category level
                      (if (and (member category body-kinds)
                               (< 100 (length message)))
                          (concatenate 'string (subseq message 0 100) "...")
                          message))))

(if* user::*do-aserve-test* 
   then (when (excl.osi:getenv "ASERVE_LOG_XMIT")
          (net.aserve::debug-on :xmit))
        (user::test-aserve-n :n user::*do-aserve-test*)
   else (format t " (user::test-aserve-n :n 0) will run the aserve test~%"))
