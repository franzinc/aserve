;; -*- mode: common-lisp; package: net.aserve.test -*-
;;
;; t-aserve.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation; 
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;

;; Description:
;;  This file is a copy of aserve/test/t-aserve.cl 
;;  modified to run the same tests in several servers
;;  simultaneously.
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
(defvar *aserve-test-config* (make-instance 'aserve-test-config))
(defmacro asc (slot &aux (accessor
			  (read-from-string
			   (format nil "~A::asc-~A "
				   (package-name 
				    (find-package :net.aserve.test))
				   slot))))
  `(,accessor *aserve-test-config*))



; remember where we were loaded from so we can run manually
(defparameter *aserve-load-truename* *load-pathname*)

(defvar *aserve-test-configs* nil)

(defun asp-index ()
  (if *log-wserver-name*
      (format nil "~A[~A]" 
	      (mp:process-name mp:*current-process*)
	      (sys::thread-bindstack-index
	       (mp:process-thread mp:*current-process*)))
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
			    &aux wname)
  (typecase n
    ((integer 0) nil)
    (null (return-from user::test-aserve-n nil))
    (otherwise 
     ;; In case someone sets *do-aserve-test* to t.
     (setq n 0)))
  (setq *aserve-test-configs* (make-array (if (eql n 0) 1 n)))
  (case n
    (0
     ;; In simple one-thread test, log server name only if requested
     ;; explicitly.
     (if l-n-p
	 (setq *log-wserver-name* log-name)
       (setq *log-wserver-name* nil))
     (test-aserve test-timeouts :direct direct :proxy proxy :proxyproxy proxyproxy 
		  :ssl ssl :proxy-auth proxy-auth))
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
		      (test-aserve test-timeouts
				   :direct direct :proxy proxy :proxyproxy proxyproxy :ssl ssl
				   :proxy-auth proxy-auth
				   )
		      (setq clean t))
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
	   (port (start-aserve-running)))
      (asc-format "server started on port ~d" port)
      (unwind-protect 
	  (labels ((do-tests ()
		     ; run test with and with compression
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
				   
		     (test-publish-file port nil) ; no compression
		     #+unix
		     (test-publish-file port t) ; with compression
		     
		     (test-publish-directory port)
		     (test-publish-computed port)
		     (test-publish-multi port)
		     (test-publish-prefix port)
		     (test-authorization port)
		     (test-encoding)
		     (test-forms port)
		     (test-client port)
		     (test-cgi port)
		     (test-http-copy-file port)
		     (test-client-unicode-content-length)
		     (test-expect-header-responses)
		     (test-retry-on-timeout port)

		     (if* (member :ics *features*)
			then (test-international port)
			     (test-spr27296))
		     (if* test-timeouts 
			then (test-timeouts port))))
	    
	    
		    
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
			    (do-tests)
		       else (asc-format "~%>> it isn't so ssl tests skipped~%~%")))
	    ) ;;; end body of unwind-protect 
	; cleanup forms:
	(stop-aserve-running)
	(stop-proxy-running)
	(stop-proxy-running)
	)))
  (if* (or (> util.test::*test-errors* 0)
	   (> util.test::*test-successes* 0)
	   (> util.test::*test-unexpected-failures* 0))
     then (asc-format "Test information from other threads:")
	  (asc-format "Errors:    ~d" util.test::*test-errors*)
	  (asc-format "Successes: ~d~%" util.test::*test-successes*)
	  (asc-format "Unexpected failures: ~d~%" 
		      util.test::*test-unexpected-failures*)))
    


(defun start-aserve-running (&optional ssl)
  ;; start aserve, return the port on which we've started aserve
  (let ((wserver (start :port nil :server :new :ssl-args (and ssl (list :certificate ssl))
			:listeners 20 ; must be at least 3 for keep-alive to be possible
			))); let the system pick a port
    (setq *wserver* wserver)
    (when *aserve-set-full-debug*
      (apply #'net.aserve::debug-on *aserve-set-full-debug*))
    (logmess (format nil "wserver name is now ~A" (wserver-name *wserver*)))
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
    (logmess (format nil "proxy wserver name is ~A" (wserver-name *wserver*))))
  
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
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name) port)))
    
    ;; manual authorization testing
    ;; basic authorization
    ;;
    (publish :path "/secret"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 (multiple-value-bind (name password) (get-basic-authorization req)
		   (if* (and (equal name "foo") (equal password "bar"))
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
    
    ; bad password
    (test 401
	  (values2 (x-do-http-request (format nil "~a/secret" prefix-local)
				      :basic-authorization '("xxfoo" . "bar"))))
    
    ; test auth precedence, basic-auth over userinfo
    (test 401
	  (values2 (x-do-http-request (format nil "http://foo:bar@localhost:~a/secret" port)
				      :basic-authorization '("xxfoo" . "bar"))))
    
    ; auth via userinfo
    (test 200
	  (values2 (x-do-http-request (format nil "http://foo:bar@localhost:~a/secret" port))))
    

    
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
    

  
(defun test-client (port)
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
                                    (not (eq socket2 socket3)))))))))))
  
  


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
    (logmess (format nil "wserver name is now ~A" (wserver-name *wserver*)))
    (let ((*wserver* proxy-wserver))
      (logmess (format nil "proxy wserver name is ~A" (wserver-name *wserver*))))
    
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
      (test "http://www.franz.com" (cdr (assoc :location headers))
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
    (let ((*wserver* server))
      (logmess (format nil "test-spr27296 wserver name is ~A" (wserver-name *wserver*))))
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
    (let ((*wserver* server))
      (logmess
       (format nil "test-client-unicode-content-length wserver name is ~A"
	       (wserver-name *wserver*))))
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
  
  ; allow all
  (start-proxy-running t)
  (test 200 (values2 (x-do-http-request "http://www.franz.com")))
  (stop-proxy-running)
  
  ; allow from localhost
  (start-proxy-running (make-instance 'proxy-control
			 :location (make-instance 'location-authorizer
				     :patterns '((:accept "127.1")
						 :deny))))
  (test 200 (and :second (values2 (x-do-http-request "http://www.franz.com"))))
  (stop-proxy-running)
  
  
  ; disallow from localhost
  (start-proxy-running (make-instance 'proxy-control
			 :location (make-instance 'location-authorizer
				     :patterns '((:accept "192.168.111.222")
						 (:deny "127.1")
						 :deny))))
  (test 404 (and :second (values2 (x-do-http-request "http://www.franz.com"))))
  (stop-proxy-running)
  
  ; allow to franz.com
  (start-proxy-running (make-instance 'proxy-control
			 :destinations '("www.franz.com"
					 ("franz.com" 333 80) ; 
					 ("www.whitehouse.gov" 222) ; only 222
					 ("www.cnn.com") ; no ports specified
					 )))
  (test 200 (and :third (values2 (x-do-http-request "http://www.franz.com"))))
  (test 404 (and :fouth (values2 (x-do-http-request "http://www.noaa.gov"))))
  (test 404 (values2 (x-do-http-request "http://www.noaa.gov:444")))
  (test 200 (values2 (x-do-http-request "http://franz.com")))
  (test 200 (values2 (x-do-http-request "http://franz.com:80")))
  (test 404 (values2 (x-do-http-request "http://www.whitehouse.gov")))
  (test 404 (values2 (x-do-http-request "http://www.cnn.com")))
  
  (stop-proxy-running)
  
  (format t "proxy authorization using hash table for destination~%")
  ; allow using a hash table
  (let ((ht (make-hash-table :test #'equalp)))
    (dolist (site '("www.franz.com" 
		    ("franz.com" 333 80)
		    ("www.whitehouse.gov" 222)
		    ("www.cnn.com")))
		    
      (setf (gethash (if* (consp site) then (car site) else site) ht) 
	(if* (consp site) 
	   then (cdr site) 
	   else t)))
    
    (start-proxy-running (make-instance 'proxy-control
			   :destinations ht))
    
    (test 200 (and :third (values2 (x-do-http-request "http://www.franz.com"))))
    (test 404 (and :fouth (values2 (x-do-http-request "http://www.noaa.gov"))))
    (test 404 (values2 (x-do-http-request "http://www.noaa.gov:444")))
    (test 200 (values2 (x-do-http-request "http://franz.com")))
    (test 200 (values2 (x-do-http-request "http://franz.com:80")))
    (test 404 (values2 (x-do-http-request "http://www.whitehouse.gov")))
    (test 404 (values2 (x-do-http-request "http://www.cnn.com")))
    
    (stop-proxy-running)))


(defun test-expect-header-responses ()
  (let* ((server (start :port nil :server :new
			:external-format (crlf-base-ef :utf8)))
	 (base (format nil "http://localhost:~a" 
		       (socket:local-port (net.aserve::wserver-socket *wserver*)))))
    (unwind-protect
	;; [bug23200] Avoid server leak in each test.
	(let ()
	  (let ((*wserver* server))
	    (logmess
	     (format nil "test-expect-header-responses wserver name is ~A"
		     (wserver-name *wserver*))))
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
    
	  (let ((tests '(("/does-not-exist" :post nil t 404)
			 ("/secret-auth" :post nil t 401)
			 ("/secret-auth" :post ("foo2" . "wrongpass") t 401)
			 ("/secret-auth" :post ("foo2" . "bar2") t 100)
			 ("/secret-auth" :get ("foo2" . "bar2") t 200)
			 ("/noauth" :get nil t 200)
			 ("/noauth" :put nil t 100)
			 ("/no-auto" :get nil t 200)
			 ("/no-auto" :put nil t 100)
			 ("/no-auto" :put nil nil 200)
			 ("/no-auto" :post nil t 100)
			 ("/no-auto" :post nil nil 200)
			 )))
	    (loop for (path method auth expect-hdr expected-code) in tests
		  do (let* ((body-p (member method '(:put :post)))
			    (body (when body-p (query-to-form-urlencoded '(("body" . "stuff")))))
			    (creq (make-http-client-request
				   (format nil "~a~a" base path)
				   :method method
				   :ssl (asc x-ssl)
				   :basic-authorization auth
				   :content (when body-p body)
				   :content-type (when body-p "application/x-www-form-urlencoded")
				   :headers (when expect-hdr '(("Expect" . "100-continue"))))))
		       (unwind-protect 
			   (progn (read-client-response-headers creq)
				  (test expected-code (client-request-response-code creq)))
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
   else (format t 
		" (user::test-aserve-n :n 0) will run the aserve test~%"))
