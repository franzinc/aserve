;; -*- mode: common-lisp; package: net.aserve.testwa -*-
;;
;; t-webactions.cl
;;
;; See the file LICENSE for the full license governing this code.
;;
;;

;; Description:
;;   test webactions in aserve

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(eval-when (compile load eval)
  (require :tester))

(defpackage :net.aserve.testwa
  (:use :common-lisp :excl :net.html.generator :net.aserve 
	:net.aserve.client
	:util.test)
  )

(in-package :net.aserve.testwa)

(defvar *x-ssl*)
(defvar *cj* nil)
(defvar *test-dir* (directory-namestring *load-pathname*))

(defun test-webactions ()
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
  
  (setq *cj* (make-instance 'cookie-jar))
  
  (with-tests (:name "webactions")
    (let* ((*wserver* *wserver*)
	   (port (start-aserve-running)))
      (format t "server started on port ~d~%" port)
      (unwind-protect 
	  (flet ((do-tests ()
		  (sitea-tests port)
		  (siteb-tests port)
		   ))
	    (format t "~%~%===== test direct ~%~%")
	    (do-tests)
	    
	    ))
	; cleanup forms:
	(stop-aserve-running)))
  (if* (or (> util.test::*test-errors* 0)
	   (> util.test::*test-successes* 0)
	   (> util.test::*test-unexpected-failures* 0))
     then (format t "~%Test information from other threads:~%")
	  (format t "Errors:    ~d~%" util.test::*test-errors*)
	  (format t "Successes: ~d~%~%" util.test::*test-successes*)
	  (format t "Unexpected failures: ~d~%" 
		  util.test::*test-unexpected-failures*)))



(defun x-do-http-request (uri &rest args)
  ;; add a proxy arg
  (apply #'do-http-request uri :ssl *x-ssl* :cookies *cj* args))

(defmacro values2 (form)
  ;; return the second value
  (let ((v1 (gensym))
	(v2 (gensym)))
    `(multiple-value-bind (,v1 ,v2) ,form
       (declare (ignore ,v1))
       ,v2)))

(defun start-aserve-running (&optional ssl)
  ;; start aserve, return the port on which we've started aserve
  (let ((wserver (start :port nil :server :new :ssl ssl))); let the system pick a port
    (setq *wserver* wserver)
    (unpublish :all t) ; flush anything published
    (setq *x-ssl* ssl)
    (socket::local-port (net.aserve::wserver-socket wserver))
    ))


(defun stop-aserve-running ()
  (shutdown))




;;--------- the tests ------------

(defvar *sitea-vara* nil)

(defparameter *sitea-session-cookie-only* nil)

(defun sitea-tests (port)
  ; load in project
  (let ((prefix-local (format nil "http://localhost:~a" port)))
    (load (concatenate 'string *test-dir* "sitea/project.cl"))

    
    ; test a sample symbolic page
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/pagea" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode))
    
    
    ; test a non existant one
    
    (test 404 (values2 (x-do-http-request (format nil "~a/sitea/notthere" 
						  prefix-local))))
    

    
    ;; test extended maps... with multiple actions firing
    (setq *sitea-vara* nil)
    
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/action" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode)
      (test '(:foo :foo :foo) *sitea-vara* :test #'equal-nocr))
      

    ;; test redir to previous page preceeded by one push
    (setq *sitea-vara* nil)
    
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/redirtry" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode)
      (test '(:foo :foo :foo :foo) *sitea-vara* :test #'equal-nocr))
      

      
    ;; test an action returning a clp name
    ;; it's not clear we should support this.  it does seem more 
    ;; regular to allow it... but then we can't as easy detect
    ;; mistyped symbolic page names returned by action functions
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/action2" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode))
    
    
    ;; test an action returning a symbolic page name which then
    ;; points at a clp file
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/action3" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode))
      

    ;; bogus sym page name returned by action
    (test 404 (values2 (x-do-http-request (format nil "~a/sitea/action4" 
						  prefix-local))))      

    ;; test with prefix
      
    ; smallest prefix
    (setq *sitea-vara* nil)
    
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/act" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode)
      (test '(:foo) *sitea-vara* :test #'equal-nocr))

      
    ; bigger suffix
    (setq *sitea-vara* nil)
    
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/act234234/asd/asd/ad" prefix-local))
      
      (test "test
" data :test #'equal-nocr)
      (test 200 retcode)
      (test '(:foo) *sitea-vara* :test #'equal-nocr))
          

      
    ;; verify that the fil prefix works too
    (setq *sitea-vara* nil)
    
    (multiple-value-bind (data retcode headers)
	(x-do-http-request (format nil "~a/sitea/filesss/act234234/asd/asd/ad" prefix-local))
      
      (test "the second test file: file2
" data :test #'equal-nocr)
      (test 200 retcode)
      (test "text/html" (cdr (assoc :content-type headers))
	    :test #'equal)
      (test '(:foo :foo) *sitea-vara* :test #'equal))

      
    ; check to see if we can change the content-type of a
    ; single file
      
    (multiple-value-bind (data retcode headers)
	(x-do-http-request (format nil "~a/sitea/testctype" prefix-local))
	
      (test "This is a plain text file
" data :test #'equal-nocr)
      (test 200 retcode)
      (test "text/plain" (cdr (assoc :content-type headers))
	    :test #'equal-nocr))
      

    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/sitea/file4" prefix-local))
	
      (test "X{start_foo}Y{start_foo}Z{end_foo}W{end_foo}
" data :test #'equal-nocr)
      (test 200 retcode)
      
      
      ;; test that sessions are not created for bogus urls references
      ;;
      (locally
          (declare (special *sitea-webaction-entity*))
        (let ((sm (net.aserve::webaction-websession-master 
                   (net.aserve::webaction-webaction *sitea-webaction-entity*))))
          (let ((before (hash-table-count (net.aserve::sm-websessions sm))))
            (dotimes (i 10)
              (x-do-http-request (format nil "~a/sitea/bogusurl" prefix-local)))
            (test before (hash-table-count (net.aserve::sm-websessions sm)) ))))
      
      ;; test with :session-cookie-only arg at nil
      ;; meaning that we'll include the session in the url
      
      ;; clear cookie jar
      (setq *cj* (make-instance 'cookie-jar))
      
      (multiple-value-bind (body retcode headers)
          (x-do-http-request (format nil "~a/sitea/redirtest" prefix-local)
                             :redirect nil)
        
        (declare (ignore body retcode))
        ;; there will be a set-cookie
        (test t (not (null (assoc :set-cookie headers))))
        
        ;; and the url will have a session id in it
        (test t (not (null (search "/~" (cdr (assoc :location headers))))))
        
        )
      
      
      ;; now test with :session-cookie-only arg at t meaning
      ;; no url rewriting to include the session id
      
      (setq *sitea-session-cookie-only* t)
      ;; reload the project
      (load (concatenate 'string *test-dir* "sitea/project.cl"))
      
      ;; clear cookie jar
      (setq *cj* (make-instance 'cookie-jar))
      
      (multiple-value-bind (body retcode headers)
          (x-do-http-request (format nil "~a/sitea/redirtest" prefix-local)
                             :redirect nil)
        (declare (ignore body retcode))
        ;; there will be a set-cookie
        (test t (not (null (assoc :set-cookie headers))))
        
        ;; and the url will not have a session id in it
        (test nil (not (null (search "/~" (cdr (assoc :location headers))))))
        
        )
      
      
      ;; test that we can alter the response code
      (multiple-value-bind (body retcode headers)
          (x-do-http-request (format nil "~a/sitea/retcodetest" prefix-local)
                             :redirect nil)
        (declare (ignore body headers))
        (test  201 retcode))
      
      ;; verify we can dynamically specify the return code
      (multiple-value-bind (body retcode headers)
          (x-do-http-request (format nil "~a/sitea/retanycodetest" prefix-local)
                             :redirect nil)
        (declare (ignore body headers))
        (test  210 retcode))
      
      ;; test it works if use a string "210" value
      (multiple-value-bind (body retcode headers)
          (x-do-http-request (format nil "~a/sitea/retanycodetest-two" prefix-local)
                             :redirect nil)
        (declare (ignore body headers))
        (test  210 retcode))
      
      (format t "~2%Expect an error message to be printed about a bad response code~2%")
      (test-error
          (x-do-http-request (format nil "~a/sitea/retanycodetest-bogus" prefix-local)
                             :redirect nil))
      

      (multiple-value-bind (body retcode headers)
          (x-do-http-request (format nil "~a/sitea/retincluded" prefix-local)
                             :redirect nil)
        (declare (ignore retcode headers))
        ;;
        ;; note that Windows will return cr-lf and unix lf, so eliminate the cr
        ;; as that's not the important part of the test.
        (test "fooabc123def
bar
"  (delete #\^m body) :test #'equal))
     
          
      )))



(defun equal-nocr (a b)
  ;; compare strings after removing all #\return's since they
  ;; can appears in web results on certain machines
  (equal (remove #\return a)
	 (remove #\return b)))





(defun action-sitea-pushit (req ent)
  (declare (ignore req ent))
  (push :foo *sitea-vara*)
  :continue
  )

(defun action-retname-two (req ent)
  ;; return actual name to run 
  (declare (ignore req ent))
  "file1.clp")


(defun action-retname-three (req ent)
  ;; return another symbolic page name
  (declare (ignore req ent))
  "pagea")


(defun action-retname-four (req ent)
  ;; return a bogus symbolic page name
  (declare (ignore req ent))
  "thisdoesntexist")

(defun action-set-retany (req ent)
  (declare (ignore ent))
  (set-retcode-value req 210))

(defun action-set-retany-two (req ent)
  (declare (ignore ent))
  (set-retcode-value req "210"))

(defun action-set-retany-bogus (req ent)
  (declare (ignore ent))
  (set-retcode-value req "garbage"))


  
(defun set-retcode-value (req code)  
  ;; set the value of testretcode to the given code
  (declare (ignore ent))
  (let ((sess (websession-from-req req)))
    (if* sess
       then (setf (websession-variable sess "testretcode") code))
    "retany.clp"))




(def-clp-function tweb_foo (req ent args body)
  (declare (ignore args))
  (html "{start_foo}")
  (emit-clp-entity req ent body)
  (html "{end_foo}"))

;; Begin siteb (subdirectory) tests

(defun siteb-tests (port)
  (let ((prefix-local (format nil "http://localhost:~a" port)))
    (load (concatenate 'string *test-dir* "siteb/project.cl"))

    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/siteb/sub/file1.clp" prefix-local))
      (declare (ignore retcode))
      ;; Without bug fix, the rewritten line does not have "sub/".
      (test "/sub/file2.html\">file2.html</a>" data
	    :test #'(lambda (x y) (search x y)))
      
      ;; verify javascript src converted too
      (test "/sub/js/foo.js\">" data
	    :test #'(lambda (x y) (search x y))))
    (multiple-value-bind (data retcode)
	(x-do-http-request (format nil "~a/siteb/pageb" prefix-local))
      (declare (ignore retcode))
      ;; Without bug fix, the rewritten line does not have "sub/".
      (test "/sub/file2.html\">file2.html</a>" data
	    :test #'(lambda (x y) (search x y))))))

;; End siteb (subdirectory) tests










(if* (and (boundp 'user::*do-aserve-test*) user::*do-aserve-test*)
   then (net.aserve.testwa::test-webactions)
   else (format t "~%(net.aserve.testwa::test-webactions) will run the webactions test~%"))



