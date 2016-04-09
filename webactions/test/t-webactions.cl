;; -*- mode: common-lisp; package: net.aserve.test -*-
;;
;; t-webactions.cl
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
  (with-tests (:name "aserve")
    (let* ((*wserver* *wserver*)
	   (port (start-aserve-running)))
      (format t "server started on port ~d~%" port)
      (unwind-protect 
	  (flet ((do-tests ()
		  (sitea-tests port)
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
  (apply #'do-http-request uri :ssl *x-ssl* args))

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





(def-clp-function tweb_foo (req ent args body)
  (declare (ignore args))
  (html "{start_foo}")
  (emit-clp-entity req ent body)
  (html "{end_foo}"))













(test-webactions)
