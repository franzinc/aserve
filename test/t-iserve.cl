;; -*- mode: common-lisp; package: net.iserve.test -*-
;;
;; t-iserve.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
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
;; $Id: t-iserve.cl,v 1.2 2000/03/21 06:32:46 jkf Exp $

;; Description:
;;   test iserve

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(eval-when (compile load eval)
  (require :tester))

(defpackage :net.iserve.test
  (:use :common-lisp :excl :net.html.generator :net.iserve 
	:net.iserve.client
	:util.test)
  )

(in-package :net.iserve.test)


(defun test-iserve ()
  (let ((*test-errors* 0)
	(*test-successes* 0)
	(*test-unexpected-failures* 0))
    (let ((port (start-iserve-running)))
      (format t "server started on port ~d~%" port)
      (unwind-protect 
	  (progn
	    (test-publish-file port)
	    )
	(stop-iserve-running)))
    (format t "~%succeses: ~d~%errors ~d~%unexpected errors: ~d~%"
	    *test-successes*
	    *test-errors*
	    *test-unexpected-failures*)))


(defun start-iserve-running ()
  ;; start iserve, return the port on which we've started iserve
  (let ((wserver (start :port nil)))	; let the system pick a port
    (unpublish :all t) ; flush anything published
    (socket::local-port (net.iserve::wserver-socket wserver))
    ))

(defun stop-iserve-running ()
  (shutdown))



;-------- publish-file tests

(defvar *dummy-file-value* nil)
(defvar *dummy-file-name*  "iservetest.xx")

(defun build-dummy-file (length)
  (let ((strp (make-string-output-stream)))
    (dotimes (i length)
      (write-char (code-char (+ #.(char-code #\a) (mod i 26))) strp)
      (if* (zerop (mod i 70))
	 then (terpri strp)))
    (terpri strp)
    (setq *dummy-file-value* (get-output-stream-string strp))
    (with-open-file (p *dummy-file-name* :direction :output
		     :if-exists :supersede)
      (write-sequence *dummy-file-value* p))))
  

(defun test-publish-file (port)
  (build-dummy-file 8055)
  
  (publish-file :path "/frob" :file *dummy-file-name*
		:content-type "text/plain")
  
  (dolist (protocol '(:http/1.0 :http/1.1))
    (multiple-value-bind (code headers body)
	(do-http-request (format nil "http://localhost:~a/frob" port)
	  :protocol protocol
	  :keep-alive t)
      (test 200 code)
      (test (format nil "text/plain" port)
	    (cdr (assoc "content-type" headers :test #'equal))
	    :test #'equal)
      #+ignore (if* (eq protocol :http/1.1)
	 then (test "chunked"
		    (cdr (assoc "transfer-encoding" headers :test #'equal))
		    :test #'equalp))
      (test *dummy-file-value* body :test #'equal))))

	
  

  

