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
;; $Id: t-iserve.cl,v 1.4 2000/03/21 17:20:14 jkf Exp $

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

(defun build-dummy-file (length line-length name)
  ;; write a dummy file named  name  (if name isn't nil)
  ;; of a given   length   with lines no longer than
  ;; line-length.
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
    
    result))
  

(defun test-publish-file (port)
  (let (dummy-1-contents 
	(dummy-1-name "xxiservetest.txt")
	dummy-2-contents
	(dummy-2-name "xx2iservetest.txt")
	(prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name)
			      port)))
    
    (setq dummy-1-contents (build-dummy-file 8055 70 dummy-1-name))

    
    ;; basic publish file test
    ;;
    ;; publish a file and retrieve it.
    ;; the result will be the same since given that we know the
    ;; length of the file, chunking won't be needed
    ;; 
    (publish-file :path "/frob" :file dummy-1-name
		  :content-type "text/plain")

    ;; 
    (dolist (cur-prefix (list prefix-local prefix-dns))
      (dolist (keep-alive '(nil t))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (format t "test 1 - ~s~%" (list keep-alive protocol))
	  (multiple-value-bind (code headers body)
	      (do-http-request (format nil "~a/frob" cur-prefix)
		:protocol protocol
		:keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain" port)
		  (cdr (assoc "content-type" headers :test #'equal))
		  :test #'equal)
	    #+ignore (if* (eq protocol :http/1.1)
			then (test "chunked"
				   (cdr (assoc "transfer-encoding" headers 
					       :test #'equal))
				   :test #'equalp))
	    (test dummy-1-contents body :test #'equal)))))


    (setq dummy-2-contents (build-dummy-file 8055 65 dummy-2-name))

    
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
		  :preload t)

    ;; 
    (dolist (cur-prefix (list prefix-local prefix-dns))
      (dolist (keep-alive '(nil t))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (format t "test 2 - ~s~%" (list keep-alive protocol))
	  (multiple-value-bind (code headers body)
	      (do-http-request (format nil "~a/frob2" cur-prefix)
		:protocol protocol
		:keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain" port)
		  (cdr (assoc "content-type" headers :test #'equal))
		  :test #'equal)
	    #+ignore (if* (eq protocol :http/1.1)
			then (test "chunked"
				   (cdr (assoc "transfer-encoding" headers 
					       :test #'equal))
				   :test #'equalp))
	    (test dummy-2-contents body :test #'equal)))))

    
    ;;;; remove published file test
    ;;
    ; verify it's still there
    (test 200 (values (do-http-request (format nil "~a/frob" prefix-local))))
    (test 200 (values (do-http-request (format nil "~a/frob" prefix-dns))))
    
    ; remove it
    (publish-file :path "/frob" :remove t)
    
    ; verify that it's not there:
    (test 404 (values (do-http-request (format nil "~a/frob" prefix-local))))
    (test 404 (values (do-http-request (format nil "~a/frob" prefix-dns))))
    
    ;; likewise for frob2
    
    ; verify it's still there
    (test 200 (values (do-http-request (format nil "~a/frob2" prefix-local))))
    (test 200 (values (do-http-request (format nil "~a/frob2" prefix-dns))))
    
    ; remove it
    (publish-file :path "/frob2" :remove t)
    
    ; verify that it's not there:
    (test 404 (values (do-http-request (format nil "~a/frob2" prefix-local))))
    (test 404 (values (do-http-request (format nil "~a/frob2" prefix-dns))))
    
    

    
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
    
    (multiple-value-bind (code headers body)
	(do-http-request (format nil "~a/checkit" prefix-local))
      (declare (ignore headers))
      (test 200 (and :df-test code))
      (test dummy-1-contents body :test #'equal))
    
    (multiple-value-bind (code headers body)
	(do-http-request (format nil "~a/checkit" prefix-dns))
      (declare (ignore headers))
      (test 200 (and :df-test code))
      (test dummy-2-contents body :test #'equal))

    ;; remove the localhost one
    (publish-file :path "/checkit" 
		  :host "localhost"
		  :remove t)
    ; verify it's gone:
    (test 404 (values (do-http-request (format nil "~a/checkit" 
					       prefix-local))))
    ; but the the dns one is still there
    (test 200 (values (do-http-request (format nil "~a/checkit" prefix-dns))))
    
    ; remove the dns one
    (publish-file :path "/checkit" 
		  :host (long-site-name)
		  :remove t)
    
    ; verify it's gone too
    (test 404 (values (do-http-request (format nil "~a/checkit" 
					       prefix-dns))))

    
    
    
    
    
    
    ; cleanup
    (delete-file dummy-1-name)
    (delete-file dummy-2-name)
    ))
    





	
  

  

