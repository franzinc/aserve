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
;; $Id: t-iserve.cl,v 1.7 2000/03/22 22:32:16 jkf Exp $

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

; set to nil before loading the test to prevent the test from auto-running
(defvar user::*do-iserve-test* t)

(defun test-iserve ()
  (with-tests (:name "iserve")
    (let ((port (start-iserve-running)))
      (format t "server started on port ~d~%" port)
      (unwind-protect 
	  (progn
	    (test-publish-file port)
	    (test-publish-computed port)
	    (test-authorization port))
	(stop-iserve-running)))))
    


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
    



(defun test-publish-computed (port)
  ;; test publishing computed entities
  (let ((dummy-1-content (build-dummy-file 0 50 nil))
	(dummy-2-content (build-dummy-file 1 50 nil))
	(dummy-3-content (build-dummy-file 100 50 nil))
	(dummy-4-content (build-dummy-file 1000 50 nil))
	(dummy-5-content (build-dummy-file 10000 50 nil))
	(dummy-6-content (build-dummy-file 100000 50 nil))
	
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
		 :function
		 #'(lambda (req ent)
		     (with-http-response (req ent)
		       (with-http-body (req ent)
			 (write-sequence this *html-stream*))))))
      (dolist (keep-alive '(nil t))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (multiple-value-bind (code headers body)
	      (do-http-request (format nil "~a~a" prefix-local (car pair))
		:protocol protocol
		:keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain" port)
		  (cdr (assoc "content-type" headers :test #'equal))
		  :test #'equal)
	    (if* (eq protocol :http/1.1)
	       then (test "chunked"
			  (cdr (assoc "transfer-encoding" headers 
				      :test #'equal))
			  :test #'equalp))
	    (test (cadr pair) body :test #'equal)))))))


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
    (multiple-value-bind (code headers)
	(do-http-request (format nil "~a/secret" prefix-local))
      (test 401 code)
      ; verify that we are asking for the right realm
      (test "Basic realm=\"secretserver\""
	    (cdr (assoc "www-authenticate" headers :test #'equal))
	    :test #'equal))
  
    
    ; good password
    (test 200
	  (values (do-http-request (format nil "~a/secret" prefix-local)
		    :basic-authorization '("foo" . "bar"))))
    
    ; bad password
    (test 401
	  (values (do-http-request (format nil "~a/secret" prefix-local)
		    :basic-authorization '("xxfoo" . "bar"))))
    


    
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
	  (values (do-http-request (format nil "~a/local-secret"
					   prefix-local))))
    
    (test 404
	  (values (do-http-request (format nil "~a/local-secret"
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
    
    (multiple-value-bind (ccode headers)
	(do-http-request (format nil "~a/secret-auth" prefix-local))
      (test 401 ccode)
      (test "Basic realm=\"SecretAuth\""
	    (cdr (assoc "www-authenticate" headers :test #'equal))
	    :test #'equal))
    
    (test 200
	  (values (do-http-request (format nil "~a/secret-auth" prefix-local)
		    :basic-authorization '("foo2" . "bar2"))))
    
    (test 200
	  (values (do-http-request (format nil "~a/secret-auth" prefix-local)
		    :basic-authorization '("foo3" . "bar3"))))
    
    (test 401
	  (values (do-http-request (format nil "~a/secret-auth" prefix-local)
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
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      (test 200
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      ; now deny all
      (setf (location-authorizer-patterns loca) '(:deny)) 
      
      (test 404
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 404
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      
      ;; accept from localhost only
      (setf (location-authorizer-patterns loca) 
	'((:accept "127.0" 8)
	  :deny))
      (test 200
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 404
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      ;; accept from dns name only 
      
      (setf (location-authorizer-patterns loca) 
	`((:accept ,(long-site-name))
	  :deny))
      
      (test 404
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 200
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      
      ;; deny dns and accept all others
      (setf (location-authorizer-patterns loca) 
	`((:deny ,(long-site-name))
	  :accept))
      
      (test 200
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 404
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      
      ;; deny localhost and accept all others
      (setf (location-authorizer-patterns loca) 
	'((:deny "127.0" 8)
	  :accept))
      
      (test 404
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 200
	  (values (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
    
    
    
    
    
    
    )))

    
    
    
    
    
    



    
(if* user::*do-iserve-test* then (test-iserve))

	
    
   
  
  

	
  

  

