;; -*- mode: common-lisp; package: neoe -*-
;;
;; examples.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: examples.cl,v 1.1.2.1 2000/02/18 18:04:36 jkf Exp $

;; Description:
;;   neo examples

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



;; examples of web pages
(defpackage :net.iserve.examples ;; neo example
  (:use :common-lisp :excl :net.html.generator :net.iserve))

(in-package :net.iserve.examples)

;; flush all publishing done so far:
(unpublish :all t)


(publish :path "/" 
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html
		  (:head (:title "Welcome to Neo"))
		  (:body 
		   (:h1 "Welcome to Neo")
		   (:p "These links show off some of Neo's capabilities. ")
		   (:i "This server's host name is "
		    (:princ-safe (header-slot-value req "host")))
		   :p
		   (:b "Sample pages") :br
			 ((:a :href "/gc") "Garbage Collector Stats") :br
			 ((:a :href "/apropos") "Apropos") :br
			 ((:a :href "/pic") "Sample jpeg") :br
			 ((:a :href "/pic-gen") "generated jpeg") "- hit reload to switch images" :br
			 ((:a :href "/cookietest") "test cookies") :br
			 ((:a :href "/secret") "Test authorization")
 			 " (name: " (:b "foo") ", password: " (:b "bar") ")"
			 :br
			 ((:a :href "/timeout") "Test timeout")
			 :br
			 ((:a :href "/getfile") "Client to server file transfer")
			 :br
			 ((:a :href "/missing-link") "Missing Link")
			 "should get error"
			 )
		  
			 )))))
			     


;; a very simple page.  This is so simple it doesn't put out the required
;; tags (like <html>) yet I suspect that most browsers will display it
;; correctly regardless.
(publish :path "/hello"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "Hello World!")))))

;; this is the "/hello" example above modified to put out the correct
;; html tags around the page.
(publish :path "/hello2"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html 
			    (:html
			     (:body "Hello World!")))))))

;; display the current gc statistics.
(publish :path "/gc"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (macrolet ((build-gsgc-table ()
			  `(html
			    ,@(mapcar 
			       #'(lambda (kind)
				   `(:tr (:td (:princ ,kind))
					 (:td (:princ-safe
					       (sys:gsgc-parameter ,kind)))))
			       '(:generation-spread
				 :current-generation
				 :tenure-limit
				 :free-bytes-new-other
				 :free-percent-new
				 :free-bytes-new-pages
				 :expansion-free-percent-new
				 :expansion-free-percent-old
				 :quantum
				 )))))
			     
				   
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		     (html (:head (:title "Allegro gc parameters"))
			   (:body
			    ((:table :bgcolor "silver" :bordercolor "blue"
				     :border "3" :cellpadding "3"
				     :cellspacing "3")
			     (:tr (:td (:b "gsgc parameter")) (:td (:b "Value")))
			     (build-gsgc-table)))))))))
		       


;; display a picture from a file.
(publish-file :path "/pic" :file "examples/prfile9.jpg"
	      :content-type "image/jpeg")



;; this is a demonstration of how you can return a jpeg 
;; image that was created on the fly (rather thsn read from
;; a file via publish-file). 
;; We don't want to actually create the image here, so we 
;; cheat and read it from a file, but it shows that you can
;; send any stream of bytes and they will be given the correct
;; mime type.
;; 
(publish :path "/pic-gen"
	 :content-type "image/jpeg"
	 :function
	 (let ((selector 0)) ; chose one of two pictures
	   #'(lambda (req ent)
	       (with-http-response (req ent)
		 (with-http-body (req ent :format :binary)
		   ; here is where you would generate the picture.
		   ; we're just reading it from a file in this example
		   (let ((stream (request-reply-stream req)))
		     (with-open-file (p (nth selector
					     '("prfile9.jpg" "fresh.jpg"))
				      :element-type '(unsigned-byte 8))

		       (setq selector (mod (1+ selector) 2))
		     
		       (loop
			 (let ((val (read-byte p nil nil)))
			   (if* (null val) 
			      then ;eof 
				   (return))
			   (write-byte val stream)
			   )))))))))
	 




;;
;; here's a form using the 'post' method
;;
(publish :path "/tform" 
	 :content-type "text/html"
	 :function
	 (let ((name "unknown"))
	   #'(lambda (req ent)
	       (let ((body (get-request-body req)))
		 (format t "got body ~s~%" body)
		 (let ((gotname (assoc "username"
				       (decode-form-urlencoded body)
					:test #'equal)))
		   (if* gotname
		      then (setq name (cdr gotname)))))
		 
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "test form"))
			 (:body "Hello " (:princ-safe name) ", "
				"Enter your name: "
				((:form :action "/tform"
					:method "post")
				 ((:input :type "text"
					  :maxlength 10
					  :size 10
					  :name "username"))))))))))

			      
				    

;; example of a form that uses that 'get' method
;;
(publish 
 :path "/apropos"
 :content-type "text/html"
 :function
 #'(lambda (req ent)
     (format t "request uri is ~s~%" (request-uri req))
     (let ((lookup (assoc "symbol" (request-query req) :test #'equal)))
       (with-http-response (req ent)
	 (with-http-body (req ent)
	   (html (:head (:title "Allegro Apropos"))
		 ((:body :background "/neoweb/fresh.jpg")
		  "New Apropos of "
		  ((:form :action "/apropos"
			  :method "get")
		   ((:input :type "text"
			    :maxlength 40
			    :size 20
			    :name "symbol")))
		  :p
			
		  (if* lookup
		     then (html :hr (:b "Apropos") " of " 
				(:princ-safe (cdr lookup))
				:br
				:br)
			  (let ((ans (apropos-list (cdr lookup))))
			    (if* (null ans)
			       then (html "No Match Found")
			       else (macrolet ((my-td (str)
						 `(html ((:td 
							  :bgcolor "blue")
							 ((:font :color "white"
								 :size "+1")
							  (:b ,str))))))
						       
				      (html ((:table
					      :bgcolor "silver"
					      :bordercolor "blue"
					      :border 3
					      :cellpadding 3
					      )
						   
					     (:tr
					      (my-td "Symbol")
					      (my-td "boundp")
					      (my-td "fboundp"))
						 
						   
					     (dolist (val ans)
					       (html (:tr 
						      (:td (:prin1-safe val))
						      (:td (:prin1 (and (boundp val) t)))
						      (:td (:prin1 (and (fboundp val) t))))
						     :newline)))))))
		     else (html "Enter name and type enter")))
		 :newline))))))


;; a preloaded picture file
(publish-file :path "/neoweb/fresh.jpg"
	      :file "examples/fresh.jpg"
	      :content-type "image/jpeg"
	      :preload t)

;; a preloaded text file
(publish-file :path "/foo"
	      :file "examples/foo.txt"
	      :content-type "text/plain"
	      :preload t)

(publish-file :path "/foo.txt"
	      :file "examples/foo.txt"
	      :content-type "text/plain"
	      :preload nil)

;; an example which causes the web browser to put up the
;; name/password box and if you enter the name "foo" and password "bar"
;; then you get access to the secret info.
(publish :path "/secret"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((auth-val (header-slot-value req "authorization")))
	       (if* (and (stringp auth-val)
			 (equal (base64-decode 
				 (cadr (split-into-words auth-val)))
				"foo:bar"))
		  then (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html (:head (:title "Secret page"))
				 (:body "You made it to the secret page"))))
		  else
		       (with-http-response (req ent :response *response-unauthorized*)
			 (with-http-body (req ent
					      :headers 
					      '(("WWW-Authenticate" 
						 . "Basic realm=\"secretserver\"")))))))))




;; these two urls show how to transfer a user-selected file from
;; the client browser to the server.
;; 
;; We use two urls (/getfile to put up the form and /getfile-post to
;; handle the post action of the form).   We could have done it all
;; with one url but since there's a lot of code it helps in the
;; presentation to separate the two.
;;
(publish :path "/getfile"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head "get file")
		       (:body
			((:form :enctype "multipart/form-data"
				:method "post"
				:action "/getfile-got")
			 "Let me know what file to grab"
			 :br
			 ((:input :type "file" 
				  :name "thefile"
				  :value "*.txt"))
			 :br
			 ((:input :type "text" :name "textthing"))
			 :br
			 ((:input :type "checkbox" :name "checkone"))
			 "check box one"
			 :br
			 ((:input :type "checkbox" :name "checktwo"))
			 "check box two"
			 :br
			 ((:input :type "submit")))))))))

;; this called with the file from 
(publish :path "/getfile-got"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     
	     (with-http-response (req ent)
	       (let ((h nil)
		     (counter 0)
		     (files-written)
		     )
		 (loop
		   ; get headers for the next item
		   (if* (null (setq h (get-multipart-header req)))
		      then ; no more items
			   (return))
		   (format t "parsed headers: ~s~%" h)
		   ; we can get the filename from the header if 
		   ; it was an <input type="file"> item.  If there is
		   ; no filename, we just create one.
		   (let ((cd (assoc "content-disposition" h :test #'equalp))
			 (filename)
			 (sep))
		     (if* (and cd (consp (cadr cd)))
			then (setq filename (cdr (assoc "filename" 
							(cddr (cadr cd))
							:test #'equalp)))
			     (if* filename
				then ;; locate the part of the filename
				     ;; after the last directory separator.
				     ;; the common lisp pathname functions are
				     ;; no help since the filename syntax
				     ;; may be foreign to the OS on which
				     ;; the server is running.
				     (setq sep
				       (max (or (position #\/ filename
							  :from-end t) -1)
					    (or (position #\\ filename
							  :from-end t) -1)))
				     (setq filename
				       (subseq filename (1+ sep) 
					       (length filename)))))
		     (if* (null filename)
			then (setq filename (format nil "tempfile~d"
						    (incf counter))))
		     
		     (push filename files-written)
		     (with-open-file (pp filename :direction :output
				      :if-exists :supersede
				      :element-type '(unsigned-byte 8))
		       (format t "writing file ~s~%" filename)
		       (let ((buffer (make-array 1024
						 :element-type '(unsigned-byte 8))))
			 
			 (loop (let ((count (get-multipart-sequence 
					     req 
					     buffer
					     :raw t)))
				 (if* (null count) then (return))
				 (write-sequence buffer pp :end count)))))
		
		     ))
	       
	       
		 ;; now send back a response for the browser
	       
		 (with-http-body (req ent)
		   (html (:html (:head (:title "form example"))
				(:body "proceessed the form, files written"
				       (dolist (file (nreverse files-written))
					 (html :br "file: "
					       (:b (:prin1-safe file))))))))))))

	     

(publish :path "/cookietest"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (set-cookie-header req 
				  :name "froba" 
				  :value "vala"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req
				  :name "the time"
				  :value (net.iserve::universal-time-to-date
					  (get-universal-time))
				  :path "/cookieverify"
				  :expires (+ (get-universal-time)
					      (* 20 60) ; 20 mins
					      )
				  )
				  
	       (with-http-body (req ent)
		 (html (:head (:title "Cookie Test"))
		       (:body "you should have a cookie now."
			      " Go "
			      ((:a :href "/cookieverify") "here")
			      " to see if they were saved"))))))

(publish :path "/cookieverify"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((cookie-info (get-cookie-values req)))
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "Cookie results"))
			 (:body
			  "The following cookies were returned: " 
			  (:prin1-safe cookie-info))))))))
	 


(publish :path "/timeout"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     ;; do nothing interesting so that the timeout will
	     ;; occur
	     (with-http-response (req ent :timeout 15)
	       (loop (sleep 5)))))


;;;;;;  directory publishing.  These will only work on a particular
;; set of machines so you'll have to modify them to point to an
;; existing tree of pages on your machine if you want to see this work.

;; the franz home pagey
#+ignore (publish-directory :prefix "/"
		   :destination "/net/tanya/home/httpd/html/"
		   )


(publish-directory :prefix "/int"
		   :destination "/net/tanya/www/internal/htdocs/")




;; a separate world:

(defparameter *server2* (make-instance 'wserver))

(publish-directory :server *server2*
		   :prefix "/"
		   :destination "/home/httpd/html/")

