;; examples of web pages
(defpackage :neoe ;; neo example
  (:use :common-lisp :excl :htmlgen :neo))

(in-package :neoe)

;; flush all publishing done so far:
(unpublish :all t)


(publish :url "/" 
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html
		  (:head (:title "Welcome to Neo"))
		  (:body 
		   (:i "This server's name is "
		    (:princ-safe (header-slot-value req "host")))
		   :p
		   (:b "Sample pages") :br
			 ((:a :href "/gc") "Garbage Collector Stats") :br
			 ((:a :href "/apropos") "Apropos") :br
			 ((:a :href "/pic") "Sample jpeg") :br
			 ((:a :href "/secret") "Test authorization")
 			 " (name: " (:b "foo") ", password: " (:b "bar") ")")
			 )))))
			     


;; a very simple page.  This is so simple it doesn't put out the required
;; tags (like <html>) yet I suspect that most browsers will display it
;; correctly regardless.
(publish :url "/hello"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "Hello World!")))))

;; this is the "/hello" example above modified to put out the correct
;; html tags around the page.
(publish :url "/hello2"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html 
			    (:html
			     (:body "Hello World!")))))))

;; display the current gc statistics.
(publish :url "/gc"
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
(publish-file :url "/pic" :file "prfile9.jpg"
	      :content-type "image/jpeg")





;;
;; here's a form using the 'post' method
;;
(publish :url "/tform" 
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
 :url "/apropos"
 :content-type "text/html"
 :function
 #'(lambda (req ent)
     (let ((lookup (assoc "symbol" (url-argument-alist req) :test #'equal)))
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
(publish-file :url "/neoweb/fresh.jpg"
	      :file "fresh.jpg"
	      :content-type "image/jpeg"
	      :preload t)

;; a preloaded text file
(publish-file :url "/foo"
	      :file "foo.txt"
	      :content-type "text/plain"
	      :preload t)

(publish-file :url "/foo.txt"
	      :file "foo.txt"
	      :content-type "text/plain"
	      :preload nil)

;; an example which causes the web browser to put up the
;; name/password box and if you enter the name "foo" and password "bar"
;; then you get access to the secret info.
(publish :url "/secret"
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
(publish :url "/getfile"
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
				  :value "foo.txt"))
			 :br
			 ((:input :type "text" :name "textthing"))
			 :br
			 ((:input :type "submit")))))))))

;; this called with the file from 
(publish :url "/getfile-got"
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
		   (if* (null (setq h (neo:get-multipart-header req)))
		      then ; no more items
			   (return))
		   
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
			 
			 (loop (let ((count (neo:get-multipart-sequence 
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

	     
	     
;;;;;;  directory publishing.  These will only work on a particular
;; set of machines so you'll have to modify them to point to an
;; existing tree of pages on your machine if you want to see this work.

;; the franz home pagey
#+ignore (publish-directory :prefix "/"
		   :destination "/net/tanya/home/httpd/html/"
		   )


(publish-directory :prefix "/int"
		   :destination "/net/tanya/www/internal/htdocs/")

#+ignore (publish-directory :prefix "/"
		   :destination "/home/alphapro/public_html/alphapro/")



;; a separate world:

(defparameter *server2* (make-instance 'wserver))

(publish-directory :server *server2*
		   :prefix "/"
		   :destination "/home/httpd/html/")

