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
		  (:body (:b "Sample pages") :br
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
n					  :name "username"))))))))))

			      
				    

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
			 ((:input :type "file" 
				  :name "thefile"
				  :value "foo.txt"))
			  ((:input :type "submit")))))))))

;; this called with the file from 
(publish :url "/getfile-got"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     
	     (with-http-response (req ent)
	       (format t "mp headers: ~s~%"
		       (neo::get-multipart-header req))
	       (with-http-body (req ent)
		 (html "foo")))))

	     
	     
;;;;;;  directory publishing.  These will only work on a particular
;; set of machines so you'll have to modify them to point to an
;; existing tree of pages on your machine if you want to see this work.

;; the franz home page
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

