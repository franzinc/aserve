;; examples of web pages
(defpackage :neoe ;; neo example
  (:use :common-lisp :excl :htmlgen :neo))

(in-package :neo)

(unpublish :all t)

#+ignore (publish :url "/" 
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
			 ))))))
			     

	     

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
		       



(publish-file :url "/pic" :file "prfile9.jpg"
	      :content-type "image/jpeg")



(publish-file :url "/sampx" :file "c:/acl/cl/doc/newfspec.htm"
	      :content-type "text/html")




(publish :url "/tform" 
	 :content-type "text/html"
	 :function
	 (let ((name "unknown"))
	   #'(lambda (req ent)
	       (setq *rr* req)
	       (let ((gotname (assoc "username"
				     (decode-form-urlencoded 
				      (neo::args req))
				     :test #'equal)))
		 (if* gotname
		    then (setq name (cdr gotname))))
		 
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "test form"))
			 (:body "Hello " (:princ-safe name) ", "
				"Enter your name: "
				((:form :action "/tform"
					:method "get")
				 ((:input :type "text"
					  :maxlength 10
					  :size 10
					  :name "username"))))))))))

			      
				    

(publish 
 :url "/apropos"
 :content-type "text/html"
 :function
 #'(lambda (req ent)
     (let ((lookup (assoc "symbol"
			  (decode-form-urlencoded
			   (neo::args req))
			  :test #'equal)))
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

				  
(publish-file :url "/neoweb/fresh.jpg"
	      :file "fresh.jpg"
	      :content-type "image/jpeg"
	      :preload t)

(publish-file :url "/foo"
	      :file "foo.txt"
	      :content-type "text/plain"
	      :preload t)

(publish-file :url "/foo.txt"
	      :file "foo.txt"
	      :content-type "text/plain"
	      :preload nil)



;; the franz home page
#+ignore (publish-directory :prefix "/"
		   :destination "/net/tanya/home/httpd/html/"
		   )

(publish-directory :prefix "/"
		   :destination "/net/tanya/www/internal/htdocs/")




;;;;;;; examples of where I'd like neo to go
