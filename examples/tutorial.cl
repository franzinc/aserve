
(defpackage :tutorial 
  (:use :common-lisp :excl :net.iserve :net.html.generator))

(in-package :tutorial)


(publish :path "/hello"
	 :content-type "text/plain"
	 :function 
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (princ "Hello World!" *html-stream*)))))
		 
(publish :path "/hello2"
	 :content-type "text/html"
	 :function 
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html 
		  (:html (:head (:title "Hello World Test"))
			 (:body 
			  ((:font :color "red") "Hello ")
			  ((:font :color "blue") "World!"))))))))



(publish :path "/hello-count"
	 :content-type "text/html"
	 :function
	 (let ((count 0))
	   #'(lambda (req ent)
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html
		    (:html
		     (:head (:title "Hello Counter"))
		     (:body 
		      ((:font :color (nth (random 5)
					  '("red" "blue" 
					    "green" "purple" "black")))
		       "Hello World had been called " 
		       (:princ (incf count)) 
		       " times")))))))))


(publish :path "/queryform"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((name (cdr (assoc "name" (request-query req) 
				     :test #'equal))))
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (if* name
		      then ; form was filled out, just say it
			   (html (:html
				  (:head (:title "Hi to " (:princ-safe name)))
				  (:body "Your name is "
					 (:b (:princ-safe name)))))
		      else ; put up the form
			   (html (:html
				   (:head (:title "Tell me your name"))
				   (:body
				    ((:form :action "queryform")
				     "Your name is "
				     ((:input :type "text"
					      :name "name"
					      :maxlength "20"))))))))))))


(publish :path "/charcount"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let* ((body (get-request-body req))
		    (text (if* body
			       then (cdr (assoc "quotation"
				      (decode-form-urlencoded body)
				      :test #'equal)))))
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (if* text
		      then ; got the quotation, analyze it
			   (html 
			    (:html
			     (:head (:title "Character Counts")
				    (:body 
				     (:table
				     (do ((i #.(char-code #\a) (1+ i)))
					 ((> i #.(char-code #\z)))
				       (html (:tr
					      (:td (:princ (code-char i)))
					      (:td (:princ 
						    (count (code-char i)
							   text)))))))))))
		      else ; ask for quotation
			   (html
			    (:html
			     (:head (:title "quote character counter")
				    (:body 
				     ((:form :action "charcount"
					     :method "POST")
				      "Enter your favorite quote "
				      :br
				      ((:textarea
					:name "quotation"
					:rows 30
					:cols 50))
				      :br
				      ((:input :type "submit"
					       :name "submit"
					       :value "count it")))))))))))))

				       
			    
			   
	       
					
				      

		       
