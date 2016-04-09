;; -*- mode: common-lisp; package: net.aserve.examples -*-
;;
;; examples.cl
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
;;   Allegro iServe examples

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



;; examples of web pages
(defpackage :net.aserve.examples ;; aserve example
  (:use :common-lisp :excl :net.html.generator :net.aserve))

(in-package :net.aserve.examples)

;; don't flush all publishing done so far. since we have other
;; example files this is bad news.
; (unpublish :all t)

(defparameter *example-pathname* *load-pathname*) ; where this file is
(defmacro example-file (name)
    ;; create an absolute address for this file we'll load
    `(merge-pathnames ,name *example-pathname*))

(defvar *hit-counter* 0)


(publish :path "/" 
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     ;(print (net.aserve::compute-request-headers req))
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (format t "html stream is ~s~%" net.aserve::*html-stream*)
		 (html
		  (:head (:title "Welcome to AllegroServe"))
		  (:body (:center ((:img :src "aservelogo.gif")))
			 (:h1 "Welcome to AllegroServe") 
			 (:p "These links show off some of AllegroServe's capabilities. ")
			 (:i "This server's host name is "
			     (:princ-safe (header-slot-value req :host)))
			 #+unix
			 (:i ", the process id is "
			     (:princ (net.aserve::getpid)))
			 :br
			 (:princ (incf *hit-counter*)) " hits"
			 :p
			 (:b "Sample pages") :br
			 ((:a :href "gc") "Garbage Collector Stats") :br
			 ((:a :href "apropos") "Apropos using get")
			 :br
			 ((:a :href "apropos-post") "Apropos using post")
			 :br
			 ((:a :href "pic") "Sample jpeg") :br
			 ((:a :href "pic-redirect") "Redirect to previous picture") :br
			 ((:a :href "pic-gen") "generated jpeg") "- hit reload to switch images" :br
			 ((:a :href "pic-multi") "test of publish-multi") " - click more than once on this link" :br
			 ((:a :href "cookietest") "test cookies") :br
			 ((:a :href "secret") "Test manual authorization")
			 " (name: " (:b "foo") ", password: " (:b "bar") ")"
			 :br
			 ((:a :href "secret-auth") "Test automatic authorization")
			 " (name: "
			 (:b "foo2")
			 " password: "
			 (:b "bar2") ")"
			 :br
			 ((:a :href "local-secret") "Test source based authorization") " This will only work if you can use "
			 "http:://localhost ... to reach this page" :
			 :br
			 ((:a :href "local-secret-auth") 
			  "Like the preceding but uses authorizer objects")
			 :br
			 ((:a :href "timeout") "Test timeout")
			 "  this will take a while to time out."
			 :br
			 ((:a :href "getfile-old") "Client to server file transfer") " - the old way"
			 :br
			 ((:a :href "getfile") "Client to server file transfer") " - the new way, with 1,000,000 byte file transfer limit"
			 :br
			 ((:a :href "missing-link") "Missing Link")
			 " should get an error when clicked"
			 :br
			 ((:a :href "computed-error") "Computed error")
			 " demonstrate handlin error in response computation"
						      
			 
			 :br
			 #+unix
			 (html
			  ((:a :href "long-slow") "long, slow, cpu-bound")
			  " action to demonstrate how AllegroServe "
			  "in multiple Unix process mode can be responsive"
			  " even if one AllegroServe process is wedged."
			  " You probably do "
			  (:b "not")
			  " want to click on this link if you are running"
			  " AllegroServe is its normal single Unix process"
			  " mode.")
			  
			 
			 :br
			 ;; run only in an international lisp.
			 ;; test at runtime since we may switch back
			 ;; and forth between international and 8 bit
			 ;; modes
			 (if* (member :ics *features* :test #'eq)
			    then (html
				  :br
				  ((:a :href "ichars")
				   "International Character Display")

				  :br
				  ((:a :href "icharcount")
				   "(International) Character Counter")
				  :br
				  ;; published in puzzle.cl
				  ((:a :href "wordpuzzle")
				   "Word Puzzle")
				  :br
				  ;; published in urian.cl
				  ((:a :href "urian")
				   "International Web Page Character Finder")
				  :br
				  ;; published in locale.cl
				  ((:a :href "locale")
				   "Locale Demo")
				  :br
				  ))
			 
			 #+(and unix (version>= 6 1))
			 (html
			  "cgi tests: " 
			  ((:a :href "cgi0") "show environment")
			  ", "
			  ((:a :href "cgi1") "handle unix-style headers")
			  ", "
			  ((:a :href "cgi2") "redirect")
			  ", "
			  ((:a :href "cgi3") "set status to unauthorized request"))
			 :hr
			 ((:img :src "aservepowered.gif")) " <-- feel free to use this image on your AllegroServe-based web site"
			 ))))))
			     


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
(publish-file :path "/pic" :file (example-file "prfile9.jpg")
	      :content-type "image/jpeg")



(publish-file :path "/aservelogo.gif" :file (example-file "aservelogo.gif")
	      :content-type "image/gif")

(publish-file :path "/aservepowered.gif" :file (example-file "aservepowered.gif")
	      :content-type "image/gif")

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
	       (with-http-response (req ent :format :binary)
		 (with-http-body (req ent)
		   ; here is where you would generate the picture.
		   ; we're just reading it from a file in this example
		   (let ((stream (request-reply-stream req)))
		     (with-open-file (p (nth selector
					     `(,(example-file "prfile9.jpg")
					       ,(example-file "fresh.jpg")))
				      :element-type '(unsigned-byte 8))

		       (setq selector (mod (1+ selector) 2))
		     
		       (loop
			 (let ((val (read-byte p nil nil)))
			   (if* (null val) 
			      then ;eof 
				   (return))
			   (write-byte val stream)
			   )))))))))
	 


;; do a redirect to the picture

(publish :path "/pic-redirect"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent
				      :response *response-moved-permanently*)
	       (setf (reply-header-slot-value req :location) "pic")
	       (with-http-body (req ent)
		 ;; this is optional and most likely unnecessary since most 
		 ;; browsers understand the redirect response
		 (html 
		  (:html
		   (:head (:title "Object Moved"))
		   (:body 
		    (:h1 "Object Moved")
		    "The picture you're looking for is now at "
		    ((:a :href "pic") "This location"))))))))
		    
		    

;; this publish-multi example is simple but really doesn't show
;; the full power of publish-multi.
;; It doesn't show that we can include the contents of files
;; The :function case doesn't make use of the old cached value to
;; decide if it wants to return the old value or create a new one.
(publish-multi :path "/pic-multi"
	       :content-type "text/html"
	       :items  (list 
			'(:string "<html><body>The first line is constant<br>")
			(let (last-clicked)
			  #'(lambda (req ent old-time cached-value)
			      (declare (ignore req ent old-time cached-value))
			      (if* (null last-clicked)
				 then (setq last-clicked 
					(get-universal-time))
				      "this is your <b>first</b> click<br>"
				 else (let* ((new (get-universal-time))
					    (diff (- new last-clicked)))
					(setq last-clicked new)
					(format nil "~d seconds since the last click<br>" diff)))))
			'(:string "The last line is constant</body></html>")))

					       
					 

					 




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
				       (form-urlencoded-to-query body)
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
     (let ((lookup (assoc "symbol" (request-query req) :test #'equal)))
       (with-http-response (req ent)
	 (with-http-body (req ent)
	   (html (:head (:title "Allegro Apropos"))
		 ((:body :background "aserveweb/fresh.jpg")
		  "New Apropos of "
		  ((:form :action "apropos"
			  :method "get")
		   ((:input :type "text"
			    :maxlength 40
			    :size 20
			    :name "symbol")))
		  #+allegro
		  " The apropos function in ACL is case sensitive."
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

(publish 
 :path "/apropos-post"
 :content-type "text/html"
 :function
 #'(lambda (req ent)
     (let ((lookup (assoc "symbol" (request-query req) :test #'equal)))
       (with-http-response (req ent)
	 (with-http-body (req ent)
	   (html (:head (:title "Allegro Apropos"))
		 ((:body :background "aserveweb/fresh.jpg")
		  "New Apropos of "
		  ((:form :action "apropos-post"
			  :method "post")
		   ((:input :type "text"
			    :maxlength 40
			    :size 20
			    :name "symbol")))
		  #+allegro
		  " The apropos function in ACL is case sensitive."
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
(publish-file :path "/aserveweb/fresh.jpg"
	      :file (example-file "fresh.jpg")
	      :content-type "image/jpeg"
	      :preload t)

;; a preloaded text file
(publish-file :path "/foo"
	      :file (example-file "foo.txt")
	      :content-type "text/plain"
	      :preload t)

(publish-file :path "/foo.txt"
	      :file (example-file "foo.txt")
	      :content-type "text/plain"
	      :preload nil)


;; some entries for benchmarking
(publish-file :path "/file2000"
	      :file (example-file "file2000.txt")
	      :content-type "text/plain"
	      :preload nil)

(publish-file :path "/file2000-preload"
	      :file (example-file "file2000.txt")
	      :content-type "text/plain"
	      :preload t)

(publish :path "/dynamic-page"
	 :content-type "text/plain"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "This is a dynamic page")))))

;; an example which causes the web browser to put up the
;; name/password box and if you enter the name "foo" and password "bar"
;; then you get access to the secret info.
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
			 (with-http-body (req ent)
			   (html (:h1 "You Failed")
				 "You failed to enter the correct name/password")
			   ))))))


(publish :path "/local-secret"
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
		  else
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html
			    (:html (:head (:title "Unauthorized"))
				   (:body 
				    "You cannot access this page "
				    "from your location")))))))))


(publish :path "/local-secret-auth"
	 :content-type "text/html"
	 :authorizer (make-instance 'location-authorizer
		       :patterns '((:accept "127.0" 8)
				   :deny))
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head (:title "Secret page"))
		       (:body (:b "Congratulations. ")
			      "You made it to the secret page"))))))

;; these two urls show how to transfer a user-selected file from
;; the client browser to the server.
;; 
;; We use two urls (/getfile to put up the form and /getfile-post to
;; handle the post action of the form).   We could have done it all
;; with one url but since there's a lot of code it helps in the
;; presentation to separate the two.
;;
(publish :path "/getfile-old"
	 :content-type "text/html; charset=utf-8"
	 :function #'(lambda (req ent) (getfile-function 
					req ent "/getfile-got-old")))

(publish :path "/getfile"
	 :content-type "text/html; charset=utf-8"
	 :function #'(lambda (req ent) (getfile-function 
					req ent "/getfile-got")))


(defun getfile-function (req ent posturl)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html (:head "get file")
	    (:body
	     ((:form :enctype "multipart/form-data"
		     :method "post"
		     :action posturl)
	      "Let me know what file to grab"
	      :br
	      ((:input :type "file" 
		       :name "thefile"
		       :value "*.txt"))
	      :br
	      ((:input :type "text" :name "textthing"))
	      "Enter some text"
	      :br
	      ((:input :type "checkbox" :name "checkone"))
	      "check box one"
	      :br
	      ((:input :type "checkbox" :name "checktwo"))
	      "check box two"
	      :br
	      ((:input :type "submit"))))))))


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



;;
;; this demonstrates the use of the low level multipart access functions.
;; In this code we parse the result of get-multipart-header ourselves
;; and we use get-multipart-sequence.
;; In the example that follows (associate with path "/getfile-got")
;; we show now to use the higher level functions to retrive multipart
;; data
(publish :path "/getfile-got-old"
	 :content-type "text/html; charset=utf-8"
	 :function
	 #'(lambda (req ent)
	     
	     (with-http-response (req ent)
	       (let ((h nil)
		     (files-written)
		     (text-strings)
		     )
		 (loop
		   ; get headers for the next item
		   (if* (null (setq h (get-multipart-header req)))
		      then ; no more items
			   (return))
		   ; we can get the filename from the header if 
		   ; it was an <input type="file"> item.  If there is
		   ; no filename, we just create one.
		   (pprint h)
		   (pprint (multiple-value-list (parse-multipart-header h)))
		   (let ((cd (assoc :content-disposition h :test #'eq))
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
		     (if* (and filename (not (equal filename "")))
			then (push filename files-written)
			     (with-open-file (pp filename :direction :output
					      :if-exists :supersede
					      :element-type '(unsigned-byte 8))
			       (format t "writing file ~s~%" filename)
			       (let ((buffer (make-array 4096
							 :element-type 
							 '(unsigned-byte 8))))
			 
				 (loop (let ((count (get-multipart-sequence 
						     req 
						     buffer)))
					 (if* (null count) then (return))
					 (write-sequence buffer pp :end count)))))
		      elseif (null filename)
			then  ; no filename, just grab as a text
			     ; string
			     (let ((buffer (make-string 1024)))
			       (loop
				 (let ((count (get-multipart-sequence
					       req buffer
					       :external-format :utf8-base)))
				   (if* count
				      then (push (subseq buffer 0 count)
						 text-strings)
				      else (return))))))))
		 
	       
	       
		 ;; now send back a response for the browser
	       
		 (with-http-body (req ent
				      :external-format :utf8-base)
		   (html (:html (:head (:title "form example"))
				(:body "-- processed the form, files written --"
				       (dolist (file (nreverse files-written))
					 (html :br "file: "
					       (:b (:prin1-safe file))))
				       :br
				       "-- Non-file items Returned: -- " :br
				       (dolist (ts (reverse text-strings))
					 (html (:princ-safe ts) :br))))))))))


;;
;; this retrieves data from a multipart form using the high level
;; functions.  You can compare this code to that above to see which
;; method you prefer
;; 
(publish :path "/getfile-got"
	 :content-type "text/html; charset=utf-8"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (let ((files-written)
		     (text-strings)
		     (overlimit)
		     )
		 (loop
		   (multiple-value-bind (kind name filename content-type)
		       (parse-multipart-header
			(get-multipart-header req))
		     
		     (case kind
		       (:eof (return)) ; no more to read
		       (:data
			(push (cons name (get-all-multipart-data req))
			      text-strings))
		       (:file
			(let ((contents (get-all-multipart-data 
					 req 
					 :type :binary
					 :limit 1000000 ; abitrary limit
					 )))
			  ; find the tail of the filename, can't use
			  ; lisp pathname code since the filename syntax
			  ; may not correspond to this lisp's native os
			  (let ((sep (max (or (position #\/ filename
							:from-end t) -1)
					  (or (position #\\ filename
							:from-end t) -1))))
			    (if* sep
			       then (setq filename 
				      (subseq filename (1+ sep)))))
			  (if* (eq contents :limit)
			     then ; tried to give us too much
				  (setq overlimit t)
			   elseif (equal filename "") ; no file given
			     thenret ; ignore
			     else
				  (with-open-file (p filename 
						   :direction :output
						   :if-exists :supersede
						   :element-type '(unsigned-byte 8))
				    (format 
				     t "writing file ~s, content-type ~s~%"
				     filename content-type)
				    (push filename files-written)
				    (write-sequence contents p)))))
		       (t ; all else ignore but read to next header
			(get-all-multipart-data req :limit 1000)))))
			  

	       
	       
		 ;; now send back a response for the browser
	       
		 (with-http-body (req ent
				      :external-format :utf8-base)
		   (html (:html (:head (:title "form example"))
				(:body "-- processed the form, files written --"
				       (dolist (file (nreverse files-written))
					 (html :br "file: "
					       (:b (:prin1-safe file))))
				       (if* overlimit
					  then (html :br
						     "File given was over our "
						     "limit in the size we "
						     "will accept"))
				       :br
				       "-- Non-file items Returned: -- " :br
				       (dolist (ts (reverse text-strings))
					 (html 
					  "item name: " (:princ-safe (car ts))
					  ", item value: " 
					  (:princ-safe (cdr ts)) 
					  :br))))))))))

	     

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
				  :name "frob2" 
				  :value "val2"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req 
				  :name "frob3-loooooooooooooong" 
				  :value "val3-loooooooooooooong"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req
				  :name "the time"
				  :value (net.aserve::universal-time-to-date
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
			      ((:a :href "cookieverify") "here")
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



(publish :path "/long-slow"
	 :content-type "text/plain"
	 :function
	 #'(lambda (req ent)
	     ;; chew up cpu time in a look that blocks 
	     ;; the scheduler from running so this aserve
	     ;; won't accept any more connections and we can
	     ;; demo the multiple process version
	     ; takes 50 secs on a 1.2ghz Athlon
	     (locally (declare (optimize (speed 3) (safety 0)))
	       (dotimes (aa 500)
		 (declare (fixnum aa))
		 (dotimes (j 300)
		   (declare (fixnum j))
		   (dotimes (i 10000) 
		     (declare (fixnum i))
		     (let ((k (+ i j)))
		       (declare (fixnum k))
		       (setf k (- i j))
		       (setf k (+ i j k))
		       (setf k (- i j k)))))))
						
					     
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html "done")))))



(publish :path "/computed-error"
	 :content-type "text/plain"
	 :function
	 #'(lambda (req ent)
	     ;; this will get an error after the header is geneated
	     ;; but before the first body output is done
	     ;; this will test the delayed header send.
	     ;; the user should see a 500 "internal server error"
	     (handler-case
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     ; make an error
		     (let ((a (+ 1 2 3 :bogus)))
		       (+ a a))
		     (html "done")))
	       (error (c)
		 (with-http-response (req ent 
					  :response 
					  *response-internal-server-error*
					  :content-type
					  "text/html")
		   (with-http-body (req ent)
		     (html (:head (:title "Internal Server Error"))
			   (:body "As expected this entity caused error " 
				  (:princ c)))))))))
	       
	       

;; cgi publishing, we publish a shell script that only works
;; on Unix shells:
#+unix
(publish :path "/cgi0" :function
	 #'(lambda (req ent)
	     (net.aserve::run-cgi-program req ent 
					  "aserve/examples/cgitest.sh"
					  :env '(("HTTP_CONNECTION" 
						      . "hack replaced value")
						     ("NewHead" . "NewVal")))))

#+unix
(publish :path "/cgi1" :function
	 #'(lambda (req ent)
	     (net.aserve::run-cgi-program req ent "aserve/examples/cgitest.sh 1")))

#+unix
(publish :path "/cgi2" :function
	 #'(lambda (req ent)
	     (net.aserve::run-cgi-program req ent "aserve/examples/cgitest.sh 2")))

#+unix
(publish :path "/cgi3" :function
	 #'(lambda (req ent)
	     (net.aserve::run-cgi-program req ent "aserve/examples/cgitest.sh 3")))


;;;;;;  directory publishing.  These will only work on a particular
;; set of machines so you'll have to modify them to point to an
;; existing tree of pages on your machine if you want to see this work.

;; the franz home page
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

;;
;; International Characters
;;

(publish
 :path "/icharcount"
 :content-type "text/html; charset=utf-8"
 :function

 #-(and allegro ics (version>= 6 0))
 #'(lambda (req ent)
     (with-http-response (req ent)
       (with-http-body (req ent)
	 (princ #.(format nil "~
This page available only with International Allegro CL post 6.0 beta")
		*html-stream*))))

 #+(and allegro ics (version>= 6 0))
 #'(lambda (req ent)
     (let* ((body (get-request-body req))
	    (text (if* body
		     then (cdr (assoc "quotation"
				      (form-urlencoded-to-query
				       body
				       :external-format :utf8-base)
				      :test #'equal)))))

       (with-http-response (req ent)
	 (with-http-body (req ent
			      :external-format :utf8-base)
	   (if* text
	      then ;; got the quotation, analyze it
		   (let ((results (analyze-text text)))
		     (html (:html (:head
				   (:title "Character Counts"))
				  (:body
				   (html (:pre (:princ-safe text)))
				   (:p "Quote by Character Names:")
				   (:table
				    (dotimes (i (length text))
				      (html (:tr
					     (:td (:princ (schar text i)))
					     (:td (:prin1 (schar text i)))))))
				   (:p "Sorted by occurrence:")
				   ((:table :border 1)
				    (dolist (r results)
				      (html (:tr
					     (:td
					      (:princ
					       (format nil "u+~4,'0x"
						       (char-code (car r)))))
					     (:td (:princ (car r)))
					     (:td (:prin1 (car r)))
					     (:td (:princ (cdr r)))))))))))
	      else ;; ask for quotation
		   (html (:html
			  (:head (:title "Character Counter"))
			  (:body
			   ((:form :action "icharcount"
				   :method "POST")
			    (:h1 "AllegroServe Demo")
			    (:p #.(format nil "~
Below are links containing international character samples you can use to copy
and paste into the following form.
Note that even characters that don't display (due to missing fonts) can still
be copied and pasted into the form below."))
			    (:ul (:li ((:a href #.(format nil "~
http://www.columbia.edu/kermit/utf8.html")
					   target "_blank")
				       "UTF-8 Sampler"))
				 (:li ((:a href #.(format nil "~
http://www.trigeminal.com/samples/provincial.html")
					   target "_blank")
				       #.(format nil "~
The \"anyone can be provincial!\" page"))))
			    "Enter your favorite quote:"
			    :br
			    ((:textarea :name "quotation" :rows 15
					:cols 50))
			    :br
			    ((:input :type "submit"
				     :value "count it"))))))))))))

(defun analyze-text (text)
  (let ((char-ht (make-hash-table))
	(results nil))
    (dotimes (i (length text))
      (let ((ch (schar text i)))
	(if* (gethash ch char-ht)
	   then (incf (gethash ch char-ht))
	   else (setf (gethash ch char-ht) 1))))
    (maphash #'(lambda (k v)
		 (push (cons k v) results))
	     char-ht)
    (sort results #'(lambda (x y) (> (cdr x) (cdr y))))))

(publish
 :path "/ichars"
 :content-type "text/html"
 :function

 #-(and allegro ics (version>= 6 0))
 #'(lambda (req ent)
     (with-http-response (req ent)
       (with-http-body (req ent)
	 (princ #.(format nil "~
This page available only with International Allegro CL post 6.0")
		*html-stream*))))

 ;; Need pre-final.1's :try-variant change to find-external-format
 #+(and allegro ics (version>= 6 0))
 #'(lambda (req ent)
     (let* ((body (get-request-body req))
	    (query (if* body
		      then (form-urlencoded-to-query body)))
	    (lisp-ef (or (if* query
			    then (cdr (assoc "lisp-ef" query :test #'equal)))
			 ":utf8"))
	    (http-charset (or (if* query
				 then (cdr (assoc "http-charset" query
						  :test #'equal)))
			      "utf-8"))
	    (http-content-type (format nil "text/html; charset=~a"
				       http-charset)))

       (setq lisp-ef
	 (or (read-from-string lisp-ef)
	     :latin1-base))
       (with-http-response (req ent)
	 (with-http-body (req ent
			      :external-format (crlf-base-ef
						(find-external-format
						 lisp-ef
						 :try-variant t)))
	   (html
	    (:html
	     (:head (:title (:princ-safe
			     (format nil "Character Display: ~a / ~a"
				     lisp-ef http-charset)))
		    ((:meta http-equiv "content-type"
			    content http-content-type)))
	     (:body
	      ((:form :action "ichars" :method "POST")
	       "HTTP content-type:  " (:strong (:prin1 http-content-type))
	       :br
	       "with-http-body's external-format:  " (:strong (:prin1 lisp-ef))
	       :br
	       :br
	       "Note that the way characters are displayed depends upon "
	       "the browser's fonts, and how the browser interprets "
	       "the HTTP content-type."
	       :br
	       :br
	       (:center
		((:table :border 1
			 :cellpadding 2)
		 (:tr (:th "Charset") (:th "Lisp Character") (:th "Display"))
		 (:tr (:td "Latin-1"))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\a))
		      (:td (:princ #\a)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\b))
		      (:td (:princ #\b)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\c))
		      (:td (:princ #\c)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\cent_sign))
		      (:td (:princ #\cent_sign)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\pound_sign))
		      (:td (:princ #\pound_sign)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\latin_small_letter_thorn))
		      (:td (:princ #\latin_small_letter_thorn)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\latin_capital_letter_ae))
		      (:td (:princ #\latin_capital_letter_ae)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\latin_capital_letter_thorn))
		      (:td (:princ #\latin_capital_letter_thorn)))
		 (:tr (:td "Latin-1")
		      (:td (:prin1 #\latin_capital_letter_i_with_circumflex))
		      (:td (:princ #\latin_capital_letter_i_with_circumflex)))
		 (:tr (:td "Latin-2"))
		 (:tr (:td "Latin-2")
		      (:td (:prin1 #\latin_small_letter_u_with_ring_above))
		      (:td (:princ #\latin_small_letter_u_with_ring_above)))
		 (:tr (:td "Latin-2")
		      (:td (:prin1 #\latin_capital_letter_n_with_caron))
		      (:td (:princ #\latin_capital_letter_n_with_caron)))
		 (:tr (:td "Latin-2")
		      (:td (:prin1 #\latin_capital_letter_l_with_stroke))
		      (:td (:princ #\latin_capital_letter_l_with_stroke)))
		 (:tr (:td "Latin-3"))
		 (:tr (:td "Latin-3")
		      (:td (:prin1 #\latin_small_letter_j_with_circumflex))
		      (:td (:princ #\latin_small_letter_j_with_circumflex)))
		 (:tr (:td "Latin-3")
		      (:td (:prin1 #\latin_capital_letter_h_with_stroke))
		      (:td (:princ #\latin_capital_letter_h_with_stroke)))
		 (:tr (:td "Latin-3")
		      (:td (:prin1 #\latin_capital_letter_c_with_circumflex))
		      (:td (:princ #\latin_capital_letter_c_with_circumflex)))
		 (:tr (:td "Latin-4"))
		 (:tr (:td "Latin-4")
		      (:td (:prin1 #\latin_small_letter_u_with_ogonek))
		      (:td (:princ #\latin_small_letter_u_with_ogonek)))
		 (:tr (:td "Latin-4")
		      (:td (:prin1 #\latin_capital_letter_i_with_macron))
		      (:td (:princ #\latin_capital_letter_i_with_macron)))
		 (:tr (:td "Latin-4")
		      (:td (:prin1 #\latin_capital_letter_g_with_cedilla))
		      (:td (:princ #\latin_capital_letter_g_with_cedilla)))
		 (:tr (:td "Latin-5"))
		 (:tr (:td "Latin-5")
		      (:td (:prin1 #\cyrillic_capital_letter_ukrainian_ie))
		      (:td (:princ #\cyrillic_capital_letter_ukrainian_ie)))
		 (:tr (:td "Latin-5")
		      (:td (:prin1 #\cyrillic_small_letter_nje))
		      (:td (:princ #\cyrillic_small_letter_nje)))
		 (:tr (:td "Latin-5")
		      (:td (:prin1 #\cyrillic_capital_letter_ya))
		      (:td (:princ #\cyrillic_capital_letter_ya)))
		 (:tr (:td "Latin-6"))
		 (:tr (:td "Latin-6")
		      (:td (:prin1 #\arabic_letter_feh))
		      (:td (:princ #\arabic_letter_feh)))
		 (:tr (:td "Latin-6")
		      (:td (:prin1 #\arabic_letter_hah))
		      (:td (:princ #\arabic_letter_hah)))
		 (:tr (:td "Latin-6")
		      (:td (:prin1 #\arabic_letter_yeh_with_hamza_above))
		      (:td (:princ #\arabic_letter_yeh_with_hamza_above)))
		 (:tr (:td "Latin-7"))
		 (:tr (:td "Latin-7")
		      (:td (:prin1 #\greek_capital_letter_delta))
		      (:td (:princ #\greek_capital_letter_delta)))
		 (:tr (:td "Latin-7")
		      (:td (:prin1 #\greek_small_letter_eta))
		      (:td (:princ #\greek_small_letter_eta)))
		 (:tr (:td "Latin-7")
		      (:td (:prin1 #\greek_capital_letter_sigma))
		      (:td (:princ #\greek_capital_letter_sigma)))
		 (:tr (:td "Latin-8"))
		 (:tr (:td "Latin-8")
		      (:td (:prin1 #\hebrew_letter_alef))
		      (:td (:princ #\hebrew_letter_alef)))
		 (:tr (:td "Latin-8")
		      (:td (:prin1 #\hebrew_letter_bet))
		      (:td (:princ #\hebrew_letter_bet)))
		 (:tr (:td "Latin-8")
		      (:td (:prin1 #\hebrew_letter_gimel))
		      (:td (:princ #\hebrew_letter_gimel)))
		 (:tr (:td "Latin-15"))
		 (:tr (:td "Latin-15")
		      (:td (:prin1 #\latin_small_ligature_oe))
		      (:td (:princ #\latin_small_ligature_oe)))
		 (:tr (:td "Latin-15")
		      (:td (:prin1 #\latin_capital_ligature_oe))
		      (:td (:princ #\latin_capital_ligature_oe)))
		 (:tr (:td "Japanese"))
		 (:tr (:td "Japanese")
		      (:td (:prin1 #\hiragana_letter_a))
		      (:td (:princ #\hiragana_letter_a)))
		 (:tr (:td "Japanese")
		      (:td (:prin1 #\hiragana_letter_i))
		      (:td (:princ #\hiragana_letter_i)))
		 (:tr (:td "CJK"))
		 (:tr (:td "CJK")
		      (:td (:prin1 #\cjk_compatibility_ideograph-f900))
		      (:td (:princ #\cjk_compatibility_ideograph-f900)))
		 (:tr (:td "CJK")
		      (:td (:prin1 #\cjk_compatibility_ideograph-f901))
		      (:td (:princ #\cjk_compatibility_ideograph-f901)))
		 (:tr (:td "CJK")
		      (:td (:prin1 #\cjk_compatibility_ideograph-f902))
		      (:td (:princ #\cjk_compatibility_ideograph-f902)))
		 (:tr (:td "Ligature"))
		 (:tr (:td "Ligature")
		      (:td (:prin1 #\latin_small_ligature_fi))
		      (:td (:princ #\latin_small_ligature_fi)))
		 (:tr (:td "Ligature")
		      (:td (:prin1 #\latin_small_ligature_fl))
		      (:td (:princ #\latin_small_ligature_fl)))
		 ))
	       :br
	       :br
	       (:princ-safe (format nil "~
Switch Lisp External-Format (Current is ~s): "
				    (ef-name (find-external-format lisp-ef))))
	       ((:select name "lisp-ef")
		((:option value ":utf8-base" :selected "selected")
		 ":utf8-base")
		((:option value ":iso8859-1") ":iso8859-1")
		((:option value ":iso8859-2") ":iso8859-2")
		((:option value ":iso8859-3") ":iso8859-3")
		((:option value ":iso8859-4") ":iso8859-4")
		((:option value ":iso8859-5") ":iso8859-5")
		((:option value ":iso8859-6") ":iso8859-6")
		((:option value ":iso8859-7") ":iso8859-7")
		((:option value ":iso8859-8") ":iso8859-8")
		((:option value ":iso8859-15")":iso8859-15")
		((:option value ":shiftjis") ":shiftjis")
		((:option value ":euc") ":euc")
		((:option value ":932") ":932 (Windows 932)")
		((:option value ":1250") ":1250 (Windows 1250)")
		((:option value ":1254") ":1254 (Windows 1254)")
		((:option value ":1251") ":1251 (Windows 1251)")
		((:option value ":1255") ":1255 (Windows 1255)")
		)
	       :br
	       (:princ-safe (format nil "~
Switch HTTP Charset: (Current is ~s): "
				    http-charset))
	       ((:select name "http-charset")
		((:option value "utf-8" :selected "selected") "utf-8")
		((:option value "iso-8859-1") "iso-8859-1")
		((:option value "iso-8859-2") "iso-8859-2")
		((:option value "iso-8859-3") "iso-8859-3")
		((:option value "iso-8859-4") "iso-8859-4")
		((:option value "iso-8859-5") "iso-8859-5")
		((:option value "iso-8859-6") "iso-8859-6")
		((:option value "iso-8859-7") "iso-8859-7")
		((:option value "iso-8859-8") "iso-8859-8")
		((:option value "iso-8859-15") "iso-8859-15")
		((:option value "shift_jis") "shift_jis")
		((:option value "euc-jp") "euc-jp")
		((:option value "windows-932") "windows-932")
		((:option value "windows-1250")
		 "windows-1250")
		((:option value "windows-1254")
		 "windows-1254")
		((:option value "windows-1251")
		 "windows-1251")
		((:option value "windows-1255")
		 "windows-1255")
		)
	       :br
	       :br
	       ((:input :type "submit" :value "Redisplay")))))))
	 ))))
