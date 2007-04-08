;; -*- mode: common-lisp; package: net.aserve.client -*-
;;
;; httpcopy.cl
;;
;; copyright (c) 2005-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
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
;; $Id: httpcopy.cl,v 1.1 2007/04/08 14:58:05 layer Exp $

(in-package :net.aserve.client)

(defun http-copy-file (url pathname
		       &rest args
		       &key (if-does-not-exist :error)
			    proxy
			    proxy-basic-authorization
			    (redirect 5)
			    (buffer-size 1024)
			    (headers nil)
			    (protocol :http/1.1)
			    (basic-authorization nil)
			    (progress-function nil)
			    (tmp-name-function
			     (lambda (pathname)
			       (format nil "~a.tmp" pathname)))
			    timeout
		       &aux (redirect-codes
			     '(#.(net.aserve::response-number
				  *response-found*)
			       #.(net.aserve::response-number
				  *response-moved-permanently*)
			       #.(net.aserve::response-number
				  *response-see-other*))))
  
  (ensure-directories-exist pathname)
  (let ((uri (net.uri:parse-uri url))
	(creq 
	 (make-http-client-request
	  url
	  :headers headers
	  :protocol protocol
	  :basic-authorization basic-authorization
	  :proxy proxy
	  :proxy-basic-authorization proxy-basic-authorization))
	(buf (make-array buffer-size :element-type '(unsigned-byte 8)))
	end
	code
	new-location
	s
	tmp-pathname
	(bytes-read 0)
	size
	temp
	progress-at)
    (unwind-protect
	(progn
	  (when progress-function
	    (multiple-value-bind (res code hdrs)
		(do-http-request url
		  :method :head
		  :proxy proxy
		  :proxy-basic-authorization proxy-basic-authorization
		  :headers headers
		  :protocol protocol
		  :basic-authorization basic-authorization)
	      (declare (ignore res))
	      (when (not (eql 200 code))
		(error "~a: code ~a" url code))
	      (handler-case
		  (setq size
		    (parse-integer
		     (or (setq temp
			   (cdr (assoc :content-length hdrs :test #'eq)))
			 (error "Cannot determine content length for ~a."
				url))))
		(error ()
		  (error "Cannot parse content-length: ~a." temp)))
	      
	      (do ((n 9 (1- n))
		   (size size))
		  ((= n 0))
		(push (truncate (* size (/ n 10))) progress-at))))
	  
	  (setq tmp-pathname (funcall tmp-name-function pathname))
	  (setq s (open tmp-pathname :direction :output
			;; bug16130: in case one was left laying around:
			:if-exists :supersede))

	  (loop
	    (read-client-response-headers creq)
	    ;; if it's a continue, then start the read again
	    (when (not (eql 100 (client-request-response-code creq)))
	      (return)))
	  
	  (when (and (member (client-request-response-code creq)
			     redirect-codes :test #'eq)
		     redirect
		     (if* (integerp redirect)
			then (> redirect 0)
			else t))	; unrestricted depth
	    (setq new-location
	      (cdr (assoc :location (client-request-headers creq)
			  :test #'eq))))
		
	  (loop
	    (if* (and timeout (numberp timeout))
	       then (let ((res (sys:with-timeout (timeout :timed-out)
				 (setq end
				   (client-request-read-sequence buf creq)))))
		      (when (eq :timed-out res)
			(error "~a is not responding."
			       (net.uri:uri-host uri))))
	       else (setq end (client-request-read-sequence buf creq)))
	    (when (zerop end)
	      (when progress-function (funcall progress-function -1 size))
	      (return)) ;; EOF
	    (when progress-at
	      (incf bytes-read buffer-size)
	      (when (> bytes-read (car progress-at))
		(setq progress-at (cdr progress-at))
		(ignore-errors (funcall progress-function bytes-read
					size))))
	    (write-sequence buf s :end end))
	    
	  (setq code (client-request-response-code creq))
	  
	  (if* new-location
	     then (client-request-close creq)
		  (close s)
		  (setq s nil)
		  ;; created above, 0 length
		  (delete-file tmp-pathname)
		  (setq new-location (net.uri:merge-uris new-location url))
		  (return-from http-copy-file
		    (apply #'http-copy-file new-location pathname
			   :redirect (if* (integerp redirect)
					then (1- redirect)
					else redirect)
			   args))
	   elseif (eql 404 code)
	     then (let ((fs "~a does not exist."))
		    (if* (eq :error if-does-not-exist)
		       then (error fs url)
		       else (warn fs url)
			    (return-from http-copy-file nil)))
	   elseif (not (eql 200 code))
	     then (error "Bad code from webserver: ~s." code))
	  
	  (close s)
	  (setq s nil)
	  (rename-file-raw tmp-pathname pathname))
      
      (when s
	;; An error occurred.
	(close s)
	(ignore-errors (delete-file tmp-pathname))
	(ignore-errors (delete-file pathname)))
      (client-request-close creq))
    t))
