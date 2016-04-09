;; -*- mode: common-lisp; package: net.aserve.client -*-
;;
;; client.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2013 Franz Inc, Oakland, CA - All rights reserved.
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
;;

;; Description:
;;   http client code.

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


;; this will evolve into the http client code but for now it's
;; just some simple stuff to allow us to test aserve
;;







(in-package :net.aserve.client)

(net.aserve::check-smp-consistency)

(defclass client-request ()
  ((uri   	;; uri we're accessing
    :initarg :uri
    :accessor client-request-uri)

   (method	; :get, :put, etc
    :initarg :method
    :accessor client-request-method)
   
   (headers ; alist of  ("headername" . "value") or (:headername . "value")
    :initform nil
    :initarg :headers
    :accessor client-request-headers)
   (response-code    ; response code (an integer)
    :initform nil
    :accessor client-request-response-code)
   (socket  ; the socket through which we'll talk to the server
    :initarg :socket
    :accessor client-request-socket)
   (response-stream ; the (possibly) chunking, inflating stream
    :initform nil
    :accessor client-request-response-stream)
   (protocol 
    ; the protocol value returned by the web server
    ; note, even if the request is for http/1.0, apache will return
    ; http/1.1.  I'm not sure this is kosher.
    :accessor client-request-protocol)
   (response-comment  ;; comment passed back with the response
    :accessor client-request-response-comment)
   ;
   (bytes-left  ;; indicates how many bytes in response left
    ; value is nil (no body)
    ;          integer (that many bytes left, not chunking)
    ;		:unknown - read until eof, not chunking
    ;		:chunking - read until chunking eof
    :accessor client-request-bytes-left
    :initform nil)
   
   (cookies  ;; optionally a cookie jar for hold received and sent cookies
    :accessor client-request-cookies
    :initarg :cookies
    :initform nil)
   
   (return-connection
    ;; set to
    ;;  :maybe - would like to return but only if no errors
    ;;  :yes   - return for sure
    :initarg :return-connection
    :accessor client-request-return-connection
    :initform nil)
   
   (content-encoding
    ;; if the content is itself encoded then it is one of
    ;; :gzip :deflate (but we don't support deflate yet)
    :initarg :content-encoding
    :initform nil
    :accessor client-request-content-encoding)
   
   (deferred-content
    ;; content we are delaying the transmission of due to
    ;; an Expect: 100-continue header.
    :initarg :deferred-content
    :initform nil
    :accessor client-request-deferred-content)
   ))


(defclass digest-authorization ()
  ((username :initarg :username
	     :initform ""
	     :reader digest-username)
   
   (password :initarg :password
	     :initform ""
	     :reader digest-password)
   
   (realm    :initform ""
	     :accessor digest-realm)
   
   (uri       :initform nil
	      :accessor digest-uri)
   
   (qop	     :initform nil
	     :accessor digest-qop)
   
   (nonce    :initform ""
	     :accessor digest-nonce)
   
   ; we sent unique cnonce each time
   (nonce-count :initform "1"
		:reader digest-nonce-count)

   (cnonce   :initform nil
	     :accessor digest-cnonce)
   
   (opaque   :initform nil
	     :accessor digest-opaque)
   
   (response :initform nil
	     :accessor digest-response)
   
   ))


(defvar crlf (make-array 2 :element-type 'character
			 :initial-contents '(#\return #\linefeed)))

(defmacro with-better-scan-macros (&body body)
  ;; define the macros for scanning characters in a string
  `(macrolet ((collect-to (ch buffer i max &optional downcasep eol-ok)
		;; return a string containing up to the given char
		`(let ((start ,i))
		   (loop
		     (if* (>= ,i ,max) 
			then ,(if* eol-ok
				 then `(return (buf-substr start ,i ,buffer ,downcasep))
				 else `(fail)))
		     (if* (eql ,ch (schar ,buffer ,i)) 
			then (return (buf-substr start ,i ,buffer ,downcasep)))
		     (incf ,i)
		     )))
	      
	      (collect-to-eol (buffer i max)
		;; return a string containing up to the given char
		`(let ((start ,i))
		   (loop
		     (if* (>= ,i ,max) 
			then (return (buf-substr start ,i ,buffer)))
		     (let ((thisch (schar ,buffer ,i)))
		       (if* (eq thisch #\return)
			  then (let ((ans (buf-substr start ,i ,buffer)))
				 (incf ,i)  ; skip to linefeed
				 (return ans))
			elseif (eq thisch #\linefeed)
			  then (return (buf-substr start ,i ,buffer))))
		     (incf ,i)
		     )))
	      
	      (skip-to-not (ch buffer i max &optional (errorp t))
		;; skip to first char not ch
		`(loop
		   (if* (>= ,i ,max) 
		      then ,(if* errorp 
			       then `(fail)
			       else `(return)))
		   (if* (not (eq ,ch (schar ,buffer ,i)))
		      then (return))
		   (incf ,i)))
	      
	      (buf-substr (from to buffer &optional downcasep)
		;; create a string containing [from to }
		;;
		`(let ((res (make-string (- ,to ,from))))
		   (do ((ii ,from (1+ ii))
			(ind 0 (1+ ind)))
		       ((>= ii ,to))
		     (setf (schar res ind)
		       ,(if* downcasep
			   then `(char-downcase (schar ,buffer ii))
			   else `(schar ,buffer ii))))
		   res)))
     
     ,@body))

(let ((bufsize 16384))
  (defun buffered-read-body (stream format)
    (flet ((buffer (size)
             (if* (eq format :text)
		then (make-string size)
		else (make-array size :element-type '(unsigned-byte 8)))))
      (let ((accum ()) (size 0) buffer read done)
        (loop (setf buffer (buffer bufsize)
                    read (handler-bind
                             ((excl::socket-chunking-end-of-file
                               (lambda (e)
                                 (declare (ignore e))
                                 (setf done t))))
                           (read-sequence buffer stream :partial-fill t)))
	  (push (cons buffer read) accum)
	  (incf size read)
	  (if* (or (zerop read) done) then (return)))
        (setf accum (nreverse accum))
        (let* ((bigbuf (buffer size))
               (pos 0))
	  (dolist (acc accum)
	    (destructuring-bind (sub . size) acc
	      (replace bigbuf sub :start1 pos :end2 size)
	      (incf pos size)))
          bigbuf)))))

(defun get-client-request-response-stream (creq)
  (or (client-request-response-stream creq)
      (let ((left (client-request-bytes-left creq))
            (socket (client-request-socket creq)))

        ; the data is coming in chunked then unchunk it
        (if* (eq :chunked left)
           then (setq socket
                      (make-instance 'net.aserve::unchunking-stream
                                     :input-handle socket)))
    
        ; if data is content-encoded, then decode
        (if* (client-request-content-encoding creq)
           then (setq socket (make-instance
				 'net.aserve::inflate-stream
			       :input-handle socket
			       ;; bug20472. :skip-gzip-header deprecated
			       ;; in code/inflate.003 patch for 8.2
			       ;; and will be gone in 9.0.
			       #-(version>= 9 0) :skip-gzip-header
			       #-(version>= 9 0) t
			       :content-length (and (integerp left) left)))
                (setf (client-request-bytes-left creq) :unknown))
    
        (setf (client-request-response-stream creq) socket))))
    
(defun read-response-body (creq &key (format :text))
  (let ((socket (get-client-request-response-stream creq))
        (left (client-request-bytes-left creq)))
	
    
    (setf (client-request-bytes-left creq) :eof)
    (if* (null left)
       then nil
     elseif (integerp left)
       then (let ((buffer (make-array left :element-type '(unsigned-byte 8))))
              (read-sequence buffer socket)
              (if* (eq format :text)
		 then (octets-to-string buffer :external-format
					(stream-external-format
					 (client-request-socket creq)))
		 else buffer)
	      
	       
	      )
     elseif (member left '(:chunked :unknown))
       then (prog1 (buffered-read-body socket format)
	      
	      )
     elseif (eq left :eof)
       then (error "Body already read."))))

      
;; bug22596
(defparameter *default-retry-on-timeout* nil)

(defun do-http-request (uri 
			&rest args
			&key 
			(method  :get)
			(protocol  :http/1.1)
			(accept "*/*")
			compress   ; say we'll willing to accept compressed response
			content
			content-type
			query
			(format :text) ; or :binary
			cookies ; nil or a cookie-jar
			(redirect 5) ; auto redirect if needed
			(redirect-methods '(:get :head))
			(retry-on-timeout *default-retry-on-timeout*)
			basic-authorization ; (name . password)
			digest-authorization ; digest-authorization object
			(keep-alive (eq method :connect))  ; if true, set con to keep alive
			headers	    ; extra header lines, alist
			proxy	    ; naming proxy server to access through
			proxy-basic-authorization  ; (name . password)
			no-proxy    ; list of domains to not proxy
			user-agent
			(external-format *default-aserve-external-format*)
			ssl		; do an ssl connection
			ssl-args	; plist of make-ssl-client-stream args
			; overrides other ssl args when specified
			skip-body ; fcn of request object
			ssl-method
			timeout
			certificate
			key
			certificate-password
			ca-file
			ca-directory
                        crl-file
                        crl-check
			verify
			max-depth
			connection ; existing socket to the server
			read-body-hook
			
			;; internal
			recursing-call ; true if we are calling ourself
			)
  
  ;; send an http request and return the result as four values:
  ;; the body, the response code, the headers and the uri 
  (let ((creq (make-http-client-request 
	       uri  
	       :method method
	       :protocol protocol
	       :accept  accept
	       :compress compress
	       :content content
	       :content-type content-type
	       :query query
	       :cookies cookies
	       :basic-authorization basic-authorization
	       :digest-authorization digest-authorization
	       :keep-alive keep-alive
	       :headers headers
	       :proxy proxy
	       :no-proxy no-proxy
	       :proxy-basic-authorization proxy-basic-authorization
	       :user-agent user-agent
	       :external-format external-format
	       :ssl ssl
	       :ssl-args ssl-args
	       :ssl-method ssl-method
	       :timeout timeout
	       :certificate certificate
	       :key key
	       :certificate-password certificate-password
	       :ca-file ca-file
	       :ca-directory ca-directory
               :crl-file crl-file
               :crl-check crl-check
	       :verify verify
	       :max-depth max-depth
	       :connection connection
	       )))

    (unwind-protect
	(let (new-location) 

          (net.aserve::maybe-accumulate-log (:xmit-client-response-headers "~s")
					    (loop
					      (if* (catch 'premature-eof
						     (read-client-response-headers creq
										   :throw-on-eof
										   (and connection
											'premature-eof))
						     ;; if it's a continue, then deliver any content we're holding
						     ;; onto and start the read again
						     (if* (not (eql 100 (client-request-response-code creq)))
							then (return)
							else (let ((sock (client-request-socket creq)))
							       (send-request-body sock (client-request-deferred-content creq))
							       (force-output sock)))
		   
						     nil)
		    
						 then ; got eof .. likely due to bogus
						      ; saved connection... so try again with
						      ; no saved connection
						      (ignore-errors (close connection))
						      (setf (getf args :connection) nil)
						      (return-from do-http-request
							(apply 'do-http-request uri args)))))
	  
	  (if* (equal "close" (cdr (assoc :connection 
					  (client-request-headers creq))))
	     then ; server forced the close
		  ; so don't return the connection
		  (setf (client-request-return-connection creq) nil))
	  
		  
	  (if* (and (member (client-request-response-code creq)
			    '(#.(net.aserve::response-number *response-found*)
			      #.(net.aserve::response-number *response-moved-permanently*)
			      #.(net.aserve::response-number *response-temporary-redirect*)
			      #.(net.aserve::response-number *response-see-other*))
			    :test #'eq)
		    redirect
		    (member method redirect-methods :test #'eq)
		    (if* (integerp redirect)
		       then (> redirect 0)
		       else t))		; unrestricted depth
	     then
		  (setq new-location
		    (cdr (assoc :location (client-request-headers creq)
				:test #'eq))))
	
	  (if* (and digest-authorization
		    (equal (client-request-response-code creq)
			   #.(net.aserve::response-number 
			      *response-unauthorized*))
		    (not recursing-call))
	     then ; compute digest info and retry
		  (if* (compute-digest-authorization 
			creq digest-authorization)
		     then (client-request-close creq)
			  (return-from do-http-request
			    (apply #'do-http-request
				   uri
				   :recursing-call t
				   args))))
		  
	  ;; auto-retry if request times out.
	  (if* (and (eq (client-request-response-code creq)
			#.(net.aserve::response-number *response-request-timeout*))
		    (if (integerp retry-on-timeout)
			(> retry-on-timeout 0)
		      retry-on-timeout))
	     then (net.aserve::debug-format :info "Received 408 response. Retrying request because retry-on-timeout is ~a.." retry-on-timeout)
		  (return-from do-http-request
		    (apply #'do-http-request
			   uri
			   :retry-on-timeout
			   (if* (integerp retry-on-timeout)
			      then (1- retry-on-timeout)
			      else retry-on-timeout)
			   :recursing-call t
			   args)))

	  (if* (or (and (null new-location) 
			; not called when redirecting
			(if* (functionp skip-body)
			   then (funcall skip-body creq)
			   else skip-body))
		   (member (client-request-response-code creq)
			   ' (#.(net.aserve::response-number 
				 *response-no-content*)
				#.(net.aserve::response-number 
				   *response-not-modified*)
				))
		   (and (eq method :connect)
			(eq (client-request-response-code creq)
			    #.(net.aserve::response-number *response-ok*))))
	     then
		  (return-from do-http-request
		    (values 
		     nil		; no body
		     (client-request-response-code creq)
		     (client-request-headers  creq)
		     (client-request-uri creq)
		     (and (client-request-return-connection creq)
			  (setf (client-request-return-connection creq)
			    :yes)
			  (client-request-socket creq))
		     )))
	  
	  (if* read-body-hook
	     then (funcall read-body-hook creq :format format)
	     else (let ((body (read-response-body creq :format format)))
		    (net.aserve::debug-format :xmit-client-response-body "~s" body)
		    
		    
		    
		    (if* new-location
		       then			; must do a redirect to get to the real site
			    (client-request-close creq)
			    (apply #'do-http-request
				   (net.uri:merge-uris new-location uri)
				   :redirect
				   (if* (integerp redirect)
				      then (1- redirect)
				      else redirect)
				   args)
		       else (if* (client-request-return-connection creq)
			       then (setf (client-request-return-connection creq)
				      :yes)
			       else (client-request-close creq))
				    
			    (values 
			     body
			     (client-request-response-code creq)
			     (client-request-headers  creq)
			     (client-request-uri creq)
			     (client-request-socket creq)
			     )))))
      
      ;; protected form:
      (and (not read-body-hook) (client-request-close creq)))))

(defun convert-trailers (trailers)
  ;; trailers looks like (("Foo" . "foo value") ("Bar" . "bar value"))
  ;; need to change header names to keyword symbols in lower case
  (mapcar #'(lambda (trailer)
	      (cons (intern (string-downcase (car trailer)) (symbol-package :foo))
		    (cdr trailer)))
	  trailers))



(defun http-copy-file (url pathname &rest args &key 
					       proxy
					       proxy-basic-authorization
					       (headers nil)
					       (protocol :http/1.1)
					       (basic-authorization nil)
					       progress-function
		       &allow-other-keys)
  (ensure-directories-exist pathname)
  (let ((creq 
	 (make-http-client-request
	  url
	  :headers headers
	  :protocol protocol
	  :basic-authorization basic-authorization
	  :proxy proxy
	  :proxy-basic-authorization proxy-basic-authorization))
	progress-at)

    (if* progress-function
       then (let ((size (bytes-at-url url
				      :proxy proxy
				      :proxy-basic-authorization proxy-basic-authorization
				      :headers headers
				      :protocol protocol
				      :basic-authorization basic-authorization)))
	      (do ((n 9 (1- n))
		   (size size))
		  ((= n 0))
		(push (truncate (* size (/ n 10))) progress-at))))
    
    (net.aserve::maybe-accumulate-log 
     (:xmit-client-response-headers "~s")
     (loop
       (read-client-response-headers creq)
       ;; if it's a continue, then start the read again
       (if* (not (eql 100 (client-request-response-code creq)))
	  then (return))))

    (dolist (arg '(:proxy :proxy-basic-authorization :headers
		    :protocol :basic-authorization))
      (remf args arg))

    (apply #'http-copy-file-request creq pathname :progress-at progress-at args)))


(defun bytes-at-url (url &key
		     proxy
		     proxy-basic-authorization
		     (headers nil)
		     (protocol :http/1.1)
		     (basic-authorization nil))
  "Return the size of the URL using a HEAD request."
  (multiple-value-bind (res code hdrs)
      (do-http-request url
	:method :head
	:proxy proxy
	:proxy-basic-authorization proxy-basic-authorization
	:headers headers
	:protocol protocol
	:basic-authorization basic-authorization)
    (declare (ignore res))
    (let (size temp)
      (if* (not (eql 200 code))
	   then (error "~a: code ~a" url code))
      (handler-case
	  (setq size
		(parse-integer
		 (or (setq temp
			   (cdr (assoc :content-length hdrs :test #'eq)))
		     (error "Cannot determine content length for ~a."
			    url))))
	(error ()
	  (error "Cannot parse content-length: ~a." temp)))
      size)))

(defun http-copy-file-request (creq pathname
			       &rest args
			       &key (if-does-not-exist :error)
			       (redirect 5)
			       (buffer-size 1024)
			       (progress-function nil)
			       (tmp-name-function
				(lambda (pathname)
				  (format nil "~a.tmp" pathname)))
			       progress-at
			       size
			       timeout
			       &aux (redirect-codes
				     '(#.(net.aserve::response-number
					  *response-found*)
				       #.(net.aserve::response-number
					  *response-moved-permanently*)
				       #.(net.aserve::response-number
					  *response-see-other*))))
  "Copy the data from the CREQ into PATHNAME."
  (ensure-directories-exist pathname)
  (let* ((url (client-request-uri creq))
	 (uri (net.uri:parse-uri url))
	 (buf (make-array buffer-size :element-type '(unsigned-byte 8)))
	 end
	 code
	 new-location
	 s
	 tmp-pathname
	 (bytes-read 0))
    (unwind-protect
	(progn
	  (setq tmp-pathname (funcall tmp-name-function pathname))
	  (setq s (open tmp-pathname :direction :output
			;; bug16130: in case one was left laying around:
			:if-exists :supersede))

	  (if* (and (member (client-request-response-code creq)
			    redirect-codes :test #'eq)
		    redirect
		    (if* (integerp redirect)
		       then (> redirect 0)
		       else t))	; unrestricted depth
	     then (setq new-location
		    (cdr (assoc :location (client-request-headers creq)
				:test #'eq))))
		
	  (net.aserve::maybe-accumulate-log (:xmit-client-response-body "~s")
            (loop
              (if* (and timeout (numberp timeout))
                 then (let ((res (sys:with-timeout (timeout :timed-out)
                                   (setq end
                                         (client-request-read-sequence buf creq)))))
                        (if* (eq :timed-out res)
                           then (error "~a is not responding."
                                       (net.uri:uri-host uri))))
                 else (setq end (client-request-read-sequence buf creq)))
              (if* (zerop end)
                 then (if* progress-function 
                         then (funcall progress-function -1 size))
                      (return)) ;; EOF
              (if* progress-at
                 then (incf bytes-read buffer-size)
                      (if* (> bytes-read (car progress-at))
                         then (setq progress-at (cdr progress-at))
                              (ignore-errors (funcall progress-function bytes-read
                                                                        size))))
              (write-sequence buf s :end end)))
	    
	  (setq code (client-request-response-code creq))
	  
	  (if* new-location
	     then (client-request-close creq)
		  (close s)
		  (setq s nil)
		  ;; created above, 0 length
		  (delete-file tmp-pathname)
		  (setq new-location (net.uri:merge-uris new-location url))
		  (return-from http-copy-file-request
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
			    (return-from http-copy-file-request nil)))
	   elseif (not (eql 200 code))
	     then (error "Bad code from webserver: ~s." code))
	  
	  (close s)
	  (setq s nil)
	  (rename-file-raw tmp-pathname pathname))
      
      (if* s
	 then ;; An error occurred.
	      (close s)
	      (ignore-errors (delete-file tmp-pathname))
	      (ignore-errors (delete-file pathname)))
      (client-request-close creq))
    t))





(defmacro with-socket-connect-timeout ((&key timeout host port)
				       &body body)
  ;;
  ;; to wrap around a call to make-socket
  ;;
  `(mp:with-timeout ((or ,timeout 99999999)
		     (error 'allegroserve-error
			    :action (format nil "connecting to a socket at ~s:~s"
					    ,host ,port)
			    :result (format nil "exceeded timeout of ~s seconds"
					    ,timeout)
			    :identifier :connect-timeout))
     ,@body))



(defmacro with-stream-closed-on-failure ((stream) &body body)
  (let ((%success (gensym)))
    `(let ((,%success nil))
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setq ,%success t))
         (when (and ,stream (not ,%success))
           (ignore-errors (close ,stream :abort t)))))))

(defun send-request-body (sock content)
  (if* content
     then ; content can be a vector or a list of vectors
	  (dolist (cont content)
	    (net.aserve::debug-format
	     :info "client sending content of ~d characters/bytes"
	     (length cont))
	    (net.aserve::debug-format
	     :xmit-client-request-body
	     "~s" (if (stringp cont)
		      cont
		    (octets-to-string cont :external-format :octets)))
	    (write-sequence cont sock))))

(defun make-http-client-request (uri &rest args
				 &key 
				 (method  :get)  ; :get, :post, ....
				 (protocol  :http/1.1)
				 keep-alive 
				 (accept "*/*") 
				 cookies  ; nil or a cookie-jar
				 basic-authorization
				 digest-authorization
				 compress
				 content
				 content-length 
				 content-type
				 query
				 headers
				 proxy
				 proxy-basic-authorization
				 no-proxy
				 user-agent
				 (external-format 
				  *default-aserve-external-format*)
				 ssl
				 ssl-args
				 ssl-method
				 timeout
				 certificate
				 key
				 certificate-password
				 ca-file
				 ca-directory
                                 crl-file
                                 crl-check
				 verify
				 max-depth
				 connection
				 use-socket
				     
				 )
  

  (declare (ignorable timeout certificate key certificate-password ca-file 
		      ca-directory crl-file crl-check verify max-depth ssl-method))
  
  
  (if* (and connection (not use-socket))
     then ; try using it
	  (handler-case (return-from make-http-client-request
			  (apply #'make-http-client-request 
				 uri
				 :use-socket connection
				 args))
	    (error (c)
	      (declare (ignore c))
	      (ignore-errors (close connection))
	      ; drop into code to do it normally
	      )))
  (let (host sock port fresh-uri scheme-default-port)
    ;; start a request
    (with-stream-closed-on-failure (sock)
      ;; CONNECT method requests do not require a uri
      ;; but do require a proxy host:port.
      (if* (eq method :connect)
         then (if* (not proxy)
                 then (error "A proxy argument must be supplied when making a connect request."))
         else ; parse the uri we're accessing
              (if* (not (typep uri 'net.uri:uri))
                 then (setq uri (net.uri:parse-uri uri)
                            fresh-uri t))
      
              ; make sure it's an http uri
              (case (or (net.uri:uri-scheme uri) :http)
                (:http nil)
                (:https (setq ssl t))
                (t (error "Can only do client access of http or https uri's, not ~s" uri)))
    
              ; make sure that there's a host
              (if* (null (setq host (net.uri:uri-host uri)))
                 then (error "need a host in the client request: ~s" uri))
  
              (setq scheme-default-port
                (case (or (net.uri:uri-scheme uri) (if* ssl 
                                                      then :https
                                                      else :http))
                  (:http 80)
                  (:https 443)))
      
              ; default the port to what's appropriate for http or https
              (setq port (or (net.uri:uri-port uri) scheme-default-port)))
      
      (if* proxy
	 then ; see if the no-proxy list disables
	      ; the proxy for this call
	      (if* (and no-proxy (atom no-proxy))
		 then ; in case just a single string given
		      (setq no-proxy (list no-proxy)))
	      
	      (dolist (nop no-proxy)
		(if* (stringp nop)
		   then (let ((pos (search nop host
					   :from-end t
					   :test #'char-equal)))
			  (if* (and pos
				    (eql pos
					 (- (length host)
					    (length nop))))
			     then ; match at the end
				  ; turn off proxy
				  (setq proxy nil)
				  (return))))))
				  
	      
      (if* proxy
         then ; sent request through a proxy server
              (assert (stringp proxy) (proxy) 
                "proxy value ~s should be a string" proxy)
              (multiple-value-bind (phost pport)
                  (net.aserve::get-host-port proxy)
                (if* (null phost)
                   then (error "proxy arg should have form \"foo.com\" ~
  or \"foo.com:8000\", not ~s" proxy))
                
                (setq sock 
                  (or use-socket
                      (with-socket-connect-timeout (:timeout timeout
                                                             :host phost 
                                                             :port pport)
                        (socket:make-socket :remote-host phost
                                            :remote-port pport
                                            :format :bivalent
                                            :type net.aserve::*socket-stream-type*
                                            :nodelay t
                                            )))))
       elseif use-socket
         then ; persistent connection
              (setq sock use-socket)
         else 
             
              (setq sock
                (with-socket-connect-timeout (:timeout timeout
                                                       :host host
                                                       :port port)
                  (socket:make-socket :remote-host host
                                      :remote-port port
                                      :format :bivalent
                                      :type 
                                      net.aserve::*socket-stream-type*
                                      :nodelay t
                                               
                                      )))
              (if* ssl
                 then #+(version>= 8 0)
		      (let ((args (or ssl-args
				      (append (list :certificate certificate
						    :key key
						    :certificate-password certificate-password
						    :ca-file ca-file
						    :ca-directory ca-directory
						    ;; These args are not available in 8.1. [bug23328] 
						    #+(version>= 8 2) :crl-file #+(version>= 8 2) crl-file
						    #+(version>= 8 2) :crl-check #+(version>= 8 2) crl-check
						    :verify verify
						    :max-depth max-depth)
					      (when ssl-method
						;; [bug23838] Called fn expect :method as the keyword.
						(list :method ssl-method))))))
			(setq sock
			  (apply 'socket::make-ssl-client-stream sock args)))
                      #-(version>= 8 0)
                      (setq sock
                        (funcall 'socket::make-ssl-client-stream sock))))
      
                      
  
      (if* (not use-socket)
         then ; a fresh socket, so set params
              #+(and allegro (version>= 6 0))
              (let ((ef (find-external-format external-format)))
                #+(version>= 6) (net.aserve::warn-if-crlf ef)
                (setf (stream-external-format sock) ef))
      
              (if* net.aserve::*watch-for-open-sockets*
                 then (schedule-finalization 
                       sock 
                       #'net.aserve::check-for-open-socket-before-gc))
      
              #+io-timeout
              (if* (integerp timeout)
                 then (socket:socket-control 
                       sock 
                       :read-timeout timeout
                       :write-timeout timeout)))
              
      
      (if* query
         then (if* (and (eq method :post) (not content))
                 then ;; bug22337. add query to the body and set the content-type
                      (setq content (query-to-form-urlencoded
                                     query :external-format external-format)
                            content-type "application/x-www-form-urlencoded")
                 else ; add info the uri
                      ; must not blast a uri we were passed
                      (if* (not fresh-uri)
                         then (setq uri (net.uri:copy-uri uri)))
                      (setf (net.uri:uri-query uri)
                            ;; rfe12963: append query to the query
                            ;; part instead of clobbering it.
                            (query-to-form-urlencoded
                             (append (form-urlencoded-to-query
                                      (net.uri:uri-query uri)
                                      :external-format external-format)
                                     query)
                             :external-format external-format))))

      (let ((command (format nil "~a ~a ~a"
                             (string-upcase (string method))
                             (if* (eq method :connect)
                                then ;; deliver 'uri' untouched
                                     uri
                                else (if* proxy
                                        then (net.uri:render-uri uri nil)
                                        else (uri-path-etc uri)))
                             (string-upcase (string protocol)))))
        (format sock "~a~a" command crlf)
        (net.aserve::debug-format :xmit-client-request-command "~s" command))
      
      ; write often to trigger error if connection closed
      (if* use-socket then (force-output sock))
  
      ; always send a Host header, required for http/1.1 and a good idea
      ; for http/1.0
      (net.aserve::maybe-accumulate-log (:xmit-client-request-headers "~s")
        (if*  (not (eql scheme-default-port  port))
           then (net.aserve::format-dif :xmit-client-request-headers
                                        sock "Host: ~a:~a~a" host port crlf)
           else (net.aserve::format-dif :xmit-client-request-headers
                                        sock "Host: ~a~a" host crlf))
        
        
        ; now the headers
        (if* (and keep-alive (eq protocol :http/1.0))
           then ; for http/1.1 keep alive is the default so no need 
              ; to express it
              (net.aserve::format-dif :xmit-client-request-headers
                                      sock "Connection: Keep-Alive~a" crlf)
         elseif (and (not keep-alive) (eq protocol :http/1.1))
           then ; request it close for us
              (net.aserve::format-dif :xmit-client-request-headers
                                      sock "Connection: close~a" crlf))
  
        
        (if* accept
           then (net.aserve::format-dif :xmit-client-request-headers
                                      sock "Accept: ~a~a" accept crlf))
  
  
        (if* compress
           then (net.aserve::format-dif :xmit-client-request-headers
                                      sock "Accept-Encoding: gzip~a"  crlf))
          
        ; some webservers (including AServe) have trouble with put/post
        ; requests without a body
        (if* (and (not content) (member method '(:put :post)))
           then (setf content ""))
        ; content can be a nil, a single vector or a list of vectors.
        ; canonicalize..
        (if* (and content (atom content)) then (setq content (list content)))
        
        (if* content
           then (let ((computed-length 0))
                (dolist (content-piece content)
                  (typecase content-piece
                    ((array character (*))
                     (if* (null content-length)
                        then (incf computed-length 
                                   (native-string-sizeof 
                                    content-piece
                                    :external-format external-format))))
                   
                    ((array (unsigned-byte 8) (*)) 
                     (if* (null content-length)
                        then (incf computed-length (length content-piece))))
                    (t (error "Illegal content array: ~s" content-piece))))
                
                (if* (null content-length)
                   then (setq content-length computed-length))))
        
              
        
        (if* content-length
           then (net.aserve::format-dif :xmit-client-request-headers
                                      sock "Content-Length: ~s~a" content-length crlf))
      
              
        (if* cookies 
           then (let ((str (compute-cookie-string uri
                                                cookies)))
                (if* str
                   then (net.aserve::format-dif :xmit-client-request-headers
                                                sock "Cookie: ~a~a" str crlf))))

	;; bug22805
	(when (and (not basic-authorization) (net.uri:uri-userinfo uri))
	  (let ((userinfo (delimited-string-to-list (net.uri:uri-userinfo uri) #\:)))
	    (setf basic-authorization (cons (first userinfo) (or (second userinfo) "")))))
	
        (if* basic-authorization
           then (net.aserve::format-dif :xmit-client-request-headers
                                        sock "Authorization: Basic ~a~a"
					(base64-encode
					 (format nil "~a:~a" 
						 (car basic-authorization)
						 (cdr basic-authorization)))
					crlf))
        
        (if* proxy-basic-authorization
           then (net.aserve::format-dif :xmit-client-request-headers
                                        sock "Proxy-Authorization: Basic ~a~a"
                                      (base64-encode
                                       (format nil "~a:~a" 
                                               (car proxy-basic-authorization)
                                               (cdr proxy-basic-authorization)))
                                      crlf))
        
        (if* (and digest-authorization
                (digest-response digest-authorization))
           then ; put out digest info
              (net.aserve::format-dif 
               :xmit-client-request-headers sock
               "Authorization: Digest username=~s, realm=~s, nonce=~s, uri=~s, qop=~a, nc=~a, cnonce=~s, response=~s~@[, opaque=~s~]~a"
               (digest-username digest-authorization)
               (digest-realm digest-authorization)
               (digest-nonce digest-authorization)
               (digest-uri digest-authorization)
               (digest-qop digest-authorization)
               (digest-nonce-count digest-authorization)
               (digest-cnonce digest-authorization)
               (digest-response digest-authorization)
               (digest-opaque digest-authorization)
               crlf))
               
                                      
                                      
  
        (if* user-agent
           then (if* (stringp user-agent)
                 thenret
               elseif (eq :aserve user-agent)
                 then (setq user-agent net.aserve::*aserve-version-string*)
               elseif (eq :netscape user-agent)
                 then (setq user-agent "Mozilla/4.7 [en] (WinNT; U)")
               elseif (eq :ie user-agent)
                 then (setq user-agent "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)")
                 else (error "Illegal user-agent value: ~s" user-agent))
              (net.aserve::format-dif :xmit-client-request-headers
                                      sock "User-Agent: ~a~a" user-agent crlf))
  
        (if* content-type
           then (net.aserve::format-dif :xmit-client-request-headers
                                        sock "Content-Type: ~a~a"
                                      content-type
                                      crlf))
        
        (if* headers
           then (dolist (header headers)
                (net.aserve::format-dif :xmit-client-request-headers
                                         sock "~a: ~a~a" 
                                        (car header) (cdr header) crlf)))
        (if* use-socket then (force-output sock)))
      
  
      (write-string crlf sock)  ; final crlf
      
      ; send out the content if there is any.
      ; this has to be done differently so that if it looks like we're
      ; going to block doing the write we start another process do the
      ; the write.  

      ;; if there is an 'Expect: 100-continue' header, store the content
      ;; in the request header so it can be delivered upon receipt of a
      ;; 100 response.
      (let ((cont (cdr (assoc "Expect" headers :test #'string-equal)))
	    deferred-content)
	(if* (and cont (match-re "\\b100-continue\\b" cont :case-fold t :return nil))
	   then (setf deferred-content content)
		(net.aserve::debug-format
		 :xmit-client-request-body
		 "deferring delivery of request body due to Expect header.")
	   else (send-request-body sock content))
            
	(force-output sock)

	(make-instance 'client-request
	  :uri uri
	  :socket sock
	  :cookies cookies
	  :method method
	  :return-connection (if* keep-alive then :maybe)
	  :deferred-content deferred-content)))))


(defun uri-path-etc (uri)
  ;; return the string form of the uri path, query and fragment
  (let ((nuri (net.uri:copy-uri uri)))
    (setf (net.uri:uri-scheme nuri) nil)
    (setf (net.uri:uri-host nuri) nil)
    (setf (net.uri:uri-port nuri) nil)
    (setf (net.uri:uri-userinfo nuri) nil)
    (if* (null (net.uri:uri-path nuri))
       then (setf (net.uri:uri-path nuri) "/"))
    
    (net.uri:render-uri nuri nil)))
    
    
(defmethod read-client-response-headers ((creq client-request)
					 &key throw-on-eof)
  ;; read the response and the headers
  (let ((buff (get-header-line-buffer))
	(buff2 (get-header-line-buffer))
	(pos 0)
	len
	(sock (client-request-socket creq))
	(headers)
	protocol
	response
	comment
	val
	)
    (unwind-protect
	(with-better-scan-macros
	    (if* (null (setq len (read-socket-line sock buff (length buff))))
	       then ; eof getting response
		    
		    ; see if someone cares
		    (if* throw-on-eof
		       then 
			    (throw throw-on-eof t))
		    
		    (error "premature eof from server"))
          (net.aserve::debug-format :xmit-client-response-headers "~a~a"
                                    (subseq buff 0 len) crlf)
	  (macrolet ((fail ()
		       `(let ((i 0))
			  (error "illegal response from web server: ~s"
				 (collect-to-eol buff i len)))))
	    (setq protocol (collect-to #\space buff pos len))
	    (skip-to-not #\space buff pos len)
	    (setq response (collect-to #\space buff pos len nil t))
	    ; some servers don't return a comment, so handle that
	    (skip-to-not #\space buff pos len nil)
	    (setq comment (collect-to-eol buff pos len)))

	  (if* (equalp protocol "HTTP/1.0")
	     then (setq protocol :http/1.0)
	   elseif (equalp protocol "HTTP/1.1")
	     then (setq protocol :http/1.1)
	     else (error "unknown protocol: ~s" protocol))
      
	  (setf (client-request-protocol creq) protocol)
      
	  (setf (client-request-response-code creq) 
	    (quick-convert-to-integer response))
      
	  (setf (client-request-response-comment creq) comment)
      

	  ; now read the header lines
	  (setq headers (net.aserve::compute-client-request-headers sock))
	  
      
	  (setf (client-request-headers creq) headers)
	  
	  ;; do cookie processing
	  (let ((jar (client-request-cookies creq)))
	    (if* jar
	       then ; do all set-cookie requests
		    (let (prev cs)
		      ; Netscape v3 web server bogusly splits set-cookies
		      ; over multiple set-cookie lines, so we look for
		      ; incomplete lines (those ending in #\;) and combine
		      ; them with the following set-cookie
		      (dolist (headval headers)
			(if* (eq :set-cookie (car headval))
			   then (if* prev 
				   then (setq prev (concatenate 'string
						     prev (cdr headval)))
				   else (setq prev (cdr headval)))
				
				(if* (not (eq #\; (last-character prev)))
				   then (push prev cs)
					(setq prev nil))
					
			 elseif prev
			   then (push prev cs)
				(setq prev nil)))
		      
		      (if* prev
			 then (push prev cs))
		      
		      (dolist (cc (nreverse cs))
			(save-cookie (client-request-uri creq)
				     jar
				     cc)))))
	  
	  
	  (setf (client-request-content-encoding creq)
	    (cdr (assoc (client-response-header-value creq :content-encoding) 
			'(("gzip" . :gzip)  ("deflate" . :deflate))
			:test #'equalp)))
	  
	  (if* (eq :head (client-request-method creq))
	     then  ; no data is returned for a head request
		  (setf (client-request-bytes-left creq) 0)
	   elseif (equalp "chunked" (client-response-header-value 
				     creq :transfer-encoding))
	     then ; data will come back in chunked style
		  (setf (client-request-bytes-left creq) :chunked)
		  #+ignore
		  (socket:socket-control (client-request-socket creq)
					 :input-chunking t)
	   elseif (setq val (client-response-header-value
			     creq :content-length))
	     then ; we know how many bytes are left
		  (setf (client-request-bytes-left creq) 
		    (quick-convert-to-integer val))
	   elseif (not (equalp "keep-alive"
			       (client-response-header-value
				creq :connection)))
	     then ; connection will close, let it indicate eof
		  (setf (client-request-bytes-left creq) :unknown)
		  (setf (client-request-return-connection creq) nil)
	     else ; no data in the response
		  nil)
	  
		  
	  
	  creq  ; return the client request object
	  )
      (progn (put-header-line-buffer buff2 buff)))))
		  



(defvar *client-request-creq* nil)

(defmethod client-request-read-sequence (buffer
					 (creq client-request)
					 &key
					 (start 0)
					 (end (length buffer)))
  ;; read the next (end-start) bytes from the body of client request, handling
  ;;   turning on chunking if needed
  ;;   return index after last byte read.
  ;;   return 0 if eof
  (let ((socket (get-client-request-response-stream creq))
        (bytes-left (client-request-bytes-left creq))
	(last start))
    (if* (integerp bytes-left)
       then
	    (if* (zerop bytes-left)
	       then 0  ; eof
             elseif (stringp buffer)
		    ;; We know the amount of bytes left, not characters left
               then (let ((pos start)
                          (dummy-str (make-string 1))
                          (ch (read-char socket)))
		      ;; This is a bit of a hack -- ACL doesn't seem to expose a
		      ;; sane interface for determining the amount of bytes a
		      ;; character requires in a given encoding.
		      ;; Use macrolet instead of flet to avoid making a closure
		      ;; in this frquently used path. [bug23176]
                      (macrolet ((char-size
				  (ch)
				  (or (and ch (symbolp ch)) (error "(char-size sym)"))
				  `(progn
				     (setf (char dummy-str 0) ,ch)
				     (native-string-sizeof dummy-str))))
                        (loop
			  (setf (aref buffer pos) ch)
			  (incf pos)
			  (if* (or (<= (decf bytes-left (char-size ch)) 0)
                                   (= pos end)
                                   (null (setf ch (read-char-no-hang socket nil))))
			     then (setf (client-request-bytes-left creq) bytes-left)
				  (return pos)))))
		    ;; just a normal read-sequence
	       else (let ((ans (net.aserve::rational-read-sequence buffer 
								   socket :start start
								   :end (+ start 
									   (min (- end start) 
										bytes-left)))))
		      (if* (eq ans start)
			 then 0  ; eof
			 else (net.aserve::debug-format
                               :xmit-client-response-body "~s"
                               (octets-to-string buffer :start start :end ans
                                                 :external-format :octets))
			      (setf (client-request-bytes-left creq)
				(- bytes-left (- ans start)))
			      ans)))
     elseif (or (eq bytes-left :chunked)
		(eq bytes-left :unknown))
       then (let ((*client-request-creq* creq))
	      (catch 'client-request-chunk-eof-catch
		;; This was a handler-case that creates a closure for each error 
		;; clause.  [bug23176]
		(handler-bind
		 ((excl::socket-chunking-end-of-file #'client-request-throw-eof))
		 (do ((i start (1+ i))
		      (stringp (stringp buffer))
		      (debug-on (member :xmit-client-response-body
					net.aserve::*debug-current*
					:test #'eq)))
		     ((>= i end) (setq last end))
		   (setq last i)
		   (let ((ch (if* stringp
				  then (read-char socket nil nil)
				  else (read-byte socket nil nil))))
		     (if* (null ch)
			  then (setf (client-request-bytes-left creq) :eof)
			  (return)
			  else (if* debug-on
				    then (net.aserve::debug-format
					  :xmit-client-response-body "~a"
					  (if* (characterp ch) 
					       then ch
					       else (code-char ch))))
			  (setf (aref buffer i) ch)))))))
	    ; we return zero on eof, regarless of the value of start
	    ; I think that this is ok, the spec isn't completely clear
	    (if* (eql last start) 
	       then 0 
	       else last)
     elseif (eq bytes-left :eof)
       then 0
       else (error "socket not setup for read correctly")
	    )))

(defun client-request-throw-eof (x)
  (declare (ignore x))
  (setf (client-request-bytes-left *client-request-creq*) :eof)
  (throw 'client-request-chunk-eof-catch nil))

  

(defmethod client-request-close ((creq client-request))
  (let ((sock (client-request-socket creq)))
    (if* (and sock (not (eq (client-request-return-connection creq) 
			    :yes)))
       then (let ((trailers (convert-trailers (net.aserve::unchunking-trailers 
					       (client-request-response-stream creq)))))
	      (if* trailers
		 then (setf (client-request-headers  creq)
			(append (client-request-headers  creq) trailers))))
		      
	    (setf (client-request-socket creq) nil)
	    (ignore-errors (force-output sock))
	    (ignore-errors (close sock)))))


(defun quick-convert-to-integer (str)
  ; take the simple string and convert it to an integer
  ; it's assumed to be a positive number
  ; no error checking is done.
  (let ((res 0))
    (dotimes (i (length str))
      (let ((chn (- (char-code (schar str i)) #.(char-code #\0))))
	(if* (<= 0 chn 9)
	   then (setq res (+ (* 10 res) chn)))))
    res))


(defmethod client-response-header-value ((creq client-request)
					 name &key parse)
  ;; return the value associated with the given name
  ;; parse it too if requested
  (if* (stringp name)
     then (error "client-response headers are now named by symbols, not strings"))
  
  (let ((val (cdr (assoc name (client-request-headers creq) :test #'eq))))
    (if* (and parse val)
       then (net.aserve::parse-header-value val)
       else val)))

    
(defmacro ignore-connection-reset-and-abort (&body body)
  (let ((tag (gensym)))
    `(catch ',tag
       (handler-bind
           ((socket-error
              (lambda (e)
                (when (member (stream-error-identifier e)
                              '(:connection-reset :connection-aborted))
                  (throw ',tag nil)))))
         ,@body))))


(defun read-socket-line (socket buffer max)
  ;; read the next line from the socket.
  ;; the line may end with a linefeed or a return, linefeed, or eof
  ;; in any case don't put that the end of line characters in the buffer
  ;; return the number of characters in the buffer which will be zero
  ;; for an empty line.
  ;; on eof return nil
  ;;
  (let ((i 0))
    (loop
      (let ((ch (ignore-connection-reset-and-abort (read-char socket nil nil))))
	(if* (null ch)
	   then ; eof from socket
		(if* (> i 0)
		   then ; actually read some stuff first
			(return i)
		   else (return nil) ; eof
			)
	 elseif (eq ch #\return)
	   thenret ; ignore
	 elseif (eq ch #\linefeed)
	   then ; end of the line,
		(return i)
	 elseif (< i max)
	   then ; ignore characters beyone line end
		(setf (schar buffer i) ch)
		(incf i))))))
		
		
    
      
;; buffer pool for string buffers of the right size for a header
;; line

(net.aserve::defvar-mp *response-header-buffers* nil)

(defun get-header-line-buffer ()
  ;; return the next header line buffer
  (let (buff)
    (net.aserve::smp-case
     ((t :macros)
      (setq buff
	(pop-atomic *response-header-buffers*)))
     (nil
      (excl::atomically ;; in a #-smp form
       (excl::fast (setq buff (pop *response-header-buffers*))))))
    (if* buff
       thenret
       else (make-array 400 :element-type 'character))))

(defun put-header-line-buffer (buff &optional buff2)
  ;; put back up to two buffers
  (net.aserve::smp-case
   (nil
    (mp:without-scheduling ;; in a #-smp form
      (push buff *response-header-buffers*)
      (if* buff2 then (push buff2 *response-header-buffers*))))
   ((t :macros)
    (progn
      (push-atomic buff *response-header-buffers*)
      (if* buff2
	 then (push-atomic buff2 *response-header-buffers*))))
   ))




(defun compute-digest-authorization (creq da)
  ;; compute the digest authentication info, if such is present
  ;; return true if did the authentication thing
  (let ((val (cdr (assoc :www-authenticate (client-request-headers creq))))
	(params))
    
    
    (if* (not (and val
		   (null (mismatch "digest " val :end2 7 :test #'char-equal))))
       then ; not a digest authentication
	    (return-from compute-digest-authorization nil))
    
    (setq params (net.aserve::parse-header-line-equals 
		  val #.(length "digest ")))
    
    
    (setf (digest-opaque da) (cdr (assoc "opaque" params :test #'equal)))
    
    (let ((md (md5-init))
	  (qop (cdr (assoc "qop" params :test #'equalp)))
	  (ha1)
	  (ha2))
      
      (setf (digest-qop da) qop)
      
      (md5-update md (digest-username da))
      (md5-update md ":")
      (md5-update md (setf (digest-realm da)
		       (or (cdr (assoc "realm" params :test #'equalp)) "")))
      (md5-update md ":")
      (md5-update md (digest-password da))
      (setq ha1 (md5-final md :return :hex))
      
      ; compute a2
      
      (setq md (md5-init))
      (md5-update md (string-upcase
		      (symbol-name (client-request-method creq))))
      (md5-update md ":")
      ; this is just a part of the whole uri but should be enough I hope
      (md5-update md (setf (digest-uri da) 
		       (uri-path-etc (client-request-uri creq))))

      (if* (equal "auth-int" qop)
	 then (error "auth-int digest not supported"))
      
      (setq ha2 (md5-final md :return :hex))
      
      
      
      
      ; calculate response
      
      (setq md (md5-init))
      
      (md5-update md ha1)
      (md5-update md ":")
      (md5-update md (setf (digest-nonce da)
		       (or (cdr (assoc "nonce" params :test #'equalp))
			   "")))
      (md5-update md ":")
      (if* qop
	 then (md5-update md (digest-nonce-count da))
	      (md5-update md ":")
	      (md5-update md (setf (digest-cnonce da)
			       (format nil "~x" (+ (ash (get-universal-time) 5)
						 (random 34567)))))
	      (md5-update md ":")
	      (md5-update md qop)
	      (md5-update md ":"))
      (md5-update md ha2)
      
      (setf (digest-response da) (md5-final md :return :hex))

      t
      )))
	      
	      
	      
      
	
			       
      
      
      
      
      
      
    
    
    

	    
	    
    

    

;;;;; cookies

(defclass cookie-jar ()
  ;; holds all the cookies we've received
  ;; items is a alist where each item has the following form:
  ;; (hostname cookie-item ...)
  ;; 
  ;; where hostname is a string that must be the suffix
  ;;	of the requesting host to match
  ;; path is a string that must be the prefix of the requesting host
  ;;	to match
  ;;  
  ;;
  ((items :initform nil
	  :accessor cookie-jar-items)))

(defmethod print-object ((jar cookie-jar) stream)
  (print-unreadable-object (jar stream :type t :identity t)
    (format stream "~d cookies" (length (cookie-jar-items jar)))))

;* for a given hostname, there will be only one cookie with
; a given (path,name) pair
;
(defstruct cookie-item 
  path      ; a string that must be the prefix of the requesting host to match
  name	    ; the name of this cookie
  value	    ; the value of this cookie
  expires   ; when this cookie expires
  secure    ; t if can only be sent over a secure server
  http-only ; t if http-only cookie
  )


(defmethod save-cookie (uri (jar cookie-jar) cookie)
  ;; we've made a request to the given host and gotten back
  ;; a set-cookie header with cookie as the value 
  ;; jar is the cookie jar into which we want to store the cookie
  
  (let* ((pval (car (net.aserve::parse-header-value cookie t)))
	 namevalue
	 others
	 path
	 domain
	 )
    (if* (consp pval)
       then ; (:param namevalue . etc)
	    (setq namevalue (cadr pval)
		  others (cddr pval))
     elseif (stringp pval)
       then (setq namevalue pval)
       else ; nothing here
	    (return-from save-cookie nil))
    
    ;; namevalue has the form name=value
    (setq namevalue (net.aserve::split-on-character namevalue #\=
						    :count 1))
    
    ;; compute path
    (setq path (cdr (net.aserve::assoc-paramval "path" others)))
    (if* (null path)
       then (setq path (or (net.uri:uri-path uri) "/"))
       else ; make sure it's a prefix
	    (if* (not (net.aserve::match-head-p 
		       path (or (net.uri:uri-path uri) "/")))
	       then ; not a prefix, don't save
		    (return-from save-cookie nil)))
    
    ;; compute domain
    (setq domain (cdr (net.aserve::assoc-paramval "domain" others)))
    
    (if* domain
       then ; one is given, test to see if it's a substring
	    ; of the host we used
	    (if* (null (net.aserve::match-tail-p domain 
						 (net.uri:uri-host uri)))
	       then (return-from save-cookie nil))
       else (setq domain (net.uri:uri-host uri)))
    
    
    (let ((item (make-cookie-item
		 :path path
		 :name  (car namevalue)
		 :value (or (cadr namevalue) "")
		 :secure (net.aserve::assoc-paramval "secure" others)
		 :expires (cdr (net.aserve::assoc-paramval "expires" others))
		 :http-only (net.aserve::assoc-paramval "httponly" others)
		 )))
      ; now put in the cookie jar
      (let ((domain-vals (assoc domain (cookie-jar-items jar) :test #'equal)))
	(if* (null domain-vals)
	   then ; this it the first time for this host
		(push (list domain item) (cookie-jar-items jar))
	   else ; this isn't the first
		; check for matching path and name
		(do* ((xx (cdr domain-vals) (cdr xx))
		     (thisitem (car xx) (car xx)))
		    ((null xx)
		     )
		  (if* (and (equal (cookie-item-path thisitem)
				   path)
			    (equal (cookie-item-name thisitem)
				   (car namevalue)))
		     then ; replace this one
			  (setf (car xx) item)
			  (return-from save-cookie nil)))
		
		; no match, must insert based on the path length
		(do* ((prev nil xx)
		      (xx (cdr domain-vals) (cdr xx))
		      (thisitem (car xx) (car xx))
		      (length (length path)))
		    ((null xx)
		     ; put at end
		     (if* (null prev) then (setq prev domain-vals))
		     (setf (cdr prev) (cons item nil)))
		  (if* (>= (length (cookie-item-path thisitem)) length)
		     then ; can insert here
			  (if* prev
			     then (setf (cdr prev)
				    (cons item xx))
				  
			     else ; at the beginning
				  (setf (cdr domain-vals)
				    (cons item (cdr domain-vals))))
			  (return-from save-cookie nil))))))))
		  
      

; rfc6265 says that all cookies are one line separated by this:
(defparameter cookie-separator "; ")
    

(defmethod compute-cookie-string (uri (jar cookie-jar))
  ;; compute a string of the applicable cookies.
  ;;
  (let ((host (net.uri:uri-host uri))
	(path (or (net.uri:uri-path uri) "/"))
	res
	rres)
    
    (dolist (hostval (cookie-jar-items jar))
      (if* (net.aserve::match-tail-p (car hostval)
				     host)
	 then ; ok for this host
	      (dolist (item (cdr hostval))
		(if* (net.aserve::match-head-p (cookie-item-path item)
					       path)
		   then ; this one matches
			(push item res)))))
    
    (if* res
       then ; have some cookies to return
	    #+ignore (dolist (item res)
		       (push (cookie-item-value item) rres)
		       (push "=" rres)
		       (push (cookie-item-name item) rres)
		       (push semicrlf rres))
	    
	    (dolist (item res)
	      (push (cookie-item-value item) rres)
	      (push "=" rres)
	      (push (cookie-item-name item) rres)
	      (push cookie-separator rres))
	    
	    (pop rres) ; remove first seperator
	    (apply #'concatenate 'string  rres))))

(defun last-character (string)
  ;; return the last non blank character, or nil
  (do ((i (1- (length string)) (1- i))
       (ch))
      ((< i 0) nil)
    (setq ch (schar string i))
    (if* (eq #\space ch) 
       thenret
       else (return ch))))
