;; aserve proxy module

(in-package :net.aserve)

(defclass locator-proxy (locator)
  ;; denotes sending the request to another machine
  ;;
  ())


(defvar *locator-proxy-obj* nil)  ; the object in the locator chain if proxying
(defvar *entity-proxy* nil)	  ; the entity denoting we should proxy

(defun enable-proxy (&key (server *wserver*))
  (if* (null *locator-proxy-obj*)
     then (setq *locator-proxy-obj* (make-instance 'locator-proxy :name :proxy)
		*entity-proxy* (make-instance 'computed-entity
				 :function #'(lambda (req ent)
					       (do-proxy-request req ent)))))
  
  ; must be first as other locators may not ignore absolute proxy urls
  (pushnew *locator-proxy-obj* (wserver-locators server))
  
  )

(defmethod standard-locator ((req http-request) (locator locator-proxy))
  ;; see if this is a proxy request and if so return the entity that
  ;; denotes we're proxying
  (if* (uri-scheme (request-raw-uri req))
     then *entity-proxy*))


(defun do-proxy-request (req ent)
  ;; a request has come in which has a uri with a scheme part,
  ;; thus denoting a request to be proxied (unless it's on our machine).
  (let* ((uri (request-raw-uri req))
	 (scheme (uri-scheme uri))
	 (host   (uri-host uri))
	 (port   (or (uri-port uri) 80))
	 )
    (if* (or (not (eq scheme :http))
	     (null host))
       then (with-http-response (req ent :response *response-bad-request*)
	      (with-http-body (req ent)
		(html (:html (:head (:title "Bad Request"))
			     (:body "This url isn't a valid proxy request "
				    (:princ-safe (net.uri:render-uri uri nil)))))))
       else ; see if it's a local request
	    (let ((ipaddr (socket:lookup-hostname host)))
	      (if* (and ipaddr
			(member ipaddr
				(wserver-ipaddrs *wserver*))
			(eq port 
			    (socket:local-port (wserver-socket *wserver*)))
			)
		 then ; it's us, make it into into a local request
		      ; and look  it up again
		      (setf (request-raw-uri req)
			(net.uri:copy-uri uri :scheme nil :host nil))
		      (handle-request req)
		 else ; must really proxy
		      (proxy-request req ent))))))

		      
		      
(defmethod unpublish-locator ((locator locator-proxy))
  nil)

		     

(defun proxy-request (req ent)
  ;; a request has come in with an http scheme given in uri
  ;; and a machine name which isn't ours.
  ;; 
  ;; the headers have been parsed.
  ;;
  ;; send out the request
  ;;
  (let* ((request-body (get-request-body req))
	 (outbuf (get-sresource *header-block-sresource*))
	 (outend)
	 (clibuf)
	 (cliend)
	 (sock)
	 (uri (request-raw-uri req))
	 (host (uri-host uri))
	 (port (uri-port uri))
	 (method (request-method req))
	 (protocol :http/1.0)
	 (state :pre-send)
	 )

    (unwind-protect
	(progn
	  
	  (format *debug-stream* "do proxy to ~s~%" uri)
	  (force-output *debug-stream*)

	  (handler-bind ((error #'(lambda (cond)
				    (format *debug-stream*
					    "error during proxy: ~a~%" cond)
				    (if* (eq state :pre-send)
				       then ; haven't sent anything
					    ; so send failed response
					    (ignore-errors
					     (proxy-failure-response req ent)))
				    (return-from proxy-request nil))))
	    
			 
				    
	    ; create outgoing headers by copying
	    (setq outend (copy-headers (request-header-block req) outbuf
				       *header-client-array*))
    
	    ;; now insert new headers
    
	    ; content-length is inserted iff this is put or post method
	    (if* (member method '(:put :post))
	       then (setq outend (insert-header outbuf outend :content-length
						(format nil "~d"
							(if* request-body
							   then (length request-body)
							   else 0)))))
    
	    ; connection  we'll set to 'close' for now but at some point
	    ; we'll connection caching so we'll want to do some keep-alive'ing
	    ;  
	    (setq outend (insert-header outbuf outend :connection "close"))
    

	    ; send host header if it isn't already there
	    (if* (null (header-buffer-values (request-header-block req) :host))
	       then ; no host given
		    (setq outend (insert-header outbuf outend :host
						(if* port
						   then (format nil "~a:~d"
								host port)
						   else host))))
	    (setq outend (insert-end-of-headers outbuf outend))

	    (if-debug-action :xmit
			     (format *debug-stream* "proxy covnerted headers toward server~%")
			     (dotimes (i outend)
			       (write-char (code-char (aref outbuf i)) *debug-stream*))
			     (format *debug-stream* "---- end---~%")
			     (force-output *debug-stream*))
  
  
  
		   
		   
		   
	    ; time to make a call to the server
	    (setq sock (socket:make-socket :remote-host host
					   :remote-port (or port 80)
					   :format :bivalent))

	    (if* *watch-for-open-sockets*
		   then (schedule-finalization 
			 sock 
			 #'check-for-open-socket-before-gc))
	    
	    (net.aserve::format-dif :xmit sock "~a ~a ~a~a"
				    (string-upcase (string method))
				    (net.aserve.client::uri-path-etc uri)
				    (string-upcase (string protocol))
				    *crlf*)
	  
	    ; now the headers
	    (write-sequence outbuf sock :end outend)
    
	    ; now the body if any
	    (if* request-body
	       then (write-sequence request-body sock))
    
	    (force-output sock)
	  
	    ; a shutdown would make sense here but it seems to confuse
	    ; the aol servers
	    ; (socket:shutdown sock :direction :output)

	    (let (protocol response comment header-start given-content-length
		  body-buffers body-length)
	      (loop
		; loop until we don't get a 100 continue
		;
		; now read the response and the following headers
		(setq outend (read-headers-into-buffer sock outbuf))

		(if* (null outend)
		   then ; response coming back was truncated
			(return-from proxy-request
			  (proxy-failure-response req ent)))
		      
  
		(multiple-value-setq (protocol response comment header-start)
		  (parse-response-buffer outbuf))

		(if* (null protocol)
		   then ; bogus response
			(return-from proxy-request
			  (proxy-failure-response req ent)))
	      
		(if* (not (eql response 100)) then (return)))
    

	      (setf (request-reply-code req) 
		(code-to-response response)) ; for the logging
	    
	      (parse-header-block outbuf header-start outend)
	    
	      ; Get the body of the message if any.
	      ; there is never a response  to a :head request although the header
	      ;  fields may imply there is.
	      ; These response codes don't have a message body:
	      ;	1xx, 204, 304
	      ; All other responses include a message body which may be of zero size
	      ;
    
	      (if* (setq given-content-length
		     (header-buffer-header-value outbuf :content-length))
		 then (setq given-content-length
			(net.aserve.client::quick-convert-to-integer 
			 given-content-length)))


	      (setf (request-reply-content-length req) (or given-content-length 0))
	    
	      (if* (not (or (eq (request-method req) :head)
			    (<= 100 response 199) 
			    (eq response 204)
			    (eq response 304)))
		 then ; got to read the body
		      (if* (null given-content-length)
			 then (multiple-value-setq (body-buffers body-length)
				(read-into-block-buffers sock nil))
			 else (multiple-value-setq (body-buffers body-length)
				(read-into-block-buffers sock 
							 given-content-length))
			      (if* (not (eql body-length given-content-length))
				 then (warn "content-length ~s but body length ~d"
					    given-content-length body-length)
				      (setq given-content-length body-length))))
    
	      (close sock)  (setq sock nil)

	    
	      ; convert the header we received from the server into one
	      ; to send to the client
	      (setq clibuf (get-sresource *header-block-sresource*))
    
	  
	      (setq cliend (copy-headers outbuf clibuf *header-server-array*))
    
	      ; add content-length if known
	      (if* given-content-length
		 then (setq cliend
			(insert-header clibuf cliend :content-length 
				       (format nil "~s" given-content-length))))
    
	      ; should add a 'via' line
    
	      ; transfer-encoding - 
	      ; we won't chunk back since we know the content length

	      (setq cliend (insert-end-of-headers clibuf cliend))

	  
	      (if-debug-action 
	       :xmit
	       (format *debug-stream* "~%~%proxy converted headers toward client~%")
	       (dotimes (i cliend)
		 (write-char (code-char (aref clibuf i)) 
			     *debug-stream*))
	       (format *debug-stream* "---- end---~%")
	       (force-output *debug-stream*))

	      ; do the response
	      (setq state :post-send)
	      (let ((rsock (request-socket req)))
		(format rsock "HTTP/1.1 ~d ~a~a" response comment *crlf*)
      
		(write-sequence clibuf rsock :end cliend)
		(if* body-length 
		   then (write-body-buffers rsock body-buffers body-length))
		(dolist (block body-buffers)
		  (free-sresource *header-block-sresource* block))
		(force-output rsock)))))
    
      ;; cleanup forms
      (if* sock then (ignore-errors (close sock)))
      (free-sresource *header-block-sresource* outbuf)
      (free-sresource *header-block-sresource* clibuf))))

    
(defun parse-response-buffer (buff)
  ;; the buffer should contain the first line of an http respose
  ;; and a response code, a crlf and then headers (but not including
  ;; the crlf after the headers)
  ;;
  (let (protocol response-code comment beginc)
    (flet ((match (array list)
	     ; test if the list of bytes matches the prefix of the array
	     (do ((i 0 (1+ i))
		  (ll list (cdr ll)))
		 ((null ll) t)
	       (if* (not (eq (aref array i) (car ll)))
		  then (return nil)))))
		  
      ;; 
      (if* (match buff '(#.(char-int #\H)
			 #.(char-int #\T)
			 #.(char-int #\T)
			 #.(char-int #\P)
			 #.(char-int #\/)
			 #.(char-int #\1)
			 #.(char-int #\.)))
	 then (case (aref buff 7)
		(#.(char-code #\0) (setq protocol :http/1.0))
		(#.(char-code #\1) (setq protocol :http/1.1)))
	      (if* (null protocol)
		 then (return-from parse-response-buffer nil)))
    
      ; compute response code
      (let ((val 0)
	    (i 8))
	(loop 
	  (let ((chv (aref buff i)))
	    (if* (<= #.(char-code #\0) chv #.(char-code #\9))
	       then (setq val (+ (* val 10) (- chv #.(char-code #\0))))
	     elseif  (member chv '(#.(char-code #\space)
				   #.(char-code #\return)
				   #.(char-code #\linefeed))
			     :test #'eq)
	       then (if* (not (zerop val))
		       then (return) ; whitespace after value, get out
			    )
	       else ; bogus response code
		    (return-from parse-response-buffer nil))
	    (incf i)))
	(setq response-code val)
      
	; search for begining of comment
	(loop
	
	  (let ((chv (aref buff i)))
	    (if* (member chv '(#.(char-code #\return)
			       #.(char-code #\linefeed))
			 :test #'eq)
	       then ; end of line before seeing a comment
		    (return)
	     elseif (not (eq chv #.(char-code #\space)))
	       then ; beginning of comment
		    (setq beginc i)
		    (return))
	    (incf i)))
      
      
	(if* beginc
	   then ; found beginning, search for end
		(loop (let ((chv (aref buff i)))
			(if* (member chv '(#.(char-code #\return)
					   #.(char-code #\linefeed))
				     :test #'eq)
			   then ; hit the end
				(return))
			(incf i))))
	; we have what we need
	(if* beginc
	   then (let ((str (make-array (- i beginc)
				       :element-type '(unsigned-byte 8))))
		  (do ((jj beginc (1+ jj))
		       (ii 0 (1+ ii)))
		      ((>= jj i))
		    (setf (aref str ii) 
		      (aref buff jj)))
		
		  (setq comment str)))
      
	(values protocol response-code comment i)))))


		
(defun read-into-block-buffers (sock size)
  ;; read up to size bytes from sock into a sequnce of block
  ;; buffers.
  ;; if size is nil then read until end of file.
  ;; return a list of block buffers with all full except perhaps
  ;; the last one
  ;; return a second value which is the number of bytes read
  (if* (eql size 0)
     then (return-from read-into-block-buffers (values nil 0)))
  
  (let (res block len bytesleft (bytesread 0) (start 0))
    (setq block (get-sresource *header-block-sresource*))
    (push block res)
    (setq len (length block))
    
    (setq bytesleft (or size len))
    (loop
      (let ((retv (read-sequence block sock 
				:start start
				:end (min len (+ start bytesleft)))))
	(if* (<= retv start)
	   then ; end of file
		(return)
	   else ; read something
		(if* size 
		   then (decf bytesleft (- retv start))
		   else (incf bytesread (- retv start)))
		(if* (<= bytesleft 0)
		   then (return)) ; all done
		(setq start retv)
		(if* (>= start len)
		   then ; need a new block
			(push (setq block (get-sresource
					   *header-block-sresource*))
			      res)
			(setq start 0)))))
    (values (nreverse res) 
	    (if* size
	       then (- size bytesleft)
	       else bytesread))))
	    
		    
	
      
	      
(defun write-body-buffers (sock buffers length)
  ;; write all the data in the buffers to the socket
  (if* (> length 0)
     then (let ((len (if* buffers then (length (car buffers)))))
	    (dolist (buff buffers)
	      (write-sequence buff sock :end (min length len))
	      (decf length len)
	      (if* (<= length 0) then (return))))))

    
(defun proxy-failure-response (req ent)
  (with-http-response (req ent :response *response-not-found*)
    (with-http-body (req ent)
      (html (:title "not found by proxy")
	    (:body
	     (:h1 "no found")
	     "The proxy could not find the requested uri")))))

  
      
      
		       
    
		  
	
    

    
    
	    
	    
		    
	    
				  
		    
	    
  

  
  


    
    
	    
	    
	    
	    
    
    
		  


      
    
  


  




