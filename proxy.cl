;; aserve proxy module

(in-package :net.aserve)

(defclass locator-proxy (locator)
  ;; denotes sending the request to another machine
  ;;
  ())


(defvar *locator-proxy-obj* nil)  ; the object in the locator chain if proxying
(defvar *entity-proxy* nil)	  ; the entity denoting we should proxy

(defun enable-proxy ()
  (if* (null *locator-proxy-obj*)
     then (setq *locator-proxy-obj* (make-instance 'locator-proxy :name :proxy)
		*entity-proxy* (make-instance 'computed-entity
				 :function #'(lambda (req ent)
					       (do-proxy-request req ent)))))
  
  ; must be first as other locators may not ignore absolute proxy urls
  (pushnew *locator-proxy-obj* (wserver-locators *wserver*))
  
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
	 (host   (uri-host uri)))
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
				(wserver-ipaddrs *wserver*)))
		 then ; it's us, make it into into a local request
		      ; and look  it up again
		      (setf (request-raw-uri req)
			(net.uri:copy-uri uri :scheme nil :host nil))
		      (handle-request req)
		 else ; must really proxy
		      (format t "will proxy ~s~%" uri)
		      
		      (multiple-value-bind
			  (body response headers)
			  (net.aserve.client:do-http-request
			      (request-uri req)
			    :method (request-method req)
			    :protocol :http/1.1
			    :accept (header-slot-value req "accept")
			    :user-agent (header-slot-value req "user-agent")
			      
			    :headers (request-headers req)
			    :content (get-request-body req)
			    :format :text)
			
			      
			(with-http-response (req ent :response 
						 (code-to-response response))
			  (with-http-body (req ent :headers headers)
			    (if* body
			       then (write-string 
				     body 
				     (request-reply-stream req)))))))))))

		      
		      
(defmethod unpublish-locator ((locator locator-proxy))
  nil)

		     

(defun proxy-request (req)
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
	 (uri (request-raw-uri req))
	 (host (uri-host uri))
	 (port (uri-port uri))
	 (method (request-method uri))
	 (protocol :http/1.0)
	 ))

  (debug-format :xmit "do proxy to ~s~%" uri)
		
  ; create outgoing headers by copying
  (setq outend (copy-headers (request-header-block req) outbuf
			     *header-client-array*))
    
  ;; now insert new headers
    
  ; content-length is inserted iff this is put or post method
  (if* (member method '(:put :post))
     then (setq outend (insert-header outbuf end :content-length
				      (format nil "~d"
					      (if* request-body
						 then (length (request-body))
						 else 0)))))
    
  ; connection  we'll set to 'close' for now but at some point
  ; we'll connection caching so we'll want to do some keep-alive'ing
  ;  
  (setq outend (insert-header outbuf end :connection "close"))
    

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
    
  (net.aserve::format-dif :xmit sock "~a ~a ~a~a"
			  (string-upcase (string method))
			  (uri-path-etc uri)
			  (string-upcase (string protocol))
			  crlf)
  ; now the headers
  (write-sequence outbuf sock :end outend)
    
  ; now the body if any
  (if* request-body
     then (write-sequence request-body sock))
    
  (force-output sock)

  (let (protocol response commment header-start given-content-length
	body-buffers body-length)
    (loop
      ; loop until we don't get a 100 continue
      ;
      ; now read the response and the following headers
      (setq outend (read-headers-into-buffer sock outbuf))
  
  
      (multiple-value-setq (protocol response comment headerstart)
	(parse-response-buffer outbuf outend))
    
      (if* (not (eql response 100)) then (return)))
    
    
    ; now get the body of the message if any.
    ; there is never a response  to a :head request although the header
    ;  fields may imply there is.
    ; These response codes don't have a message body:
    ;	1xx, 204, 304
    ; All other responses include a message body which may be of zero size
    ;
    
    (setq given-content-length
      (header-buffer-value outbut :content-length))
    
    
    (if* (not (or (<= 100 response 199) 
		  (eq response 204)
		  (eq response 304)))
       then ; got to read the body
	    (let (buffers 
	    (if* (null content-length)
	       then (warn "no content length, read till eof")
		    (multiple-value-setq (body-buffers body-length)
		      (read-into-block-buffers sock nil))
		    else (
		    
	    
  

  
  
    ))


    
    
	    
	    
	    
	    
    
    
		  


      
    
  


  
