;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; proxy.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
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
;; $Id: proxy.cl,v 1.18 2000/10/06 15:16:16 jkf Exp $

;; Description:
;;   aserve's proxy and proxy cache

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)


(defparameter *extra-lifetime-factor* 1.1)

(defstruct pcache 
  ;; proxy cache
  table		; hash table mapping to pcache-ent objects
  file		; file in which to store the cache info
  (probes 0)	; cache problems
  (hits	  0)	; number of times we could send something from the cache
  (items  0)	; total number of objects cached
  (bytes  0)	; total number of bytes cached
  (memory 0)	; total number of bytes in memory
  (disk	  0)	; bytes written to disk

  ; requests that completely bypass the cache
  (r-direct 0)

  ; ims or non-ims where there is no entry that mathes this url
  ; and the request headers
  (r-miss 0)

  ; non-ims request.  value within the min-freshness contstraints
  ; ims request where we know that the value in the cache is fresh
  ;  and has been modified since the ims time
  (r-fast-hit 0)
  
  
  ; ims request made and cached value is fresh and based on that
  ; we know that the client's version is fresh so send 
  ; not-modified response back
  (r-fast-validation   0) 

  ; ims request where we return not-modified after checking with
  ; the origin server
  (r-slow-validation 0)
  
  
  ; stale copy in the cache, we check with the origin server
  ; and find that our copy was inconsistent and get a new copy to cache
  (r-consistency-miss 0)
  
  
  
  )



(defstruct pcache-ent
  last-modified-string  ; last modified time from the header
  last-modified  ; universal time entry was last modified
  expires	; universal time when this entry expires
  
  
  request	; request header block 
  data		; data blocks (first is the response header block)
  data-length	; number of octets of data
  code		; response code
  comment	; response comment 

  ; count of the internal use of this
  use 		; nil - dead entry. >= 0 - current users of this entry

  (state :new)	; nil - normal , 
                ; :dead - trying to kill off, 
  		; :new - filling the entry
  
  ; number of times this entry was returned due to a request
  (returned 0)
  )




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
	    (let ((ipaddr (ignore-errors (socket:lookup-hostname host))))
	      (if* (null ipaddr)
		 then (with-http-response (req ent :response
					       *response-not-found*)
			(with-http-body (req ent)
			  (html
			   (:html
			    (:head (:title "404 - Not Found"))
			    (:body
			     (:h1 "Host not found")
			     "The proxy failed to find the address for host "
			     (:b (:princ-safe host)))))))
		      (return-from do-proxy-request))
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
		      (proxy-cache-request req ent))))))


  
		      
(defmethod unpublish-locator ((locator locator-proxy))
  nil)

		     

(defun proxy-request (req ent &key pcache-ent (respond t))
  ;; a request has come in with an http scheme given in uri
  ;; and a machine name which isn't ours.
  ;; 
  ;; the headers have been parsed.
  ;;
  ;; send out the request
  ;; get the response and if respond is true send back the response
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
				    
				    (if* pcache-ent
				       then (setf (pcache-ent-state 
						   pcache-ent)
					      :dead))
						   
				    (if* (not (member :notrap *debug-current* 
						      :test #'eq))
				       then ; we want to auto-handle the error
					    (if* (eq state :pre-send)
					       then ; haven't sent anything
						    ; so send failed response
						    (ignore-errors
						     (proxy-failure-response req ent)))
					    (return-from proxy-request nil)))))
	    

	    ;(logmess "got proxy request")
	    ;(dump-header-block (request-header-block req) *initial-terminal-io*)
				    
	    ; create outgoing headers by copying
	    (copy-headers (request-header-block req) outbuf
			  *header-client-array*)
    
	    ;; now insert new headers
    
	    ; content-length is inserted iff this is put or post method
	    (if* (member method '(:put :post) :test #'eq)
	       then (insert-header outbuf :content-length
				   (format nil "~d"
					   (if* request-body
					      then (length request-body)
					      else 0))))
    
	    ; connection  we'll set to 'close' for now but at some point
	    ; we'll connection caching so we'll want to do some keep-alive'ing
	    ;  
	    (insert-header outbuf :connection "close")
    

	    ;(logmess "outbuf now")
	    ;(dump-header-block outbuf *initial-terminal-io*)
	    
	    ; send host header if it isn't already there
	    (if* (null (header-buffer-values (request-header-block req) :host))
	       then ; no host given
		    (insert-header outbuf :host
				   (if* port
				      then (format nil "~a:~d"
						   host port)
				      else host)))
	    (setq outend (add-trailing-crlf outbuf 1))

	    (if-debug-action :xmit
			     (format *debug-stream* "proxy covnerted headers toward server~%")
			     (dotimes (i outend)
			       (write-char (code-char (aref outbuf i)) *debug-stream*))
			     (format *debug-stream* "---- end---~%")
			     (force-output *debug-stream*))
  
  
  
		   
		   
		   
	    ; time to make a call to the server
	    (handler-case
		(setq sock (socket:make-socket :remote-host host
					       :remote-port (or port 80)
					       :format :bivalent
					       :type *socket-stream-type*))
	      (error (cond)
		(declare (ignore cond))
		(with-http-response (req ent :response
					 *response-not-found*)
		  (with-http-body (req ent)
		    (html
		     (:html
		      (:head (:title "404 - Not Found"))
		      (:body
		       (:h1 "404 - Not Found")
		       "The proxy failed to connect to machine "
		       (:b (:princ-safe host))
		       " on port "
		       (:b (:princ-safe (or port 80))))))))
		(return-from proxy-request)))

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


	      
	    
	      (if* (not (or (eq (request-method req) :head)
			    (<= 100 response 199) 
			    (eq response 204)
			    (eq response 304)))
		 then ; got to read the body
		      (multiple-value-setq (body-buffers body-length)
			(read-into-block-buffers sock 
						 given-content-length))
		      
		      (if* (and given-content-length
				(not (eql body-length given-content-length)))
			 then (warn "content-length ~s but body length ~d"
				    given-content-length body-length)
			      (setq given-content-length body-length)))
	      
	      
	      (setf (request-reply-content-length req) 
		(or body-length given-content-length 0))
    
	      (close sock)  (setq sock nil)

	    
	      ; convert the header we received from the server into one
	      ; to send to the client
	      (setq clibuf (get-sresource *header-block-sresource*))
    
	  
	      (copy-headers outbuf clibuf *header-server-array*)
    
	      ; add content-length if known
	      (if* given-content-length
		 then (insert-header clibuf :content-length 
				     (format nil "~s" given-content-length)))
    
	      ; should add a 'via' line
    
	      ; transfer-encoding - 
	      ; we won't chunk back since we know the content length

	      (setq cliend (add-trailing-crlf clibuf 2))

	  
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
	      
	      (if* respond
		 then (ignore-errors
		       (let ((rsock (request-socket req)))
		
			 (format rsock "HTTP/1.1 ~d ~a~a" response comment *crlf*)
      
			 (write-sequence clibuf rsock :end cliend)
			 (if* body-length 
			    then (write-body-buffers rsock body-buffers 
						     body-length))
			 (force-output rsock))))
		
	      (if* (and pcache-ent 
			(eq (request-method req) :get))
		 then ; we are caching
		      (cache-response req pcache-ent
				      response comment clibuf 
				      body-buffers body-length)
		      ; these buffers have been saved in the cache
		      ; so nil them out so they aren't freed
		      (setf clibuf nil
			    body-buffers nil
			    (request-header-block req) nil))
		
	      (dolist (block body-buffers)
		(free-sresource *header-block-sresource* block))
	      )))
    
      ;; cleanup forms
      (if* sock 
	 then (ignore-errors (force-output sock))
	      (ignore-errors (close sock :abort t)))
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

  


;;;--------------------- proxy cache ------------------      

(defparameter *min-freshness* 10) ; assume values valid this long
(defparameter *likely-fresh*  60) ; values probably valid this long but check

; cache items are in one of these states
;  fresh - item in case is considered to be valid
;  stale - item in cache may be valid but it will requires validate to verify
;





; proxy cache actions
;  direct - go to the proxy without considering the cache
;  hit - found item in the cache and it is fresh
; 





  
(defun create-proxy-cache (&key (server *wserver*))
  ;; create a cache for the proxy
  (let (pcache)
    (setf (wserver-pcache server)
      (setq pcache (make-pcache 
		    :table (make-hash-table :test #'equal))))
  
    (publish :path "/cache-stats"
	     :function
	     #'(lambda (req ent)
		 (display-proxy-cache-statistics req ent pcache)))))
				


(defun display-proxy-cache-statistics (req ent pcache)
  (let ((dead-count 0)
	(dead-bytes 0)
	(new-count 0)
	(per-age-count (make-array 10 :initial-element 0))
	(per-age-bytes (make-array 10 :initial-element 0))
	(other-age-count 0)
	)
    (maphash #'(lambda (key vals)
		 (declare (ignore key))
		 (dolist (val vals)
		   (let ((state (pcache-ent-state val))
			 (use (pcache-ent-use val))
			 (size (pcache-ent-data-length val)))
		     (if* (eq state :dead)
			then (incf dead-count)
			     (incf dead-bytes
				   (if* (numberp size) then size else 0))
		      elseif (eq state :new)
			then (incf new-count)
		      elseif (and (null state)
				  (numberp use)
				  (<= 0 use 9))
			then (incf (aref per-age-count use))
			     (incf (aref per-age-bytes use)
				   (if* (numberp size) then size else 0))
			else (incf other-age-count)))))
					   
	     (pcache-table pcache))
    
    (with-http-response (req ent)
      (with-http-body (req ent)
	(html
	 (:html
	  (:head (:title "AllegroServe Proxy Cache Statistics"))
	  (:body 
	   (:h1 "AllegroServe Proxy Cache Statistics")
	   ((:table :border 2)
	    (:tr
	     (:th "Cache Action")
	     (:th "Count"))
		       
	    (dolist (ent
			'(("direct"   pcache-r-direct)
			  ("miss"     pcache-r-miss)
			  ("consistency miss" pcache-r-consistency-miss)
			  ("fast hit" pcache-r-fast-hit)
			  ("fast validation" pcache-r-fast-validation)
			  ("slow validation" pcache-r-slow-validation)))
	      (html
	       (:tr 
		(:td (:princ (car ent)))
		(:td (:princ (funcall (cadr ent) pcache)))))))
	  
	  
	   ((:table :border 2)
	    (:tr
	     (:th "Kind of Entry")
	     (:th "Count")
	     (:th "Bytes"))
	   
	    (:tr
	     (:td "Dead Entries") 
	     (:td (:princ dead-count))
	     (:td (:princ dead-bytes)))
	   
	   
	    (:tr
	     (:td "New Entries") (:td (:princ new-count)) (:td))
	   
	    (:tr
	     (:td "Other Entries") (:td (:princ other-age-count)) (:td))
	   
	    (dotimes (i 10)
	      (html
	       (:tr
		(:td (:princ i))
		(:td (:princ (aref per-age-count i)))
		(:td (:princ (aref per-age-bytes i))))))))))))))
				     
				     
				   
  


(defun proxy-cache-request (req ent)
  ;; if we've got a proxy cache then retrieve it from there
  ;; else just proxy the request
  
  
  (let ((pcache (wserver-pcache *wserver*))
	(rendered-uri (net.uri:render-uri (request-raw-uri req) nil)))
    
    (if* (or (null pcache)
	     ; should handle :head requests too
	     (not (eq (request-method req) :get))
	     ; don't look in cache if request has cookies
	     (header-slot-value req :authorization)
	     (header-slot-value req :cookie)
	     )
       then (if* pcache then (incf (pcache-r-direct pcache)))
	    (logmess (format nil "direct for ~a~%" rendered-uri))
	    (return-from proxy-cache-request
	      (proxy-request req ent)))

    (logmess (format nil "cache: look in cache for ~a~%" rendered-uri))

    (dolist (pcache-ent (gethash rendered-uri (pcache-table pcache))
	      ; not found, must proxy and then cache if the
	      ; result looks good
	      (progn
		(logmess "not in cache, proxy it")
		(proxy-and-cache-request req ent (get-universal-time) nil)))
      (if* (lock-pcache-ent pcache-ent)
	 then (unwind-protect
		  (progn (use-value-from-cache req ent pcache-ent)
			 (return))
		(unlock-pcache-ent pcache-ent))))))



(defun proxy-and-cache-request (req ent now pcache-ent)
  ;; must send the request to the net via the proxy.
  ;; if pcache-ent is non-nil then this is the existing
  ;; cache entry which may get updated or killed.
  ;; 
  (let ((new-ent (make-pcache-ent))
	(rendered-uri (net.uri:render-uri (request-raw-uri req) nil))
	(pcache (wserver-pcache *wserver*)))
    (proxy-request req ent :pcache-ent new-ent)
    (if* (eq (pcache-ent-code new-ent) 200)
       then ; turns out it was modified, must
	    ; make this the new entry

	    (if* pcache-ent
	       then (logmess (format nil "replace cache entry for ~a"
				     rendered-uri))
		    (logmess "consistency miss")
		    (incf (pcache-r-consistency-miss pcache))
	       else (logmess "miss")
		    (incf (pcache-r-miss pcache)))
	    
		    
	    (push new-ent
		  (gethash rendered-uri
			   (pcache-table pcache)))
			      
	    ; and disable the old entry
	    (if* pcache-ent
	       then (setf (pcache-ent-state pcache-ent) :dead))
     elseif (and pcache-ent
		 (eq (pcache-ent-code new-ent) 304))
		 
       then ; still not modified, recompute the 
	    ; expiration time since we now know
	    ; that the item is older
	    ;* this may end up violation the expiration
	    ; time in a header from a previous call
	    (logmess (format nil "change expiration date for ~a"
			     rendered-uri))      
	    (incf (pcache-r-slow-validation pcache))
	    (setf (pcache-ent-expires pcache-ent)
	      (max (pcache-ent-expires pcache-ent)
		   (compute-approx-expiration
		    (pcache-ent-last-modified pcache-ent)
		    now))))))
  


(defun use-value-from-cache (req ent pcache-ent)
  ;; we've determined that pcache-ent matches the request.
  ;; now deal with the issue of it possibily being out of date
  (let* ((ims (header-slot-value req :if-modified-since))
	 (now (get-universal-time))
	 (pcache (wserver-pcache *wserver*))
	 (fresh))
    (logmess (format nil "ims is ~s" ims))

    ; compute if the entry is fresh or stale
    (setq fresh (<= now (pcache-ent-expires pcache-ent)))
    
    
    (if* (and ims (not (equal "" ims)))
       then ; we're in a conditional get situation, where the
	    ; condition is If-Modified-Since
	    (setq ims (date-to-universal-time ims))
	    
	    (if* (< ims (pcache-ent-last-modified pcache-ent))
	       then ; it has been modified since the ims time
		    ; must return the whole thing
		    (if* fresh
		       then (logmess "validation->fast hit")
			    (incf (pcache-r-fast-hit pcache))
			    (send-cached-response req pcache-ent)
		       else (proxy-and-cache-request req ent now pcache-ent))
		    
	     elseif fresh
	       then ; (>= ims last-modified-time) and the entry
		    ; is fresh, we we believe the last-modified-time
		    ; thus we respond that entry is correct
		    (logmess "fast validation")
		    (incf (pcache-r-fast-validation pcache))
		    (send-not-modified-response req ent)
	       else ; (>= ims last-modified-time) butthe entry
		    ;  stale, so we can't trust the last modified time
		    ;
		    (proxy-and-cache-request req ent now pcache-ent))
       else ; unconditional get
	    (if* fresh
	       then (logmess "fast hit")
		    (incf (pcache-r-fast-hit pcache))
		    (send-cached-response req pcache-ent)
	       else ; issue a validating send
		    
		    (insert-header 
		     (request-header-block req)
		     :if-modified-since
		     (or (pcache-ent-last-modified-string pcache-ent) 
			 (setf (pcache-ent-last-modified-string pcache-ent)
			   (universal-time-to-date
			    (pcache-ent-last-modified pcache-ent)))))

		    (proxy-and-cache-request req ent now pcache-ent)))))
		      
		    
(defun compute-approx-expiration (changed  now)
  ;; compute the expires time based on the last time the file was
  ;; change and current time.
  (let ((delta (max 1 (ceiling 
		       (* (- now changed) *extra-lifetime-factor*)))))
    (+ changed delta)))

  


(defun send-not-modified-response (req ent)
  ;; return a not-modified response
  (with-http-response (req ent :response *response-not-modified*)
    (with-http-body (req ent))))

	    
	    



(defun send-cached-response (req pcache-ent)
  ;; send back this response
  (logmess (format nil "cache: sending back cached response: ~a, length ~d~%" 
		   (net.uri:render-uri (request-raw-uri req) nil)
		   (pcache-ent-data-length pcache-ent)))
  (incf (pcache-ent-returned pcache-ent))
  
  (let ((rsock (request-socket req)))
    (format rsock "HTTP/1.1 ~d ~a~a" 
	    (pcache-ent-code pcache-ent)
	    (pcache-ent-comment pcache-ent) *crlf*)

    (setf (request-reply-code req) (code-to-response
				    (pcache-ent-code pcache-ent)))
    
    (let ((data (pcache-ent-data pcache-ent))
	  (data-length (pcache-ent-data-length pcache-ent)))
      
      
      (setf (request-reply-content-length req) data-length)
      
      (write-sequence (car data) rsock 
		      :end (add-trailing-crlf (car data) 3))
      (if* data-length 
	 then (write-body-buffers rsock (cdr data) data-length))
		
		
      (force-output rsock))))



(defun lock-pcache-ent (pcache-ent)
  ;; attempt to increase the use count of this entry by one.
  ;; If successful return true.
  ;; If the entry is dead return nil
  (mp:without-scheduling
    (let ((val (pcache-ent-use pcache-ent)))
      (if* val
	 then (setf (pcache-ent-use pcache-ent) (1+ val))))))

(defun unlock-pcache-ent (pcache-ent)
  ;; reduce the use count of this entry
  (mp:without-scheduling
    (let ((val (pcache-ent-use pcache-ent)))
      (if* val
	 then (if* (and (zerop (decf val))
			(eq (pcache-ent-state pcache-ent) :dead))
		 then (setf (pcache-ent-use pcache-ent) nil)
		 else (setf (pcache-ent-use pcache-ent) val))))))


#+ignore
(defun match-request-blocks (request-block cache-block)
  ;; compare the header entries that have to match for this
  ;; cached request to be used.
  ;;
  (if* (eq request-block cache-block)
     then (error "block reused when it shouldn't have"))
  
  (let ((hcma *header-cache-match-array*))
    (dotimes (i (length hcma) t)
      (let ((ent (svref hcma i)))
	(if* ent
	   then (if* (not (header-match-values request-block
					       cache-block
					       i
					       (eq ent :mx)))
		   then ; give up
			(return nil)))))))

#+ignore
(defun response-is-young-enough (last-modified-ut request-block)
  ;; return true if the last-modified date satisified
  ;; the contraints of the request
  
  (declare (ignore last-modified-ut request-block))
  
  
  ;; to be done
  t
  )
    

(defun cache-response (req pcache-ent 
		       response-code comment client-response-header
		       body-buffers body-length)
  
  ;; we are caching, save the information about this response 
  ;; in the pcache-ent we are passed, which should be blank
  
	    
  (logmess (format nil "cache: caching response to ~a, length ~d~%" 
		   (net.uri:render-uri (request-raw-uri req) nil)
		   body-length
		   ))
  
  (let (now)
    (if* (eql response-code 200)
       then ; full response
	    (setf (pcache-ent-code pcache-ent) response-code)
	    (setf (pcache-ent-comment pcache-ent) comment)
	  
	    (setf (pcache-ent-data pcache-ent) 
	      (cons client-response-header body-buffers))
	    (setf (pcache-ent-data-length pcache-ent) body-length)
	  
	    
	  
	    (setf (pcache-ent-state pcache-ent) nil ; means valid data
		  (pcache-ent-use  pcache-ent) 0)
	  
	    (let* ((last-mod (header-buffer-header-value 
			      client-response-header
			      :last-modified))
		   (last-mod-val (and last-mod
				      (date-to-universal-time last-mod)))
		   (expires (header-buffer-header-value 
			     client-response-header
			     :expires))
		   (expires-val (and expires
				     (date-to-universal-time expires))))
	      
	      
	      (setq client-response-header nil
		    body-buffers nil)
	    
	      (if* last-mod-val
		 then (setf (pcache-ent-last-modified-string pcache-ent) last-mod
			    (pcache-ent-last-modified pcache-ent) last-mod-val)
		 else ; no value given, store current time minus a 
		      ; second to account for the transit time
		    
		      (setf (pcache-ent-last-modified pcache-ent)  
			(- (setq now (get-universal-time)) 2)))
	    
	      (if* expires-val
		 then ; given expiration date, use it
		      ;* it may be bogus since people doing ads use this
		      ;  to force cache reloads
		      (setf (pcache-ent-expires pcache-ent) expires-val)
		 else ; must compute expiration
		      (setf (pcache-ent-expires pcache-ent)
			(compute-approx-expiration
			 (pcache-ent-last-modified pcache-ent)
			 (or now (get-universal-time))))))
		       
     elseif (eql response-code 304)
       then ; just set that so the reader of the response will know
	    ; the result
	    (setf (pcache-ent-code pcache-ent) response-code))
  
    (if* client-response-header 
       then (free-header-block client-response-header))
  
    (if* body-buffers
       then (free-header-blocks body-buffers))))

	  
  



  

#+ignore	    
(defun dump-cache ()
  ;; dump the proxy cache
  (let ((pcache (wserver-pcache *wserver*)))
    (if* (null pcache)
       then (format t "There is no cache~%")
       else (format t  "~%---- proxy cache --- ~%")
	    (maphash #'(lambda (k values)
			 (format t "uri: ~a~%" k)
			 (dolist (pcache-ent values)
			   (format t " code: ~s~%data-length: ~s~%"
				   (pcache-ent-code  pcache-ent)
				   (pcache-ent-data-length pcache-ent))
			   (format t " returned: ~s~%"
				   (pcache-ent-returned pcache-ent))
			   (format t " use: ~d~%max valid: ~d~%"
				   (pcache-ent-use  pcache-ent)
				   (pcache-ent-max-valid-time  pcache-ent))
			   (format t " request header:~%")
			   (dump-header-block (pcache-ent-request 
					       pcache-ent))
			   (format t "~%-----~%")
			   
			   
			   (format t "~%")))
		     (pcache-table pcache)))))

    

		  
	
    

    
    
	    
	    
		    
	    
				  
		    
	    
  

  
  


    
    
	    
	    
	    
	    
    
    
		  


      
    
  


  




