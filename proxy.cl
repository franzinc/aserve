;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; proxy.cl
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
;;   aserve's proxy and proxy cache

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)

(check-smp-consistency)

(defmacro with-mp-locked-connection-cache ((s) &rest body)
  (smp-case
   ((t :macros) `(with-locked-structure (,s :non-smp :without-scheduling)
		   ,@body))
   (nil `(si::without-scheduling ,s ,@body)))) ;; in a #-smp block

(defmacro with-mp-locked-pcache-ent ((s) &rest body)
  (smp-case
   ((t :macros) `(with-locked-structure (,s :non-smp :without-scheduling)
		   ,@body))
   (nil `(si::without-scheduling ,s ,@body)))) ;; in a #-smp block

(defmacro with-fast-mp-locked-pcache-ent ((s) &rest body)
  (smp-case
   ((t :macros) `(with-locked-structure (,s :non-smp :atomically)
		   ,@body))
   (nil `(excl::atomically ,s ,@body)))) ;; in a #-smp block

(defmacro with-mp-locked-pcache ((s) &rest body)
  (smp-case
   ((t :macros) `(with-locked-structure (,s :non-smp :without-scheduling)
		   ,@body))
   (nil `(si::without-scheduling ,s ,@body)))) ;; in a #-smp block

(defmacro with-mp-locked-pcache-queue ((s) &rest body)
  (smp-case
   ((t :macros) `(with-locked-structure (,s :non-smp :without-scheduling)
		   ,@body))
   (nil `(si::without-scheduling ,s ,@body)))) ;; in a #-smp block

; denotes a request from the browser
(defconstant *browser-level* 100) ;

(defparameter *extra-lifetime-factor* 1.1)

; number of seconds to add to expiration time of any entry in cache
(defparameter *extra-lifetime* 0) 
	

; true if we are to save cached connections
(defparameter *connection-caching* t) 




; statistics about connection caching

(defparameter *connections-cached* 0) ; number of connections put in to cache
(defparameter *connections-made*  0) ; number of make-socket calls made
(defparameter *connections-used-cached* 0) ; number of cached connections used
; the cache
(defparameter *connection-cache-queue* (cons nil nil)) ; (first . last) queue of conn-cache objects
(defparameter *connection-cache-expire* 10) ; number of seconds to live

(defparameter *connection-cache-lock*
    (smp-case
     ((t :macros) (excl::make-basic-lock :name "connection-cache"))
     (nil nil)))

; number of seconds we wait for a connect before we consider it timed
; out.  Given the chance Linux seems to wait forever for a connection
; so we need to shut it down ourselves.
(defparameter *connection-timed-out-wait* 30)


(defstruct (pcache 
	    #+smp-macros (:include synchronizing-structure)
	    )
  ;; proxy cache
  table		; hash table mapping to pcache-ent objects
  disk-caches   ; list of pcache-disk items

  cleaner	; process doing housecleaning
  (cleaner-lock  (mp:make-process-lock :name "cache cleaner")) 

  size		; specified size of the cache (in blocks)
  high-water    ; when blocks in cache gets above this start flushing to disk
  low-water     ; when blocks in cache get below this stop flushing
  
  (dead-items 0) ; total number of objects on the dead-ent list

  (level0-time 0) ; last time level0 access was done
  
  queueobj	; queue object holding in-memory cache objects
  
  dead-ent	; linked list of dead pcache-ents, linked by next field only

  ; hash table of handlers for uris, keyed by host
  uri-info-table

  ; pointer to linkscan object if we are doing automatic link scanning
  ; in the proxy cache
  linkscan

  entry-cached-hook ; funcalled with pcache, pcache-ent and level
  
  ; requests that completely bypass the cache
  (r-direct 0)

  ; ims or non-ims where there is no entry that mathes this url
  ; and the request headers
  (r-miss 0)

  ; like r-miss except the level is greater than 0 thus this is a
  ; cache fill rather than a direct user request
  (r-cache-fill 0)
  
  ; non-ims request.  value within the min-freshness contstraints
  ; ims request where we know that the value in the cache is fresh
  ;  and has been modified since the ims time
  (r-fast-hit 0)

  ; non-ims request.  value is stale but after checking with the
  ; origin server we find that our value is up to date
  (r-slow-hit 0)
  
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



(defstruct pcache-disk
  ;; for each disk file on which we are caching
  filename
  stream
  blocks	; total blocks in cache
  free-blocks   ; number of blocks remaining
  free-list ; list of (start . end) blocks that are free

  high-water  ; when free blocks is less than this start flushing
  low-water   ; when free blocks is more than this stop flushing
  
  (lock (mp:make-process-lock :name "disk pcache"))
  ; doubly linked list of pcache-ents in this cache
  queueobj
  )





; each pcache entry holds the information on a 200 response
; use  state
; --- ------
; >=0  nil	in memory cache entry.  linked to mru-ent
; >0   :dead	in memory entry in use but which will no longer be used
;		after the uses are over.  linked to dead-ent
; nil  :dead	in memory entry which is ready to be reclaimed along
;		with all the blocks it points to.  linked to dead-ent
; 


(defstruct (pcache-ent
	    #+smp-macros (:include synchronizing-structure)
	    )
  key		; the string form of the uri 
  uri		; the actual uri
  last-modified-string  ; last modified time from the header
  last-modified  ; universal time entry was last modified
  expires	; universal time when this entry expires
  
  
  data		; data blocks (first is the response header block)
  data-length	; number of octets of data
  blocks	; number of cache blocks (length of the value in data slot)
  code		; response code.   200 or 302
  comment	; response comment 
  
  cookie	; the cookie if any with this request

  ; count of the internal use of this
  use 		; nil - dead entry. >= 0 - current users of this entry

  (state :new)	; nil - normal , 
                ; :dead - trying to kill off, 
  		; :new - filling the entry

  ;; scanned	; true when link scanning has been done
  
  ; number of times this entry was returned due to a request
  (returned 0)
  
  ; if cached to the disk this tells where
  disk-location
  pcache-disk
  loading-flag  ; true when we are loading in from the disk
  
  ; linked list of pcache-ents
  queueobj  ; queue  object we're stored in
  prev
  next

  ; scanning for links
  ; notes:
  ;  links is initially nil and once the page has been scanned for links
  ;      it is t or a list of uri objects 
  ;      or it is :scanning when then pcache-ent is in the to be link scanned
  ;  level is non-zero when this entry is on the link-scan queue or
  ;	uri-scan queue.
  ;  scan-next is valid (nil or non-nil) when this entry is on the 
  ;    link-scan queue or uri-scan queue.
  ;  
  links	    ; nil or list of uris to img's then  list of uris from a links
  links-left ; list of uris still to scan
  level	    ; level at which to scan these links
  scan-next ; next pcache-ent to scan

  (autoscan-time 0) ; univeral time when last scanned by the link scanner
  )


(defstruct queueobj 
  (items 0)  ; number of items in the queue (aside from dummy ones)
  (bytes 0)  ; number of data bytes cached
  (blocks 0) ; number of buffer blocks
  mru  ; points to dummy pcache-ent at the head of the queue
  lru  ; points to dummy pcache-ent at the tail of the quee
  
  )
  

(defstruct uri-info
  ;; information on how to handle a uri or set of uris
  host	    ; string naming the host of the uri
  (port 80) 
  path      ; string in regexp form denoting the path. nil means all
  path-regexp ; compiled regular expression for the path (or nil)
  
  ; nil or number of extra seconds of lifetime to add to uri
  ; nil means use systemwide default
  extra-lifetime
  
  ; nil or default depth for link scanning
  ; nil means use systemwide default
  scan-depth 
  exclude ; list of regexps for links to not scan
  exclude-regexp ; compiled regexp versions of dont-follow
  
  ; called on links to determine at which level they should be followed
  scan-function 
  
  ; true if we should follow links to a site different than this page
  offsite
  )


  


(defclass locator-proxy (locator)
  ;; denotes sending the request to another machine
  ;;
  ())


(defclass proxy-control ()
  ;; used to control if proxy will operate or will send back a 500 error
  
  
  (
   ;; nil or a location-authorizer object to do request location checking
   (location  :initarg :location :accessor proxy-control-location
	      :initform nil)
   
   ;; nil  (meaning everything allowed)
   ;; or a non-empty list of hostname to which proxying is allowed, or a hash table of
   ;; hostnames to which proxying is allwoed
   
   (destinations :initarg :destinations
		 :accessor proxy-control-destinations
		 :initform nil)
   
  ))



(defun enable-proxy (&key (server *wserver*)
			  proxy-proxy)
  ;; 
  (let ((locator-proxy-obj 
	 (make-instance 'locator-proxy 
	   :name :proxy
	   :extra (make-instance 'computed-entity
		    :function #'(lambda (req ent)
				  (if* (authorize-proxy-request 
					req ent (wserver-proxy-control server))
				     then (do-proxy-request req ent)
				     else (denied-request req)))
		    :extra (if* proxy-proxy
			      then (multiple-value-bind (host port)
				       (get-host-port proxy-proxy)
				     (if* (null host)
					then (error "bad host port specification: ~s" proxy-proxy))
				     (cons host port)))
		    :will-handle-expect-continue t))))
	   
  
    ; must be first as other locators may not ignore absolute proxy urls
    (pushnew locator-proxy-obj (wserver-locators server))
  
    ))

(defmethod standard-locator ((req http-request) (locator locator-proxy))
  ;; see if this is a proxy request and if so return the entity that
  ;; denotes we're proxying
  (if* (uri-scheme (request-raw-uri req))
     then ; compute entity object
	  (locator-extra locator)))


(defmethod authorize-proxy-request ((req http-request) (ent entity) (proxy-control proxy-control))
  ;; return true iff this proxy request can be done
  ;;
  ; first check where request is from
  (let ((location (proxy-control-location proxy-control)))
    (if* location
       then (if* (null (authorize location req ent))
	       then (return-from authorize-proxy-request nil))))
	    
  ; and now where it's going
  (let ((destinations (proxy-control-destinations proxy-control)))
    (if* destinations
       then (let ((hostname (net.uri:uri-host (request-raw-uri req)))
		  (port     (net.uri:uri-port (request-raw-uri req))))
	      (if* (consp destinations)
		 then ; ("hostname" port1 port2 ...)
		      (dolist (dest destinations)
			(if* (consp dest)
			   then (if* (and (equalp (car dest) hostname)
					  (member (or port 80)
						  (cdr dest)))
				   then (return t))
			 elseif (equalp dest hostname)
			   then (return t)))
	       elseif (hash-table-p destinations)
		 then (let ((ports (gethash hostname destinations)))
			; ports is t (for all) or a list
			; of valid ports
			(or (eq ports t)
			    (and (consp ports)
				 (member (or port 80) ports))))))
       else t ; ok by default
	    )))

(defmethod authorize-proxy-request ((req http-request) (ent entity) (no-proxy-control (eql nil)))
  
  t)


				
				
				
		      
			      
		      
	    
  

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
		      (check-cache-then-proxy-request 
		       req ent t *browser-level*))))))


  
		      
(defmethod unpublish-locator ((locator locator-proxy))
  nil)

		     

(defun proxy-request (req ent &key pcache-ent (respond t) 
				   (level *browser-level*))
  ;; a request has come in with an http scheme given in uri
  ;; and a machine name which isn't ours.
  ;; 
  ;; the headers have been parsed.
  ;;
  ;; send out the request
  ;; get the response and if respond is true send back the response
  ;;
  (let* ((request-body (get-request-body req))
	 (outbuf (get-header-block))
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
	 (keep-alive)
	 (cached-connection)
	 (phostport (and ent (entity-extra ent)))
	 (otherheaders))

    (if* phostport
       then ; we're proxying to a proxy. yikes
	    (setq host (car phostport)
		  port (cdr phostport)))
    
    (unwind-protect
	(tagbody
	  
	 retry-proxy

	  (handler-bind ((error 
			  #'(lambda (cond)
			      (logmess
			       (format nil "error during proxy: ~a ~% with ~
cached connection = ~s~%" cond cached-connection))
			      (if* cached-connection
				 then ; retry
				      (logmess "retry proxy")
				      (if* sock
					 then (ignore-errors
					       (close sock :abort t)))
				      (go retry-proxy))
				    
			      (if* pcache-ent
				 then (kill-pcache-ent pcache-ent))
				    
						   
			      (if* (not (member :notrap *debug-current*
						:test #'eq))
				 then ; we want to auto-handle the error
				      (if* (eq state :pre-send)
					 then ; haven't sent anything
					      ; so send failed response
					      (ignore-errors
					       (proxy-failure-response req ent)))
				      (return-from proxy-request nil)))))
	    


	    (setq keep-alive nil ; assume not keep alive
		  cached-connection nil)
	    
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
	    
	    (insert-header outbuf :connection 
			   (if* *connection-caching*
			      then "Keep-Alive"
			      else "close"))
    

	    ;(logmess "outbuf now")
	    ;(dump-header-block outbuf *initial-terminal-io*)
	    
	    ; send host header 
	    (let ((host (or (request-header-host req)
			    (header-buffer-header-value
			     (request-header-block req) :host)
			    (if* port
			       then (format nil "~a:~d"
					    host port)
			       else host))))
	      (insert-header outbuf :host host))
			     
	    
	    ; if the proxier decides to check authorization before
	    ; doing a proxy then the authorization header will appear
	    ; in the request-headers list and we want to prevent the
	    ; authorization header from being sent twice in this case:
	    ; [spr33532]
	    (dolist (header (request-headers req))
	      (if* (not (eq (car header) :authorization))
		 then (insert-non-standard-header outbuf (car header) (cdr header))))
	    
	    (setq outend (add-trailing-crlf outbuf 1))

	    ; time to make a call to the server
	    (handler-case
		(multiple-value-setq (sock cached-connection)
		  (get-possibly-cached-connection
		   host (or port 80)))
	      (error (cond)
		(declare (ignore cond))
		(if* respond
		   then (with-http-response (req ent :response
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
			       (:b (:princ-safe (or port 80)))))))))
		(return-from proxy-request)))

	    (if* *watch-for-open-sockets*
	       then (schedule-finalization 
		     sock 
		     #'check-for-open-socket-before-gc))
	    

	    ;; there are bogus ip redirectors out there that want to
	    ;; see the whole request in the packet. (e.g www.cbs.com)
	    ;; so we build as much as we can and then blast that out
	    
	    ; this is written in this non-pretty way for speed
	    
	    
	    (let ((firstbuf (get-header-block))
		  (ind 0)
                  rest-of-headers-ind
		  (cmdstrings
		   '((:get . #.(make-array 3
					   :element-type '(unsigned-byte 8)
					   :initial-contents
					   (list
					    (char-int #\G)
					    (char-int #\E)
					    (char-int #\T))))
		     (:post . #.(make-array 4
				 :element-type '(unsigned-byte 8)
				 :initial-contents
				 (list
				  (char-int #\P)
				  (char-int #\O)
				  (char-int #\S)
				  (char-int #\T))))
				       
		     ))
		  (prot-strings
		   '((:http/1.0 . #.(make-array 8
						:element-type '(unsigned-byte 8)
						:initial-contents
						(list
						 (char-int #\H)
						 (char-int #\T)
						 (char-int #\T)
						 (char-int #\P)
						 (char-int #\/)
						 (char-int #\1)
						 (char-int #\.)
						 (char-int #\0)
						 )))
		     (:http/1.1 . #.(make-array 8
				     :element-type '(unsigned-byte 8)
				     :initial-contents
				     (list
				      (char-int #\H)
				      (char-int #\T)
				      (char-int #\T)
				      (char-int #\P)
				      (char-int #\/)
				      (char-int #\1)
				      (char-int #\.)
				      (char-int #\1)
				      )))))
		     
		  )
	      (let ((cmd (cdr (assoc method cmdstrings :test #'eq))))
		
		; write method
		(if* cmd
		   then (dotimes (i (length cmd))
			  (setf (ausb8 firstbuf i)  (ausb8 cmd i)))
			(incf ind (length cmd))
		   else ; unusual method, turn method into a string
			(let ((str (string-upcase (string method))))
			  (dotimes (i (length str))
			    (setf (ausb8 firstbuf i) 
			      (char-int (schar str i))))
			  (incf ind (length str))))

		(setf (ausb8 firstbuf ind) #.(char-int #\space))
		(incf ind)
		
		
		; now the uri
		(let ((str (if* phostport
			      then ; proxying so send http://...
				   (net.uri:render-uri (request-raw-uri req) 
						       nil)
			      else (net.aserve.client::uri-path-etc uri))))
		  (dotimes (i (length str))
		    ; should do string-to-octets...
		    (setf (ausb8 firstbuf ind) 
		      (char-int (schar str i)))
		    (incf ind)))
		
		(setf (ausb8 firstbuf ind) #.(char-int #\space))
		(incf ind)
		
		; now the protocol
		    
		(let ((cmd (cdr (assoc protocol prot-strings :test #'eq))))
		  (if* (null cmd)
		     then (error "can't proxy protocol ~s" protocol))
		  (dotimes (i (length cmd))
		    (setf (ausb8 firstbuf ind)  (ausb8 cmd i))
		    (incf ind)))
		    
		(setf (ausb8 firstbuf ind) #.(char-int #\return))
		(incf ind)
		(setf (ausb8 firstbuf ind) #.(char-int #\linefeed))
		(incf ind)
		    
                (debug-format :xmit-proxy-client-request-command "~s"
                              (octets-to-string firstbuf :end ind
                                                :external-format :octets))
                (setq rest-of-headers-ind ind)
		    
		; now add as much of the headers as we can 
		(do ((i 0 (1+ i))
		     (tocopy (min (- (length firstbuf) ind) outend)))
		    ((>= i tocopy)

                     (maybe-accumulate-log (:xmit-proxy-client-request-headers "~s")
					   (debug-format :xmit-proxy-client-request-headers "~a"
							 (octets-to-string firstbuf
									   :start rest-of-headers-ind
									   :end ind
									   :external-format :octets))
					   (write-sequence firstbuf sock :end ind)
					   (if* (< i outend)
					      then ; still more from original buffer left
						   (debug-format :xmit-proxy-client-request-headers "~a"
								 (octets-to-string
								  outbuf :start i :end outend
								  :external-format :octets))
						   (write-sequence outbuf sock
								   :start i
								   :end outend)))
		     )
		      
		  (setf (ausb8 firstbuf ind) (ausb8 outbuf i))
		  (incf ind))
		    
		(free-header-block firstbuf)))

	    
	    
	    ; now the body if any
	    (if* request-body
	       then (debug-format :xmit-proxy-client-request-body "~s"
                                  (octets-to-string request-body
                                                    :external-format :octets))
                    (write-sequence request-body sock))
    
	    (force-output sock)
	  
	    ; a shutdown would make sense here but it seems to confuse
	    ; the aol servers
	    ;(socket:shutdown sock :direction :output)

	    (let (protocol response comment header-start given-content-length
		  body-buffers body-length)
	      (loop
		; loop until we don't get a 100 continue
		;
		; now read the response and the following headers
		(setq outend (read-headers-into-buffer sock outbuf))
                (debug-format :xmit-proxy-client-response-headers "~s"
                              (octets-to-string outbuf :end outend
                                                :external-format :octets))

		(if* (null outend)
		   then ; response coming back was truncated
			(error "truncated proxy response"))
		      
  
		(multiple-value-setq (protocol response comment header-start)
		  (parse-response-buffer outbuf))

		(if* (null protocol)
		   then ; bogus response
			(return-from proxy-request
			  (proxy-failure-response req ent)))
	      
		(if* (not (eql response 100)) then (return)))
    

	      (setf (request-reply-code req) 
		(code-to-response response)) ; for the logging
	    
	      (setq otherheaders
		(parse-header-block outbuf header-start outend))
	    
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
				    given-content-length body-length))
		      (setq given-content-length body-length))
	      
	      
	      (setf (request-reply-content-length req) 
		(or body-length given-content-length 0))

	      (setq keep-alive
		(equalp (header-buffer-header-value outbuf :connection)
			"keep-alive"))
	      
	      (if* keep-alive
		 then (add-to-connection-cache sock
					       host
					       (or port 80))
		 else (close sock))
	      
	      (setq sock nil)

	    
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
	      
	      (dolist (header otherheaders)
		(insert-non-standard-header clibuf (car header) (cdr header)))

	      (setq cliend (add-trailing-crlf clibuf 2))

	      ; do the response
	      (setq state :post-send)
	      
	      (if* respond
		 then (ignore-errors
		       (let ((rsock (request-socket req)))

                         (debug-format
                          :xmit-proxy-server-response-headers
                          "~s"
                          (concatenate 'string
			    (format nil "HTTP/1.1 ~d ~a~a" response
				    (and comment (octets-to-string comment)) *crlf*)
			    (octets-to-string clibuf :end cliend
					      :external-format :octets)))

			 (format rsock "HTTP/1.1 ~d ~a~a" response (and comment (octets-to-string comment)) *crlf*)
      
			 (write-sequence clibuf rsock :end cliend)
			 (if* body-length 
			    then (write-body-buffers rsock body-buffers 
						     body-length))
			 (force-output rsock))))
		
	      (if* (and pcache-ent 
			(eq (request-method req) :get))
		 then ; we are caching
		      (let ((tmp-clibuf clibuf)
			    (tmp-body-buffers body-buffers))
			(setf clibuf nil
			      body-buffers nil)
			(cache-response req pcache-ent
					response comment tmp-clibuf 
					tmp-body-buffers body-length level)
			; these buffers have been saved in the cache
			; so nil them out so they aren't freed
			))
		
	      (dolist (block body-buffers) (free-header-block block))
	      )))
    
      ;; cleanup forms
      (if* sock 
	 then (ignore-errors (force-output sock))
	      (ignore-errors (close sock :abort t)))
      
      (free-header-block outbuf)
      (free-header-block clibuf))))

    
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
		(#.(char-int #\0) (setq protocol :http/1.0))
		(#.(char-int #\1) (setq protocol :http/1.1)))
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
      (let ((retv (rational-read-sequence block sock 
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
     then (maybe-accumulate-log (:xmit-proxy-server-response-body "~s")
            (let ((len (if* buffers then (length (car buffers)))))
              (dolist (buff buffers)
                (debug-format :xmit-proxy-server-response-body "~a"
                              (octets-to-string buff :end (min length len)
                                                :external-format :octets))
                (write-sequence buff sock :end (min length len))
                (decf length len)
                (if* (<= length 0) then (return)))))))

    
(defun proxy-failure-response (req ent)
  (with-http-response (req ent :response *response-not-found*)
    (with-http-body (req ent)
      (html (:title "not found by proxy")
	    (:body
	     (:h1 "not found")
	     "The proxy could not find the requested uri: "
	     (:princ-safe (request-raw-uri req)))))))

  
;;;--------------------- connection cache -------------
(defstruct connection-cache-ent 
  expire		; time when this entry expires
  host
  port
  socket
  )

  
(defun add-to-connection-cache (socket host port)
  (let* ((now (get-universal-time))
	 (ent (list (make-connection-cache-ent 
		     :expire (+ now *connection-cache-expire*)
		     :host host
		     :port port
		     :socket socket)))
	 (queue *connection-cache-queue*))
    
    (incf *connections-cached*)
    
    (with-mp-locked-connection-cache (*connection-cache-lock*)
      (incf *connections-cached*)
      (let ((start (first-valid-entry now queue)))
		
	(if* (null start)
	   then ; empty, this is the first entry
		(setf (car queue) 
		  (setf (cdr queue) ent))
		; add at the end
	   else (setf (cdr (cdr queue)) ent)
		(setf (cdr queue) ent))))))

(defun first-valid-entry (now queue)
  ;; remove expired entries and return the list of entries
  ;; beginning with the first non expired entry
  (let ((start (car queue)))
    ; scan down cache removing expired entries
    (loop
      (if* (null start) then (return))
      (if* (< (connection-cache-ent-expire 
	       (car start))
	      now)
	 then ; kill this one
	      (ignore-errors 
	       (close (connection-cache-ent-socket
		       (car start))
		      :abort t))
	      (setf (car queue)
		(setq start (cdr start)))
	 else (return)))
	
    start))
	
  
(defun get-possibly-cached-connection (host port)
  ;; check the cache and then return a cached connection
  ;; build a new one if there isn't  one cached
  (let ((now (get-universal-time))
	(queue *connection-cache-queue*))
    (with-mp-locked-connection-cache (*connection-cache-lock*)
      (let ((start (first-valid-entry now queue))
	    (prev nil))
	(loop
	  (if* (null start) then (return))
	  
	  (if* (and (equalp host (connection-cache-ent-host (car start)))
		    (eql port (connection-cache-ent-port (car start))))
	     then ; a match
		  (if* prev
		     then ; slice it out
			  (if* (null (setf (cdr prev) (cdr start)))
			     then ; we removed last entry, fix last
				  (setf (cdr queue) prev))
		     else ; we're removing the first
			  (setf (car queue) (cdr start)))
		  (incf *connections-used-cached*)
		  (return-from get-possibly-cached-connection 
		    (values (connection-cache-ent-socket (car start)) t)))
	  
	  (setq prev start
		start (cdr start)))))
	
    ; get here if there is no match

    (incf *connections-made*)
    (socket:with-pending-connect
	(mp:with-timeout (*connection-timed-out-wait*   ; ok w-t
			  (error "connection timed out"))
	  (socket:make-socket :remote-host host
			      :remote-port port
			      :format :bivalent
			      :type *socket-stream-type*
			      :nodelay t)))))

	
	
			  
				  
			  
  
	      
		
		
		
		
		
					 


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





  
(defun create-proxy-cache (&key (server *wserver*) (size #.(* 10 1024 1024)))
  ;; create a cache for the proxy
  (let (pcache)
    (setf (wserver-pcache server)
      (setq pcache (make-pcache 
		    :table (make-hash-table :size 1000 :test #'equal)
		    :queueobj (make-and-init-queueobj)
		    :uri-info-table (make-hash-table :test #'equalp)
		    )))
    
    (configure-memory-cache :server server :size size)
    (start-proxy-cache-processes server pcache)))


(defun start-proxy-cache-processes (server pcache &aux (thx (atomic-incf *thread-index*)))
  (let ((name (format nil "~d-~A~Acache-cleaner" 
		      thx
		      (if* *log-wserver-name* 
			 then (wserver-name server) 
			 else "")
		      (if* *log-wserver-name* 
			 then "-" 
			 else ""))))
    (setf (pcache-cleaner pcache)
      (mp:process-run-function 
       name
       #'(lambda (server)
	   (let ((*wserver* server)
		 (pcache (wserver-pcache server)))
	     (loop
	       (if* (null (wserver-accept-thread server))
		  then ; we are shutting down, exit thread
		       (return))
		 
	       (mp:with-process-lock ((pcache-cleaner-lock pcache))
		 (ignore-errors (cache-housekeeping)))
	       (sleep 30))))
       server))
    (setf (getf (mp:process-property-list (pcache-cleaner pcache))
		'short-name)
      (format nil "c~d" thx))
    )
  
  (publish :path "/cache-stats"
	   :function
	   #'(lambda (req ent)
	       (display-proxy-cache-statistics req ent pcache)))
  (publish :path "/cache-entries"
	   :function
	   #'(lambda (req ent)
	       (display-proxy-cache-entries req ent pcache)))
  
  (publish :path "/cache-entries-gc"
	   :function
	   #'(lambda (req ent)
	       (gc)
	       (display-proxy-cache-statistics req ent pcache)))
  
  (publish :path "/cache-entries-global-gc"
	   :function
	   #'(lambda (req ent)
	       (gc t)
	       (display-proxy-cache-statistics req ent pcache)))
  
	       
  )


(defun kill-proxy-cache (&key (server *wserver*))
  ;; kill off the cache and return all resources to the pool
  
  (let ((pcache (wserver-pcache server)))
    
    (if* (null pcache) then (return-from kill-proxy-cache))
    
    (mp:with-process-lock ((pcache-cleaner-lock pcache))
      ;; now we know that the other thread cleaning out
      ;; the cache won't call cache-housekeeping while we're
      ;; busy doing our business.
      
      ; this will signal the cache cleaner process to exit
      (setf (pcache-cleaner pcache) nil)
      
      ; clean out and remove the disk caches first
      (dolist (pcache-disk (pcache-disk-caches pcache))
	(flush-disk-cache pcache pcache-disk 0)
	(ignore-errors (close (pcache-disk-stream pcache-disk)))
	(ignore-errors (delete-file (pcache-disk-filename pcache-disk))))
      
      (setf (pcache-disk-caches pcache) nil)
      
      ; now clean the memory cache
      (flush-memory-cache pcache 0)
      
      ; and now return all the blocks on dead list
      (flush-dead-entries pcache))))



(defun configure-memory-cache (&key (server *wserver*)
				    size)
  ;; specify the desired size of the memory cache
  (let ((pcache (wserver-pcache server)))
    (if* (null pcache)
       then (error "There is no memory cache to size"))
    
    ; store it in blocks
    (setf (pcache-size pcache) (truncate size *header-block-size*))
    
    (setf (pcache-high-water pcache) (truncate (* .90 (pcache-size pcache))))
    (setf (pcache-low-water pcache) (truncate (* .80 (pcache-size pcache))))))

(defun add-disk-cache (&key (server *wserver*) 
			    filename 
			    (size #.(* 10 1024 1024)))
  (if* (null filename)
     then ; create a filename
	  (loop
	    (let ((name (format nil "acache-~x.acf" (random 34567))))
	      (if* (not (probe-file name))
		 then (setq filename name) 
		      (return)))))
  (let* ((blocks (truncate size *header-block-size*))
	 (pcache-disk (make-pcache-disk
		       :filename filename
		       :queueobj (make-and-init-queueobj)
		       :high-water (truncate (* .90 blocks))
		       :low-water  (truncate (* .80 blocks))
		       :blocks blocks
		       :free-blocks blocks
		       :free-list (list (cons 0 (1- blocks)))
		       :stream (open filename
				     :if-exists :supersede
				     :if-does-not-exist :create
				     :direction :io
				     #-(and allegro (version>= 6))
				     :element-type
				     #-(and allegro (version>= 6))
				     '(unsigned-byte 8)))))
    (push pcache-disk (pcache-disk-caches 
		       (wserver-pcache server)))
    filename))
	  
		      
  
    
    

(defun make-and-init-queueobj ()
  ; make a queue object with the dummy mru,lru entries
  (let ((q (make-queueobj
	    :mru (make-pcache-ent)
	    :lru (make-pcache-ent))))
    (setf (pcache-ent-next (queueobj-mru q)) (queueobj-lru q)
	  (pcache-ent-prev (queueobj-lru q)) (queueobj-mru q))
    q))


(defun display-proxy-cache-statistics (req ent pcache)
	
	
    
  ; count number of memory entries
  (let ((dead-bytes 0)
	(dead-blocks 0))
    (do ((ent (pcache-dead-ent pcache) (pcache-ent-next ent)))
	((null ent))
      (let ((this-data-length (pcache-ent-data-length ent))
	    (this-dead-blocks (pcache-ent-blocks ent)))
	(if* this-data-length
	   then (incf dead-bytes this-data-length))
	(if* this-dead-blocks
	   then (incf dead-blocks this-dead-blocks))))
      
    
    (with-http-response (req ent)
      (with-http-body (req ent)
	(html
	 (:html
	  (:head (:title "AllegroServe Proxy Cache Statistics"))
	  (:body 
	   (:h1 "AllegroServe Proxy Cache Statistics")
	   :p
	   "Show " ((:a :href "cache-entries") "Cache Entries,")
	   " Refresh " ((:a :href "cache-stats") "this page")
	   :br
	   "Perform a " ((:a :href "cache-entries-gc") "normal gc,")
	   " Perform a " ((:a :href "cache-entries-global-gc") "global gc")
	   :br
	   ((:table :border 2)
	    (:tr
	     (:th "Cache Action")
	     (:th "Count"))
		       
	    (dolist (ent
			'(("direct"   pcache-r-direct)
			  ("miss (user request)"     pcache-r-miss)
			  ("miss (anticipated request)"  pcache-r-cache-fill)
			  ("consistency miss" pcache-r-consistency-miss)
			  ("fast hit" pcache-r-fast-hit)
			  ("fast validation" pcache-r-fast-validation)
			  ("slow validation" pcache-r-slow-validation)))
	      (html
	       (:tr 
		(:td (:princ (car ent)))
		(:td (:princ (funcall (cadr ent) pcache)))))))

	   :br
	   ;; info on connection caching
	   "Connection caching is " 
	   (:princ (if* *connection-caching* 
		      then "enabled"
		      else "diabled"))
	   :br
	   ((:table :border 2)
	    (:tr
	     (:td "make-socket calls") (:td (:princ *connections-made*)))
	    (:tr
	     (:td "connections cached") (:td (:princ *connections-cached*)))
	    (:tr
	     (:td "cached connections used")
	     (:td (:princ *connections-used-cached*))))
	   
			 

	   :br
	   ((:table :border 2)
	    (:tr
	     (:th "Kind of Entry")
	     (:th "Count")
	     (:th "Bytes")
	     (:th "Blocks"))

	    (:tr
	     (:td "Live entries in memory")
	     (:td (:princ (queueobj-items (pcache-queueobj pcache)))
		  (:td (:princ (queueobj-bytes (pcache-queueobj pcache))))
		  (:td (:princ (queueobj-blocks (pcache-queueobj pcache)))
		       " ("
		       (:princ (* (queueobj-blocks (pcache-queueobj pcache)) 
				  *header-block-size*))
		
		       " bytes)")))
	    
	    (:tr
	     (:td "Dead entries in memory") 
	     (:td (:princ (pcache-dead-items pcache)))
	     (:td (:princ dead-bytes))
	     (:td (:princ dead-blocks)
		  " ("
		  (:princ (* dead-blocks *header-block-size*)) " bytes)"
		  ))
	    )
	   :br
	   "Memory Cache"
	   ((:table :border 2)
	    (:tr
	     (:th "items")
	     (:th "used-blocks/total-blocks")
	     (:th "bytes")
	     )
	    (let ((queueobj (pcache-queueobj pcache)))
	      (html (:tr 
		     (:td (:princ-safe (queueobj-items queueobj)))
		     (:td (:princ-safe (queueobj-blocks queueobj))
			  "/"
			  (:princ-safe (pcache-size pcache)))
		     (:td (:princ-safe (queueobj-bytes queueobj)))))))
	   :br
	   :br
	   "Disk Caches"
	   :br
	   ((:table :border 2)
	    (:tr
	     (:th "filename")
	     (:th "items")
	     (:th "blocks")
	     (:th "bytes")
	     (:th "free blocks")
	     (:th "free list")
	     )
	    (dolist (pcache-disk (pcache-disk-caches pcache))
	      (let ((queueobj (pcache-disk-queueobj pcache-disk)))
		(html (:tr 
		       (:td (:princ-safe (pcache-disk-filename pcache-disk)))
		       (:td (:princ-safe (queueobj-items queueobj)))
		       (:td (:princ-safe (queueobj-blocks queueobj)))
		       (:td (:princ-safe (queueobj-bytes queueobj)))
		       (:td (:princ-safe (pcache-disk-free-blocks
					  pcache-disk)))
		       (:td (:princ-safe (pcache-disk-free-list
					  pcache-disk))))))))
	   :br
	   "Current memory usage"
	   :br
	   (:pre (:princ-safe (with-output-to-string (*standard-output*)
				 (room t))))
			  
	   )))))))
				     
				     
(defun display-proxy-cache-entries (req ent pcache)
  ;; show all the proxy cache entries
  (let ((now (get-universal-time)))
    (flet ((display-pcache-ent (ent)
	     
	     (html (:b "uri: ")
		   (:princ-safe (pcache-ent-key ent))
		   :br
		   (if* (>= now (pcache-ent-expires ent))
		      then (html (:b ((:font :color "red")
				      "Stale -- ")))
		      else (html (:b ((:font :color "green")
				      "Fresh -- "))))
		   (:b "Expires: ")
		   (:princ-safe 
		    (universal-time-to-date (pcache-ent-expires ent)))
				
				      
		   :br
		   (:b "Last Modified: ")
		   (:princ-safe 
		    (universal-time-to-date (pcache-ent-last-modified ent)))
		   :br
		   (:b "Size: ")
		   (:princ (pcache-ent-data-length ent))
		   (:b ", State: ")
		   (:princ-safe (pcache-ent-state ent))
		   (:b ", Use: ")
		   (:princ-safe (pcache-ent-use ent))
		   (:b ", Code: ")
		   (:princ-safe (pcache-ent-code ent))
		   (if* (pcache-ent-disk-location ent)
		      then (html :br
				 (:b "Disk Location: "
				     (:princ-safe
				      (pcache-ent-disk-location ent)))))
		   :p
		   :br
		   )
	     
	     ))
      (with-http-response (req ent)
	(with-http-body (req ent)
	  (html
	   (:html
	    (:head (:title "Proxy Cache Entries"))
	    (:body
	     (:h1 "Proxy cache entries")
	     :p
	     "Here is a summary of " ((:a :href "cache-stats") "Cache Statistics.")
	     :br
	     "The current time is " (:princ-safe (universal-time-to-date now))
	     :br
	     :br

	     (let ((ent (pcache-ent-next (queueobj-mru 
					  (pcache-queueobj pcache))))
		   (last-ent (queueobj-lru (pcache-queueobj pcache))))
	       (loop
		 (if* (or (null ent)  (eq last-ent ent)) then (return))
		 (display-pcache-ent ent)
		 (setq ent (pcache-ent-next ent))))
	   
	     ; now display the disk caches
	     (dolist (pcache-disk (pcache-disk-caches pcache))
	       (html :hr 
		     :br
		     "Disk Cache: " (:princ-safe (pcache-disk-filename pcache-disk))
		     :br
		     "Free blocks: " (:princ-safe
				      (pcache-disk-free-blocks pcache-disk))
		     :br
		     "Free list: " (:princ-safe
				    (pcache-disk-free-list pcache-disk))
		     :br
		     :br)
	     
	       ; display the entries
	       (do ((ent (pcache-ent-next (queueobj-mru 
					   (pcache-disk-queueobj pcache-disk)))
			 (pcache-ent-next ent)))
		   ((or (null ent)
			(null (pcache-ent-key ent))))
	       
		 (display-pcache-ent ent)))
		
	    
	     ))))))))

#+ignore
(defun verify-memory-cache (tag)
  ;; verify that all memory cache items are not on the free list too
  
  (let ((pcache (wserver-pcache *wserver*)))
    (let ((ent (pcache-ent-next (queueobj-mru 
				 (pcache-queueobj pcache))))
	  (last-ent (queueobj-lru (pcache-queueobj pcache))))
      (loop
	(if* (or (null ent)  (eq last-ent ent)) then (return))
	;; test to see if block are on the free list
	  (setq *bug* ent)
	(dolist (db (pcache-ent-data ent))
	  (chk-header-block db tag))
	(setq ent (pcache-ent-next ent))))
    ))
  

; was 'proxy-cache-request'

(defun check-cache-then-proxy-request (req ent respond level)
  ;; if we've got a proxy cache then retrieve it from there
  ;; else just proxy the request
  
  ;; respond is true if we really want to respond, it will be 
  ;; nil if we just want to ensure that what we need is in the cache
  ;;
  
  (let ((pcache (wserver-pcache *wserver*))
	(rendered-uri))
    
    (if* (or (null pcache)
	     ; should handle :head requests too
	     (not (eq (request-method req) :get))
	     ; don't look in cache if request has cookies
	     (and (header-slot-value req :authorization)
		  (progn 
		    (logmess "authorization forces direct")
		    t))
	     )
       then (if* pcache then (incf (pcache-r-direct pcache)))
	    (return-from check-cache-then-proxy-request
	      (proxy-request req ent :respond respond)))

    ; clear out the fragment part (after the #) so that we don't match
    ; on that.
    (setf (net.uri:uri-fragment (request-raw-uri req)) nil)
    (setq rendered-uri
      (transform-uri (net.uri:render-uri (request-raw-uri req) nil)))
      
    
    (dlogmess (format nil "cache: look in cache for ~a, level ~d, ents ~d~%" 
		      rendered-uri
		      level
		      (length (gethash rendered-uri (pcache-table pcache)))
		      ))

    (dolist (pcache-ent (gethash rendered-uri (pcache-table pcache))
	      ; not found, must proxy and then cache if the
	      ; result looks good
	      (progn
		(dlogmess (format nil "not in cache, proxy it level ~d" level))
		(if* (eql *browser-level* level)
		   then (incf (pcache-r-miss pcache))
			(log-proxy rendered-uri level :mi nil)
		   else (incf (pcache-r-cache-fill pcache))
			(log-proxy rendered-uri level :pf nil))
		
			
		(proxy-and-cache-request req ent (get-universal-time) 
					 nil respond level)))
      (if* (lock-pcache-ent pcache-ent)
	 then (unwind-protect
		  (if* (equal (pcache-ent-cookie pcache-ent)
			      (header-slot-value req :cookie))
		     then ; can use this one
			     
			  (multiple-value-bind (response new-pcache-ent)
			      (use-value-from-cache req ent pcache-ent
						    level respond)

			      
			    ; run hook only if we're at the browser level
			    ; (since this will allow us to flush the queue)
			    ; of if this wasn't a fast hit and thus there
			    ; is new data to cache
			    (if* (and (pcache-entry-cached-hook pcache)
				      (or (eql level *browser-level*)
					  (not (member response '(:fh :fv :sh)
						       :test #'eq))))
			       then ; we want to do link scanning of this
				    (if* (null new-pcache-ent)
				       then (setq new-pcache-ent 
					      pcache-ent))
				    
				    (if* (lock-pcache-ent new-pcache-ent)
				       then ; it will be unlocked by the hook fcn
					    (funcall (pcache-entry-cached-hook pcache)
						     pcache
						     new-pcache-ent level))))
			  (return)
		     else (dlogmess 
			   (format nil "can't use cached ~s due to cookie difference~%"
				   rendered-uri))
			  (dlogmess (format nil
					    "cached cookie ~s~%, current cookie: ~s~%" 
					    (pcache-ent-cookie pcache-ent)
					    (header-slot-value req :cookie))))
				 
		(unlock-pcache-ent pcache-ent))
	 else (logmess "entry could not be locked~%")))))



(defun proxy-and-cache-request (req ent now pcache-ent respond level)
  ;; must send the request to the net via the proxy.
  ;; if pcache-ent is non-nil then this is the existing
  ;; cache entry which may get updated or killed.
  ;; 
  ;; return the reponse code from the proxy call
  ;; return the new pcache-ent that got the new value if the return
  ;;  code is 200 or 302
  ;;
  ;;
  (let ((new-ent (make-pcache-ent))
	(rendered-uri 
	 (transform-uri (net.uri:render-uri (request-raw-uri req) nil)))
	(pcache (wserver-pcache *wserver*)))
    
    (setf (pcache-ent-key new-ent) rendered-uri
	  (pcache-ent-uri new-ent) (request-raw-uri req))
    
    (proxy-request req ent :pcache-ent new-ent :respond respond :level level)
    (if* (member (pcache-ent-code new-ent) '(200 
					     302 ; redirect
					     ))
       then ; turns out it was modified, must
	    ; make this the new entry

	    (if* pcache-ent
	       then (dlogmess (format nil "replace cache entry for ~a"
				     rendered-uri)))
	    
	    
	    (push new-ent
		  (gethash rendered-uri
			   (pcache-table pcache)))

	    ; put at the head of the memory queue
	    ; could already be dead from some other threads o
	    ; be careful
	    (if* (lock-pcache-ent new-ent)
	       then (if* (not (eq (pcache-ent-state new-ent) :dead))
		       then (move-pcache-ent new-ent nil 
					     (pcache-queueobj pcache)))
		    (unlock-pcache-ent new-ent))
			      
	    ; and disable the old entry
	    (if* pcache-ent
	       then (kill-pcache-ent pcache-ent pcache))
	    
     elseif (and pcache-ent
		 (eq (pcache-ent-code new-ent) 304))
		 
       then ; still not modified, recompute the 
	    ; expiration time since we now know
	    ; that the item is older
	    ;* this may end up violation the expiration
	    ; time in a header from a previous call
	    (dlogmess (format nil "change expiration date for ~a"
			     rendered-uri))      
	    (setf (pcache-ent-expires pcache-ent)
	      (max (pcache-ent-expires pcache-ent)
		   (compute-approx-expiration
		    (pcache-ent-last-modified pcache-ent)
		    now))))
    
    (values (pcache-ent-code new-ent) new-ent)))
  


(defun use-value-from-cache (req ent pcache-ent level respond)
  ;; we've determined that pcache-ent matches the request.
  ;; now deal with the issue of it possibily being out of date
  ;;
  ;; return a keyword specifying the kind of access that was
  ;; done
  ;;
  (let* ((ims (header-slot-value req :if-modified-since))
	 (now (get-universal-time))
	 (pcache (wserver-pcache *wserver*))
	 (fresh))
    
    (most-recently-used-ent pcache-ent)
    
    (dlogmess (format nil "ims is ~s" ims))

    ; compute if the entry is fresh or stale
    (setq fresh (<= now (pcache-ent-expires pcache-ent)))
    
    
    (dlogmess (format nil "ims is ~s, fresh by ~s seconds" ims 
		      (-  (pcache-ent-expires pcache-ent) now)))
    
    
    (if* (and ims (not (equal "" ims)))
       then ; we're in a conditional get situation, where the
	    ; condition is If-Modified-Since
	    (setq ims (date-to-universal-time ims))
	    
	    
	    (if* fresh
	       then (if* (< ims (pcache-ent-last-modified pcache-ent))
		       then ; it has been modified since the ims time
			    ; must return the whole thing
			    (dlogmess "validation->fast hit")
			    (incf (pcache-r-fast-hit pcache))
			    (if* respond
			       then (send-cached-response req pcache-ent))
			    (log-proxy (pcache-ent-key pcache-ent)
				       level
				       :fh
				       (- (pcache-ent-last-modified pcache-ent)
					  ims))
			    :fh
		       else ; it hasn't been modified since the ims time
			    (dlogmess "fast validation")
			    (incf (pcache-r-fast-validation pcache))
			    (if* respond
			       then (send-not-modified-response req ent))
			    (log-proxy (pcache-ent-key pcache-ent)
				       level
				       :fv
				       (- (pcache-ent-last-modified pcache-ent)
					  ims))
			    :fv
			    )
	       else ; stale, must revalidate
		    (let ((code
			   (proxy-and-cache-request req ent 
						    now pcache-ent respond
						    level)))
		      (if* (eql code 304)
			 then (dlogmess "slow validation")
			      (incf (pcache-r-slow-validation pcache))
			      (log-proxy (pcache-ent-key pcache-ent)
					 level
					 :sv
					 (- (pcache-ent-last-modified pcache-ent)
					    ims))
			      :sv 
			 else (dlogmess "consistency miss")
			      (incf (pcache-r-consistency-miss
				     pcache))
			      (log-proxy (pcache-ent-key pcache-ent)
					 level
					 :cm
					 (- (pcache-ent-last-modified pcache-ent)
					    ims))
			      :cm
			      )))
			    
       else ; unconditional get
	    (if* fresh
	       then (dlogmess "fast hit")
		    (incf (pcache-r-fast-hit pcache))
		    (if* respond then (send-cached-response req pcache-ent))
		    (log-proxy (pcache-ent-key pcache-ent)
			       level
			       :fh
			       nil)
		    :fh
	       else ; issue a validating send
		    
		    (insert-header 
		     (request-header-block req)
		     :if-modified-since
		     (or (pcache-ent-last-modified-string pcache-ent) 
			 (setf (pcache-ent-last-modified-string pcache-ent)
			   (universal-time-to-date
			    (pcache-ent-last-modified pcache-ent)))))
		    
		    (multiple-value-bind (code new-pcache-ent)
			   (proxy-and-cache-request req ent now pcache-ent nil
						    -1)
		      ; we didn't respond just sent out a probe
		      (multiple-value-prog1
			  (if* (eql code 304)
			     then ; hasn't been modified since our 
				  ; cached entry
				  (dlogmess "slow hit")
				  (incf (pcache-r-slow-hit pcache))
				  (log-proxy (pcache-ent-key pcache-ent)
					     level
					     :sh
					     nil)
				  :sh
				  
			     else ; was modified, so new item is cached
				  (setq pcache-ent new-pcache-ent)
				  (dlogmess "consistency miss")
				  (incf (pcache-r-consistency-miss pcache))
				  (log-proxy (pcache-ent-key pcache-ent)
					     level
					     :cm
					     nil)
				  (values :cm pcache-ent)
				  )
			(if* respond 
			   then (send-cached-response req pcache-ent))))))))


		    
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
  (dlogmess (format nil "cache: sending back cached response: ~a, length ~d~%" 
		   (net.uri:render-uri (request-raw-uri req) nil)
		   (pcache-ent-data-length pcache-ent)))
  (incf (pcache-ent-returned pcache-ent))


  (if* (pcache-ent-disk-location pcache-ent)
     then (retrieve-pcache-from-disk pcache-ent))
  
  
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
  (with-fast-mp-locked-pcache-ent (pcache-ent)
    (excl::fast
     (let ((val (pcache-ent-use pcache-ent)))
       (if* val
	  then (setf (pcache-ent-use pcache-ent) 
		  (the fixnum (1+ (the fixnum val)))))))))

(defun unlock-pcache-ent (pcache-ent)
  ;; reduce the use count of this entry
  (with-mp-locked-pcache-ent (pcache-ent)
    (let ((val (pcache-ent-use pcache-ent)))
      (if* val
	 then (if* (and (zerop (excl::fast
				(decf (the fixnum val))))
			(eq (pcache-ent-state pcache-ent) :dead))
		 then (setf (pcache-ent-use pcache-ent) nil)
		 else (setf (pcache-ent-use pcache-ent) val))))))

(defun most-recently-used-ent (pcache-ent)
  ;; make this entry the most recently used in whatever queue it's on
  (let ((queueobj (pcache-ent-queueobj pcache-ent)))
    (move-pcache-ent pcache-ent queueobj queueobj)))

#+ignore ;; fixed below for thread safety
(defun move-pcache-ent (pcache-ent fromq toq)
  ;; move the pcache-ent between queues
  ;; fromq and toq can be nil or the same.
  ;;
  (let ((prev (pcache-ent-prev pcache-ent))
	(next (pcache-ent-next pcache-ent)))
    
    (mp:without-scheduling
      ; unlink
      (if* (and prev next)
	 then (setf (pcache-ent-next prev) next
		    (pcache-ent-prev next) prev))
    
      (if* (and fromq (not (eq fromq toq)))
	 then ; must update counts in the from queue
	      (decf (queueobj-items fromq))
	      (decf (queueobj-bytes fromq) (pcache-ent-data-length pcache-ent))
	      (decf (queueobj-blocks fromq)
		    (pcache-ent-blocks pcache-ent)))
    
      ; link into the toq, at the mru position
      (if* toq
	 then ;debugging
	      (if* (eq (pcache-ent-state pcache-ent) :dead)
		 then (break "shouldn't be dead during move"))
	      
	      (let* ((mru-head (queueobj-mru toq))
		     (mru (pcache-ent-next mru-head)))
		(setf (pcache-ent-next mru-head) pcache-ent
		      (pcache-ent-prev pcache-ent) mru-head
		      (pcache-ent-next pcache-ent) mru
		      (pcache-ent-prev mru) pcache-ent))
	      (if* (not (eq fromq toq))
		 then ; increment counts
		      (incf (queueobj-items toq))
		      (incf (queueobj-bytes toq) 
			    (pcache-ent-data-length pcache-ent))
		      (incf (queueobj-blocks toq)
			    (pcache-ent-blocks pcache-ent))))
      
      (setf (pcache-ent-queueobj pcache-ent) toq))))

;; This is the new version, fixed for thread safety
(defun move-pcache-ent (pcache-ent fromq toq)
  ;; move the pcache-ent between queues
  ;; fromq and toq can be nil or the same.
  ;;
  ;; to handle errors detected inside a locked block, we
  ;; set up a flag that we can check after leaving the locked code
  (let ((error-flag nil))
    (if* (eq fromq toq)
       then ;; just shifting it to the head of the queue it's in
	    (if* (null fromq)
	       then (setq error-flag :noqueue)
	     elseif (eq (pcache-ent-state pcache-ent) :dead) ; debugging 
	       then (setq error-flag :dead)
	       else (with-mp-locked-pcache-queue (fromq)
		      (let* ((prev (pcache-ent-prev pcache-ent))
			     (next (pcache-ent-next pcache-ent))
			     (mru-head (queueobj-mru toq))
			     (mru (pcache-ent-next mru-head)))
			;; unlink and relink, or report error
			(if* (or (null prev) (null next))
			   then (setq error-flag :unlinked)
			   else (setf (pcache-ent-next prev) next
				      (pcache-ent-prev next) prev
				      (pcache-ent-next mru-head) pcache-ent
				      (pcache-ent-prev pcache-ent) mru-head
				      (pcache-ent-next pcache-ent) mru
				      (pcache-ent-prev mru) pcache-ent)))))
       else ;; toq != fromq; we're changing one or more queue counts
	    (if* fromq
	       then ;; it's leaving the queue fromq
		    (with-mp-locked-pcache-queue (fromq)
		      (let ((prev (pcache-ent-prev pcache-ent))
			    (next (pcache-ent-next pcache-ent)))
			;; unlink
			(if* (or (null prev) (null next))
			   then (setq error-flag :unlinked)
			   else (setf (pcache-ent-next prev) next
				      (pcache-ent-prev next) prev)
				(decf (queueobj-items fromq))
				(decf (queueobj-bytes fromq)
				      (pcache-ent-data-length pcache-ent))
				(decf (queueobj-blocks fromq)
				      (pcache-ent-blocks pcache-ent))))))
	    (if* (and (null error-flag) toq)
	       then ;; link into the toq, at the mru position
		    (if* (eq (pcache-ent-state pcache-ent) :dead)
		       then (setq error-flag :dead)
		       else (with-mp-locked-pcache-queue (toq)
			      (let* ((mru-head (queueobj-mru toq))
				     (mru (pcache-ent-next mru-head)))
				(setf (pcache-ent-next mru-head) pcache-ent
				      (pcache-ent-prev pcache-ent) mru-head
				      (pcache-ent-next pcache-ent) mru
				      (pcache-ent-prev mru) pcache-ent))
			      (incf (queueobj-items toq))
			      (incf (queueobj-bytes toq) 
				    (pcache-ent-data-length pcache-ent))
			      (incf (queueobj-blocks toq)
				    (pcache-ent-blocks pcache-ent)))))
	    (if* (null error-flag)
	       then (setf (pcache-ent-queueobj pcache-ent) toq)))
    ;; we've either made changes or set error-flag non-nil
    (case error-flag
      (:noqueue
       (error "attempt to move a pcache-ent without specifying a queue"))
      (:dead (break "shouldn't be dead during move"))
      (:unlinked
       (error "attempt to shift an unlinked pcache-ent")))))
		      


(defun kill-pcache-ent (pcache-ent &optional (pcache (wserver-pcache
						      *wserver*)))
  ; make this entry dead
  (with-mp-locked-pcache (pcache)
    
    ; stop any scanning of this uri
    (setf (pcache-ent-level pcache-ent) -1) 
    
    (let ((state (pcache-ent-state pcache-ent)))
      (if* (not (eq :dead state))
	 then ; make it dead
	      (setf (pcache-ent-state pcache-ent) :dead)

	      (move-pcache-ent pcache-ent
			       (pcache-ent-queueobj pcache-ent)
			       nil ; move to nowhere, 
			       )
		
	      ; link onto the dead list
	      (setf (pcache-ent-next pcache-ent) (pcache-dead-ent pcache)
		    (pcache-dead-ent pcache) pcache-ent)
	      
	      ; if currently not in use, then make sure it's never used
	      (if* (eql 0 (pcache-ent-use pcache-ent))
		 then (setf (pcache-ent-use pcache-ent) nil))
		
	      ;; stats
	      (incf (pcache-dead-items pcache))
		
	      ; remove from hash table
	      (let ((ents (gethash (pcache-ent-key pcache-ent)
				   (pcache-table pcache))))
		(setf (gethash (pcache-ent-key pcache-ent)
			       (pcache-table pcache))
		  (delete pcache-ent ents :test #'eq)))
		  
		
	      ))))

	      
     

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


    

(defun cache-response (req pcache-ent 
		       response-code comment client-response-header
		       body-buffers body-length level)
  
  ;; we are caching, save the information about this response 
  ;; in the pcache-ent we are passed, which should be blank
  
	    
  (dlogmess (format nil "cache: caching response to ~a, code ~d, length ~d~%" 
		    (net.uri:render-uri (request-raw-uri req) nil)
		    response-code
		    body-length
		    ))
  
  (let (now uri-info
	(pcache (wserver-pcache *wserver*)))
    (if* (or (eql response-code 200)
	     (eql response-code 302) ; redirect
	     )
       then ; full response
	    
	    (setf (pcache-ent-code pcache-ent) response-code)
	    (setf (pcache-ent-comment pcache-ent) comment)
	  
	    (setf (pcache-ent-data pcache-ent) 
	      (cons client-response-header body-buffers))
	    
	    (setf (pcache-ent-data-length pcache-ent) body-length
		  
		  (pcache-ent-blocks pcache-ent) 
		  (length (pcache-ent-data pcache-ent)))
	  

	    (setf (pcache-ent-cookie pcache-ent)
	      (header-slot-value req :cookie))
	  
	    (setf (pcache-ent-state pcache-ent) nil ; means valid data
		  (pcache-ent-use  pcache-ent) 0)

	    (setq uri-info (find-uri-info (request-raw-uri req)))
	    
	    (if* uri-info
	       then (dlogmess (format nil "have uri info for ~a"
				      (request-raw-uri req))))
	    
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
			 (or now (get-universal-time)))))
	      
	      ;; add extra lifetime for certain entries
	      (incf (pcache-ent-expires pcache-ent)
		    (if* (and uri-info (uri-info-extra-lifetime uri-info))
		       then (uri-info-extra-lifetime uri-info)
		       else *extra-lifetime*))
	      
	      (if* (pcache-entry-cached-hook pcache)
		 then (if* (lock-pcache-ent pcache-ent)
			 then ; it will be unlocked by the hook fcn
			      (funcall (pcache-entry-cached-hook pcache)
				       pcache pcache-ent level))))
		       
     elseif (eql response-code 304)
       then ; just set that so the reader of the response will know
	    ; the result
	    (setf (pcache-ent-code pcache-ent) response-code))
  
    (if* client-response-header 
       then (free-header-block client-response-header))
  
    (if* body-buffers
       then (free-header-blocks body-buffers))))


; --- cleaning out old entries


(defun cache-housekeeping (&optional (pcache (wserver-pcache *wserver*)))
  ;; bring all the caches to within the appropriate tolerance
  
  ; first clean out disk caches so we can put more memory stuff in them
  (dolist (pcache-disk (pcache-disk-caches pcache))
    (if* (> (- (pcache-disk-blocks pcache-disk)
	       (pcache-disk-free-blocks pcache-disk))
	    (pcache-disk-high-water  pcache-disk))
       then ; must flush it
	    (flush-disk-cache pcache
			      pcache-disk 
			      (pcache-disk-low-water  pcache-disk))))
  
  ; clear out all entries made dead
  (flush-dead-entries pcache)

  ; now clean out the memory cache if needed by moving to a disk cache
  (if* (> (queueobj-blocks (pcache-queueobj pcache))
	  (pcache-high-water pcache))
     then (flush-memory-cache pcache
			      (pcache-low-water pcache)))
  
  ; now rotate caches so they are evenly used
  (let ((caches (pcache-disk-caches pcache)))
    (if* (cdr caches)
       then (let ((new (cdr caches)))
	      (setf (cdr caches) nil)
	      (nconc new caches)
	      (setf (pcache-disk-caches pcache) new)))))


(defun flush-disk-cache (pcache pcache-disk goal)
  ;; flush entries from the disk cache until the number of blocks
  ;; is less than or equal to the goal
  (let* ((needed (- (- (pcache-disk-blocks pcache-disk) 
		       (pcache-disk-free-blocks pcache-disk))
		    goal))
	 (queueobj (pcache-disk-queueobj pcache-disk))
	 (mru-head (queueobj-mru queueobj))
	 (lru-head (queueobj-lru queueobj)))
    (loop
      (if* (<= needed 0)
	 then (return))
      
      ; pick off the lru and kill it
      (mp::with-process-lock ((pcache-disk-lock pcache-disk))
	(let ((lru (pcache-ent-prev lru-head)))
	  (if* (not (eq lru mru-head))
	     then ; a legit block
		  (dlogmess (format nil "kill ~s from disk queue"
				   (pcache-ent-key lru)))
		  (decf needed (pcache-ent-blocks lru))
		  (kill-pcache-ent lru pcache)
		  (log-proxy (pcache-ent-key lru) 0 :kd nil)
	     else (return) ; no more left ? shouldn't happen
		  ))))))


(defun flush-memory-cache (pcache goal)
  ;; move memory cache items to a disk cache if possible
  
  (if* (null pcache)
     then (setq pcache (wserver-pcache *wserver*)))
  
  (let* ((needed (- (queueobj-blocks (pcache-queueobj pcache))
		    goal))
	 (queueobj (pcache-queueobj pcache))
	 (mru-head (queueobj-mru queueobj))
	 (lru-head (queueobj-lru queueobj))
	 (disk-caches (pcache-disk-caches pcache))
	 (ent-todo)
	 )
    
    (loop
      (if* (<= needed 0) then (return))
      #+ignore ;; recoded below
      (block main
	(setq ent-todo nil)
	
	(mp:without-scheduling
	  ;; find the next ent to process without other processes running
	  (let ((lru lru-head))
	    (loop
	      (setq lru (pcache-ent-prev lru))
	      (if* (eq lru mru-head) 
		 then (setq needed 0) 
		      (return-from main))
	      (if* (lock-pcache-ent lru)
		 then (setq ent-todo lru)
		      (return-from main))))))
      
      ;; this is the new version
      (with-mp-locked-pcache (pcache)
	(setq ent-todo nil)
	(do ((lru (pcache-ent-prev lru-head) (pcache-ent-prev lru)))
	    ((eq lru mru-head)
	     (setq needed 0))
	  (if* (lock-pcache-ent lru)
	     then (setq ent-todo lru)
		  (return))))
      
      (if* ent-todo
	 then ; move this one to disk or kill it off
	      (if* (dolist (dc disk-caches t)
		     (if* (move-ent-to-disk ent-todo dc)
			then ; successful move to disk
			     (decf needed (pcache-ent-blocks ent-todo))
			     (unlock-pcache-ent ent-todo)
			     (return nil)))
		 then ; can't put on disk.  kill it in memory
		      (decf needed (pcache-ent-blocks ent-todo))
		      (kill-pcache-ent ent-todo pcache)
		      (unlock-pcache-ent ent-todo))))))


		  
(defun flush-dead-entries (pcache)
  ;; flush all the deal items from the cache, returning
  ;; their resource
  (let (ent)
    (smp-case
     (nil
      (atomically-fast
       (setf ent (pcache-dead-ent pcache)
	     (pcache-dead-ent pcache) nil)))
     ((t :macros)
      (loop
	(setq ent (pcache-dead-ent pcache))
	(excl::fast
	 (if* (atomic-conditional-setf (pcache-dead-ent pcache) nil ent)
	    then (return))))))
    
    ; now we have an exclusive link to the dead entries
    ; which we can free at our leisure
    (let ((count 0))
      (loop
	(if* (null ent) then (return))
	(incf count)
	(free-header-blocks (pcache-ent-data ent))
	(let ((diskloc (pcache-ent-disk-location ent)))
	  ; if stored on the disk, free those blocks
	  (if* diskloc
	     then (return-free-blocks (pcache-ent-pcache-disk ent)
				      diskloc)))
	(setq ent (pcache-ent-next ent)))
      (smp-case
       (nil
	(atomically-fast
	 (decf (the fixnum (pcache-dead-items pcache)) 
	       (the fixnum count))))
       ((t :macros)
	(excl::fast
	 (decf-atomic (the fixnum (pcache-dead-items pcache)) 
		      (the fixnum count)))))
      )))
     
     
  

      
(defun empty-all-caches (&key (server *wserver*))
  ;; remove everything from all caches
  
  (let ((pcache (wserver-pcache server)))
    (mp:with-process-lock ((pcache-cleaner-lock pcache))
      (flush-dead-entries pcache)
      (flush-memory-cache pcache 0) ; empty memory
      (dolist (dcache (pcache-disk-caches pcache))
	(flush-disk-cache pcache dcache 0)))))




  
  
    
  


	    
	    



;------------ disk cache
  
(defun move-ent-to-disk (pcache-ent pcache-disk)
  ;; copy the given pcache-ent to the disk
  ;; assume that we've locked it at this point
  ;;
  ;; return t if we suceede and nil if we didn't
  ;;
  (if* (pcache-ent-disk-location pcache-ent)
     then (dlogmess (format nil "cached ~s is already on the disk"
			   (pcache-ent-key pcache-ent)))
	  (return-from move-ent-to-disk t))
  
  (let ((to-store-list (get-disk-cache-blocks 
			pcache-disk (pcache-ent-blocks pcache-ent)))
	(buffs))
    
    (if* to-store-list
       then (dlogmess (format nil "store ~s on disk at ~s~%"
			     (pcache-ent-key pcache-ent)
			     to-store-list))
	    (store-data-on-disk pcache-ent pcache-disk to-store-list)
	    (log-proxy (pcache-ent-key pcache-ent) 0 :wd nil)
	    (let ((ans
		   (with-mp-locked-pcache-ent (pcache-ent)
		     (if* (and (null (pcache-ent-state pcache-ent))
			       (eql 1 (pcache-ent-use pcache-ent)))
			then ; we are tre sole user of this entry so we cna
			     ; replace the buffers with the disk location
			     (setf (pcache-ent-disk-location pcache-ent) 
			       to-store-list
			       
			       (pcache-ent-pcache-disk pcache-ent) 
			       pcache-disk
			       
			       buffs (pcache-ent-data pcache-ent)
			       
			       (pcache-ent-data pcache-ent) nil)
		     

			     ; move to disk's list
			     (move-pcache-ent pcache-ent
					      (pcache-ent-queueobj pcache-ent)
					      (pcache-disk-queueobj
					       pcache-disk))
		
		     
					    
			     t
		     
			else ; someone started using the entry.. so forget we 
			     ; wrote it
			     (logmess 
			      (format nil "can't complete store: use ~d, state ~s~%"
				      (pcache-ent-use pcache-ent)
				      (pcache-ent-state pcache-ent)))
				      
			     (return-free-blocks pcache-disk to-store-list)
		     
			     nil))))
    
	      (free-header-blocks buffs)
	      ans))))

(defun ensure-pcache-in-memory (pcache-ent)
  (smp-case
   (t
    (with-locked-structure (pcache-ent)
      (if* (pcache-ent-disk-location pcache-ent)
	 then (retrieve-pcache-from-disk pcache-ent))))
   ((nil :macros)
    (if* (pcache-ent-disk-location pcache-ent)
       then (retrieve-pcache-from-disk pcache-ent)))))


(defun retrieve-pcache-from-disk (pcache-ent)
  ;; read the cache entry back in from the disk
  
  ;; This is now called with the pcache-ent locked in smp lisps,
  ;; so the flagval test isn't needed in smp at all
  
  ; ensure the loading flag to true and set flagval
  ; to the value before we set the flag.
  ; If the value was nil and thus we set it to true, then
  ; we are the process responsible for loading in the data
  ;
  #-smp
  (let ((flagval (atomically-fast
		  (let ((val (pcache-ent-loading-flag pcache-ent)))
		    (if* (null val) 
		       then (setf (pcache-ent-loading-flag pcache-ent) t))
		    val))))
    (if* flagval
       then (mp:process-wait "cache entry to be loaded"
			     #'(lambda (pcache-ent) 
				 (null (pcache-ent-loading-flag pcache-ent)))
			     pcache-ent)
	    (return-from retrieve-pcache-from-disk))
    )
  ;; it's our job to load in the entry
  (let* ((block-list (pcache-ent-disk-location pcache-ent))
	 (pcache-disk (pcache-ent-pcache-disk pcache-ent))
	 (stream (pcache-disk-stream pcache-disk))
	 (bytes (+ (pcache-ent-data-length pcache-ent)
		   *header-block-size*))
	 (res))
    (dlogmess (format nil "retrieve ~s in blocks ~s~%"
		      (pcache-ent-key pcache-ent)
		      block-list))
    (log-proxy (pcache-ent-key pcache-ent) 0 :rd nil)
    
    (mp:with-process-lock ((pcache-disk-lock pcache-disk))
      ;; get a lock so we're the only thread doing operations
      ;; on the stream to the cache
      (dolist (ent block-list)
	(file-position stream (* (car ent) *header-block-size*))
	(dotimes (i (1+ (- (cdr ent) (car ent))))
	  (let ((buff (get-header-block)))
	    (read-sequence buff stream :end (min *header-block-size*
						 bytes))
	    (decf bytes *header-block-size*)
	    (push buff res))))
      (setf (pcache-ent-data pcache-ent) (nreverse res))
      

      (return-free-blocks pcache-disk block-list)
	
      ;; insert in the memory ru list
      (most-recently-used-ent pcache-ent)
    
      ;; insert in memory 
	
      (atomically-fast
       (setf (pcache-ent-disk-location pcache-ent) nil
	     (pcache-ent-pcache-disk pcache-ent) nil
	     (pcache-ent-loading-flag pcache-ent) nil)))))

	
	
      
      
    
    
    
			     
		    
(defun get-disk-cache-blocks (pcache-disk count)
  ;; return the location of count cache blocks
  
  (mp:with-process-lock ((pcache-disk-lock pcache-disk))
    (let ((free (pcache-disk-free-blocks pcache-disk)))
      (decf free count)
      (if* (>= free 0)
	 then (setf (pcache-disk-free-blocks pcache-disk) free)
	      ; now find that many blocks
	      (let ((free-list (pcache-disk-free-list pcache-disk))
		    (toret))
		(loop
		  (let ((ent (car free-list)))
		    (if* (null ent)
		       then ; should not have run out.. this is bad
			    (return-from get-disk-cache-blocks nil)
		       else (let ((amt (1+ (- (cdr ent) (car ent)))))
			      (if* (< amt count)
				 then ; need this and more
				      (push ent toret)
				      (decf count amt)
				      (pop free-list)
			       elseif (eql amt count)
				      ; perfect
				 then (push ent toret)
				      (pop free-list)
				      (return)
				 else ; too many, take what we need
				      (push (cons (car ent)
						  (+ (car ent)
						     count 
						     -1))
					    toret)
				      (incf (car ent) count)
				      (return))))))
		(setf (pcache-disk-free-list pcache-disk) free-list)
		toret)))))
		
				      

(defun return-free-blocks (pcache-disk list-of-blocks)
  ;; return the given blocks to the free list
  ;; list of block is a list of conses (start . end)
  ;; and we must insert them in the free list which has the
  ;; same form, and we want to merge blocks too.
  (mp:with-process-lock ((pcache-disk-lock pcache-disk))
    (let ((giveback 0)
	  (free-list (pcache-disk-free-list pcache-disk)))
      (dolist (ent list-of-blocks)
	(incf giveback (1+ (- (cdr ent) (car ent))))
	(do ((prev nil cur)
	     (cur free-list (cdr cur)))
	    ((null cur)
	     ; add at end of the line 
	     (if* prev
		then (setf (cdr prev) (list ent))
		else ; only thing
		     (setq free-list (list ent))))
	  (if* (< (cdr ent) (caar cur))
	     then ; fit it in between prev and cur
		  ; we know that it's not adjacent to the previous entry
		  ; see if adjacent to this cur entry
		  (if* (eql (1+ (cdr ent)) (caar cur))
		     then ; adjacent, just adjust that one
			  (setf (caar cur) (car ent))
			  
		     else ; not adjacent, link it in
			  (if* prev
			     then (setf (cdr prev) (cons ent cur))
			     else (setq free-list
				    (cons ent cur))))
		  (return)
	   elseif (eql (1+ (cdar cur)) (car ent))
	     then ; is adjacent at the right end to cur
		  (setf (cdar cur) (cdr ent))
		  ; see if cur now joins with the one after cur
		  (setq prev cur 
			cur (cdr prev))
		  (if* (and cur (eql (1+ (cdar prev)) (caar cur)))
		     then ; it does 
			  (setf (cdar prev) (cdar cur))
			  (setf (cdr prev) (cdr cur)))
			  
		  (return))))
      (setf (pcache-disk-free-list pcache-disk) free-list)
      (incf (pcache-disk-free-blocks pcache-disk) giveback))))

      

(defun store-data-on-disk (pcache-ent pcache-disk list-of-blocks)
  ;; store the data in the pcache-ent to the disk using
  ;; the blocks in list-of-blocks (list of cons format)
  ;;
  (let ((buffers (pcache-ent-data pcache-ent))
	(stream (pcache-disk-stream pcache-disk))
	(bytes (+  *header-block-size*  ; for header block
		   (pcache-ent-data-length pcache-ent))))
    (dlogmess (format nil "writing ~d buffers to list ~d~%" 
		     (length buffers)
		     list-of-blocks))
    (dolist (ent list-of-blocks)
      ; prepare to write
      (file-position stream (* (car ent) *header-block-size*))
      
      (dotimes (i (1+ (- (cdr ent) (car ent))))
	(if* (null buffers)
	   then (error "ran out of buffers before blocks"))
	(let ((length (min *header-block-size* bytes)))
	  (if* (> length 0)
	     then (write-sequence (car buffers) stream :end length)))
	(pop buffers)
	(decf bytes *header-block-size*)))))
    

				      
		    
		    
		    
			    
		
;--- end disk cache  
  
;--- uri transforms 

(defparameter *uri-transforms* 
    ;; list of functions that take a string and if they make a change
    ;; return a string (else they return nil)
    nil)


(defun transform-uri (string)
  ;; transform the string
  (let (ans)
    (dolist (tr *uri-transforms* string)
      (if* (setq ans (funcall tr string))
	 then (return ans)))))


; define sample transform
(defun add-transform (function)
  (pushnew function *uri-transforms*))



;--- specification of uri handling by proxy
(defun handle-uri (host path
		   &key (server *wserver*)
			(extra-lifetime nil el-p)
			(scan-depth nil sd-p)
			(exclude nil ex-p)
			(scan-function nil sf-p)
			(offsite t ofs-p)
			)
  
  ;; store info about how to handle this uri during the scan

  ;; host can be "foo.com" or "foo.com:8000"
  ;;
  
  (let ((pcache (wserver-pcache server))
	(table)
	(uri-info)
	(ahost)
	(aport)
	)
    (if* (null pcache) then (error "proxying isn't enabled"))
    (setq table (pcache-uri-info-table pcache))
    
    (multiple-value-setq (ahost aport) (get-host-port host))
    (if* (null ahost)
       then (error "bad form for the host:port ~s" host))
    
    (dolist (ent (gethash ahost table))
      (if* (and (equal path (uri-info-path ent))
		(eql aport (uri-info-port ent)))
	 then (return (setq uri-info ent))))
    
    (if* (null uri-info)
       then ; add new one
	    (setq uri-info (make-uri-info 
			    :host ahost 
			    :port aport
			    :offsite offsite
			    :path path
			    :path-regexp (if* path
					    then (compile-regexp path))))
	    (push uri-info (gethash ahost table)))
    
    ; set fields
    (if* el-p
       then (setf (uri-info-extra-lifetime uri-info) extra-lifetime))
    
    (if* sd-p 
       then (setf (uri-info-scan-depth uri-info) scan-depth))
    
    (if* ex-p
       then (if* (and exclude 
		      (not (consp exclude)))
	       then (setq exclude (list exclude)))
	    
	    (setf (uri-info-exclude uri-info) exclude
		  (uri-info-exclude-regexp uri-info) (mapcar #'compile-regexp exclude))
	    )
    
    (if* sf-p
       then (setf (uri-info-scan-function uri-info) scan-function))
    
    (if* ofs-p
       then (setf (uri-info-offsite uri-info) offsite))
    
    uri-info))

	      
    
    
(defmethod find-uri-info ((uri net.uri:uri))
  ;; locate the uri-info corresponding to this uri, if
  ;; there is one
  (let ((pcache (wserver-pcache *wserver*))
	(path (net.uri:uri-path uri))
	(host (net.uri:uri-host uri))
	(port (or (net.uri:uri-port uri) 80)))
    (if* pcache
       then (dolist (ent (gethash host (pcache-uri-info-table pcache)))
	      (if* (eql port (uri-info-port ent))
		 then 
		      (let ((pregexp (uri-info-path-regexp ent)))
			(if* (null pregexp)
			   then ; matches everything
				(return ent)
			 elseif (match-regexp pregexp path :return nil)
			   then (return ent))))))))



	      
	    
    
    
	    
      
  
			





;-------- state save/restore

; items to save
;  pcache   (wserver-pcache)
; 

; structures needing saving
(defmethod make-load-form ((obj pcache) &optional env) 
  (make-load-form-saving-slots obj :environment env))

(defmethod make-load-form ((obj pcache-disk) &optional env)
  (make-load-form-saving-slots obj :environment env))

(defmethod make-load-form ((obj pcache-ent) &optional env)
  (make-load-form-saving-slots obj :environment env))

(defmethod make-load-form ((obj queueobj) &optional env)
  (make-load-form-saving-slots obj :environment env))

(without-package-locks
(defmethod make-load-form ((obj mp:process-lock) &optional env)
  (make-load-form-saving-slots obj :environment env))

; this is just temporary until we get a patch for this in uri.fasl
(defmethod make-load-form ((self net.uri:uri) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(uri-host self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)
     :fragment ,(uri-fragment self)
     :plist ',(uri-plist self)
     :string ,(net.uri::uri-string self)
     ; bug is missing ' in parsed-path value
     :parsed-path ',(net.uri::.uri-parsed-path self)))
)

(defun save-proxy-cache (filename &key (server *wserver*))
  ;; after the server threads have been saved, this function can 
  ;; be called to save out the state of the proxy cache
  
  ; prepare to save by removing objects that can't be saved
  
  (let ((pcache (wserver-pcache server)))
    (if* (null pcache) 
       then (return-from save-proxy-cache nil))
    
    (setf (pcache-cleaner pcache) nil)
    
    ; don't save the hash table since we can recreate it as since
    ; saving it is very expensive due to the fasl-circle check
    (setf (pcache-table pcache) nil)
    
    
    (dolist (pcache-disk (pcache-disk-caches pcache))
      ; for each disk cache
      (ignore-errors (close (pcache-disk-stream pcache-disk)))
      (setf (pcache-disk-stream pcache-disk) nil)))
    
  
  (with-open-file (p filename :direction :output :if-exists :supersede)
    (fasl-write (wserver-pcache server) p t))
  
  (setf (wserver-pcache server) nil))



(defun restore-proxy-cache (filename &key (server *wserver*))
  (let ((pcache (car (fasl-read filename))))
    (if* (not (typep pcache 'pcache))
       then (error "file didn't contain a saved cache"))
    
    (dolist (pcache-disk (pcache-disk-caches pcache))
      ; open each cache
      (let ((filename (pcache-disk-filename pcache-disk)))
	(setf (pcache-disk-stream pcache-disk)
	  (open filename 
		:if-exists :overwrite
		:if-does-not-exist :error
		:direction :io
		#-(and allegro (version>= 6))
		:element-type
		#-(and allegro (version>= 6))
		'(unsigned-byte 8)))))
    
    (setf (wserver-pcache server) pcache)
    
    ; rebuild the hash table
    (let ((table (make-hash-table :size 1000 :test #'equal)))
      (flet ((process-queue (table queueobj)
	       ;; add all entries to the hash table
	       (let ((lru-head (queueobj-lru queueobj))
		     (cur (pcache-ent-next (queueobj-mru queueobj))))
		 (loop
		   (if* (eq cur lru-head) then (return))
		   
		   (if* (not (eq :dead (pcache-ent-state cur)))
		      then (push cur
				 (gethash (pcache-ent-key cur)
					  table)))
		   (setq cur (pcache-ent-next cur))))))
		 
	(dolist (pcache-disk (pcache-disk-caches pcache))
	  (process-queue table (pcache-disk-queueobj pcache-disk)))
	(process-queue table (pcache-queueobj pcache))
	
	(setf (pcache-table pcache) table)
	))
      
    
    (start-proxy-cache-processes server pcache)))

    
;----
