;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; main.cl
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
;; $Id: main.cl,v 1.94 2001/01/22 16:17:29 jkf Exp $

;; Description:
;;   aserve's main loop

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(defpackage :net.aserve
  (:use :common-lisp :excl :net.html.generator)
  (:export
   #:authorize
   #:authorizer
   #:base64-decode
   #:base64-encode
   #:compute-strategy
   #:computed-entity
   ;; don't export, these should be private
   ; #:debug-off		
   ; #:debug-on			
   #:denied-request
   #:enable-proxy
   #:failed-request
   #:form-urlencoded-to-query
   #:get-basic-authorization
   #:get-cookie-values
   #:get-multipart-header
   #:get-multipart-sequence
   #:get-request-body
   #:handle-request
   #:header-slot-value
   #:http-request  	; class
   #:locator		; class
   #:location-authorizer  ; class
   #:location-authorizer-patterns
   #:password-authorizer  ; class
   #:process-entity
   #:publish
   #:publish-file
   #:publish-directory
   #:query-to-form-urlencoded
   #:reply-header-slot-value 
   #:set-basic-authorization
   #:standard-locator
   #:unpublish-locator

   #:request-method
   #:request-protocol
   #:request-protocol-string
   #:request-query
   #:request-query-value
   #:request-raw-request
   #:request-raw-uri
   #:request-socket
   #:request-uri
   #:request-wserver
   
   #:request-reply-code
   #:request-reply-date
   #:request-reply-content-length
   #:request-reply-content-type
   #:request-reply-plist
   #:request-reply-protocol-string
   #:request-reply-strategy
   #:request-reply-stream
   
   #:set-cookie-header
   #:shutdown
   #:split-into-words
   #:start
   #:uridecode-string
   #:uriencode-string
   #:unpublish
   #:url-argument
   #:url-argument-alist
   #:with-http-response
   #:with-http-body
   
   #:wserver
   #:wserver-enable-chunking
   #:wserver-enable-keep-alive
   #:wserver-locators
   #:wserver-log-function
   #:wserver-log-stream
   #:wserver-socket

   #:*aserve-version*
   #:*http-response-timeout*
   #:*mime-types*
   #:*response-accepted*
   #:*response-bad-request*
   #:*response-continue*
   #:*response-created*
   #:*response-found*
   #:*response-internal-server-error*
   #:*response-not-found*
   #:*response-not-modified*
   #:*response-ok*
   #:*response-moved-permanently*
   #:*response-see-other*
   #:*response-temporary-redirect*
   #:*response-unauthorized*
   #:*wserver*))

(in-package :net.aserve)

(defparameter *aserve-version* '(1 1 38))


(provide :aserve)

(defparameter *aserve-version-string*
    ;; for when we need it in string format
    (format nil "~d.~d.~d" 
	    (car *aserve-version*)
	    (cadr *aserve-version*)
	    (caddr *aserve-version*)))
	    
;;;;;;;  debug support 

(defparameter *debug-all* nil)	; all of the debugging switches
(defparameter *debug-log* nil)  ; all debugging switches that write info
				; to the *debug-stream*
(defparameter *debug-current*  nil)	; current switches set

(defparameter *debug-stream* *initial-terminal-io*)

; set to true to automatically close sockets about to be gc'ed
; open sockets should never be subject to gc unless there's a bug
; in the code leading to leaks.
(defvar *watch-for-open-sockets* t) 

(defmacro define-debug-kind (name class what)
  `(progn (ecase ,class
	    (:all (pushnew ,name *debug-all*))
	    (:log (pushnew ,name *debug-log*)
		  (pushnew ,name *debug-all*)))
	  (setf (get ,name 'debug-description) ,what)))

(define-debug-kind :notrap :all 
  "If set than errors in handlers cause a break loop to be entered")

(define-debug-kind :xmit   :log
  "If set then most of the traffic between clients and servers is also sent to the debug stream")

(define-debug-kind :info   :log
  "General information")

    

(defun debug-on (&rest args)
  ;; add the given debug kinds to the log list
  (if* (null args)
     then (note-debug-set)
     else (dolist (arg args)
	    (case arg
	      (:all (setq *debug-current* *debug-all*))
	      (:log (setq *debug-current*
		      (union *debug-current* *debug-log*)))
	      (t (pushnew arg *debug-current*))))))

(defun debug-off (&rest args)
  ;; turn off the debugging
  (if* (null args)
     then (note-debug-set)
     else (dolist (arg args)
	    (case arg
	      (:all (setq *debug-current* nil))
	      (:log (setq *debug-current*
		      (set-difference *debug-current* *debug-log*)))
	      (t (setq *debug-current* (remove arg *debug-current*)))))))

(defun note-debug-set ()
  ;; describe what debugging switches exist and if they are on
  ;; and off
  (dolist (kind *debug-all*)
    (format t "~7s ~4a  ~a~%" 
	    kind
	    (if* (member kind *debug-current*)
	       then "on"
	       else "off")
	    (get kind 'debug-description))))

	    

(defmacro debug-format (kind &rest args)
  ;; do the format to *debug-stream* if the kind of this info
  ;; is matched by the value of *debug-current*
  `(if* (member ,kind *debug-current* :test #'eq)
      then (format *debug-stream* "d> (~a): " (mp:process-name sys:*current-process*))
	   (format *debug-stream* ,@args)))


(defmacro format-dif (debug-key &rest args)
  ;; do the format and also do the same format to the 
  ;; debug stream if the given debug keyword is set
  ;; do the format and then send to *initial-terminal-io*
  `(progn (format ,@args)
	  (if* (member ,debug-key *debug-current* :test #'eq)
	     then (format *debug-stream* "x>(~a): " 
			  (mp:process-name sys:*current-process*))
		  (format *debug-stream* ,@(cdr args)))))

(defmacro if-debug-action (kind &rest body)
  ;; only do if the debug value is high enough
  `(progn (if* (member ,kind *debug-current* :test #'eq)
	     then ,@body)))

(defun check-for-open-socket-before-gc (socket)
  (if* (open-stream-p socket)
     then (logmess 
	   (format nil 
		   "socket ~s is open yet is about to be gc'ed. It will be closed" 
		   socket))
	  (ignore-errors (close socket))))


;;;;;;;;;;; end debug support ;;;;;;;;;;;;


;; foreign function imports
#+unix
(ff:def-foreign-call (setuid "setuid") ((x :int)) :returning :int)
#+unix
(ff:def-foreign-call (setgid "setgid") ((x :int)) :returning :int)


;; more specials
(defvar *max-socket-fd* 0) ; the maximum fd returned by accept-connection
(defvar *aserve-debug-stream* nil) ; stream to which to seen debug messages

; type of socket stream built.
; :hiper is possible in acl6
(defvar *socket-stream-type* 
    #+(and allegro (version>= 6)) :hiper
    #-(and allegro (version>= 6)) :stream
)

;; specials from other files
(defvar *header-block-sresource*)
(defvar *header-index-sresource*)
(defvar *header-keyword-array*
    ;; indexed by header-number, holds the keyword naming this header
    )
    

	
;;;;;;;;;;;;;  end special vars


(defclass wserver ()
  ;; all the information contained in a web server
  (
   ;;
   ;;-- user visible slots --
   ;; (accessors exported)
   
   (socket 		;; listening socket 
    :initarg :socket
    :accessor wserver-socket)
     
   (enable-keep-alive ;; do keep alive if it's possible
    :initform t
    :initarg :enable-keep-alive
    :accessor wserver-enable-keep-alive)
     
   (enable-chunking  ;; do chunking if it's possible
    :initform t
    :initarg :enable-chunking
    :accessor wserver-enable-chunking)
     
   (locators
    ;; list of locators objects in search order
    :initform (list (make-instance 'locator-exact
		      :name :exact)
		    (make-instance 'locator-prefix
		      :name :prefix)) 
    :accessor wserver-locators)
   
   (log-function
    ;; function to call after the request is done to 
    ;; do the logging
    :initarg :log-function
    :initform nil	; no logging initially
    :accessor wserver-log-function)
   
   (log-stream
    ;; place for log-function to store stream to log to if
    ;; it makes sense to do so
    :initarg :log-stream
    :initform  t 	; initially to *standard-output*
    :accessor wserver-log-stream)

   (accept-hook
    ;; if non-nil the function to call passing the socket about to be
    ;; processed by aserve, and charged with returning the socket to
    ;; process
    :initarg :accept-hook
    :initform nil
    :accessor wserver-accept-hook)
   
   ;;
   ;; -- internal slots --
   ;;
   
   (terminal-io  ;; stream active when we started server
    :initform *terminal-io*
    :initarg  :terminal-io
    :accessor wserver-terminal-io)
   
   (worker-threads  ;; list of threads that can handle http requests
    :initform nil
    :accessor wserver-worker-threads)
     
   (free-workers    ;; estimate of the number of workers that are idle
    :initform 0
    :accessor wserver-free-workers)
     
   (accept-thread   ;; thread accepting connetions and dispatching
    :initform nil
    :accessor wserver-accept-thread)

   (link-scan-threads  ;; threads scanning cached entries for links
    :initform nil
    :accessor wserver-link-scan-threads)
   
   (uri-scan-threads  ;; list of uri scanning processes
    :initform nil
    :accessor wserver-uri-scan-threads)
   
   (invalid-request
    ;; entity to invoke given a request that can't be
    ;; satisfied
    :initform nil  ; will build on demand if not present
    :accessor wserver-invalid-request)
   
   (denied-request
    ;; entity to invoke given a request that was denied
    :initform nil  ; will build on demand if not present
    :accessor wserver-denied-request)
   
   (ipaddrs
    ;; list of the ip addresses by which this wserver has been contacted
    :initform nil
    :accessor wserver-ipaddrs
    )
   
   (pcache
    ;; proxy cache
    :initform nil
    :accessor wserver-pcache)

   (shutdown-hooks
    ;; list of functions to call, passing this wserver object as an arg
    ;; when the server shuts down
    :initform nil
    :accessor wserver-shutdown-hooks)
   ))



(defmethod print-object ((wserver wserver) stream)
  (print-unreadable-object (wserver stream :type t :identity t)
    (format stream "port ~a" 
	    (let ((sock (wserver-socket wserver)))
	      (if* sock 
		 then (socket:local-port sock)
		 else "-no socket-")))))
     
     
     


;;;;;; macros 

(defmacro with-http-response ((req ent
				&key (timeout '*http-response-timeout*)
				     (check-modified t)
				     (response '*response-ok*)
				     content-type
				     )
			       &rest body)
  ;;
  ;; setup to response to an http request
  ;; do the checks that can shortciruit the request
  ;;
  (let ((g-req (gensym))
	(g-ent (gensym))
	(g-timeout (gensym))
	(g-check-modified (gensym)))
    `(let ((,g-req ,req)
	   (,g-ent ,ent)
	   (,g-timeout ,timeout)
	   (,g-check-modified ,check-modified)
	   )
       (catch 'with-http-response
	 (compute-strategy ,g-req ,g-ent)
	 (up-to-date-check ,g-check-modified ,g-req ,g-ent)
	 (mp::with-timeout ((if* (and (fixnump ,g-timeout)
				      (> ,g-timeout 0))
			       then ,g-timeout
			       else 9999999)
			    (timedout-response ,g-req ,g-ent))
	   ,(if* response
	       then `(setf (request-reply-code ,g-req) ,response))
	   ,(if* content-type
	       then `(setf (request-reply-content-type ,g-req) ,content-type)
	       else `(setf (request-reply-content-type ,g-req) (content-type ,g-ent)))
	   ,@body
	   )))))


#+(and allegro (version>= 6 0 pre-final 1))
(defun warn-if-crlf (external-format)
  (let ((ef (find-external-format external-format)))
    (if* (not (eq (crlf-base-ef ef) ef))
       then (warn "~
External-format `~s' passed to make-http-client-request filters line endings.
Problems with protocol may occur." (ef-name ef)))))

(defmacro with-http-body ((req ent
			   &key format headers (external-format :latin1-base))
			  &rest body)
  (declare (ignorable external-format))
  (let ((g-req (gensym))
	(g-ent (gensym))
	(g-format (gensym))
	(g-headers (gensym))
	(g-external-format (gensym))
	)
    `(let ((,g-req ,req)
	   (,g-ent ,ent)
	   (,g-format ,format)
	   (,g-headers ,headers)
	   #+(and allegro (version>= 6 0 pre-final 1))
	   (,g-external-format (find-external-format ,external-format))
	   )
       (declare (ignore-if-unused ,g-req ,g-ent ,g-format ,g-external-format))
       ,(if* body 
	   then `(compute-response-stream ,g-req ,g-ent))
       (if* ,g-headers
	  then (bulk-set-reply-headers ,g-req ,g-headers))
       (send-response-headers ,g-req ,g-ent :pre)
       (if* (not (member :omit-body (request-reply-strategy ,g-req)))
	  then (let ((*html-stream* (request-reply-stream ,g-req)))
		 #+(and allegro (version>= 6 0 pre-final 1))
		 (if* (and (streamp *html-stream*)
			   (not (eq ,g-external-format
				    (stream-external-format *html-stream*))))
		    then (warn-if-crlf ,g-external-format)
			 (setf (stream-external-format *html-stream*)
			   ,g-external-format))
		 (progn ,@body)))
       
       (if* (member :keep-alive (request-reply-strategy ,g-req))
	  then ; force the body to be read so we can continue
	       (get-request-body ,g-req))
       (send-response-headers ,g-req ,g-ent :post))))
			  


; safe versions during multiprocessing

(defmacro atomic-incf (var)
  `(mp:without-scheduling (incf ,var)))

(defmacro atomic-decf (var)
  `(mp:without-scheduling (decf ,var)))


;;;;;;;;; end macros

	       
       


(eval-when (compile load eval)
  ;; these are the common headers and are stored in slots in 
  ;; the objects
  ;; the list consists of  ("name" . name)
  ;; where name is symbol naming the accessor function
  (defparameter *fast-headers*
      (let (res)
	(dolist (name '(:connection
			:date
			:transfer-encoding
			:accept
			:host
			:user-agent
			:content-length))
	  (push (list name   ;; keyword symbol name
		      (read-from-string (format nil "reply-~a" name)) ;; symbol name
		      ;; accessor name
		      (read-from-string 
			    (format nil "request-header-~a" name))) res))
	res))
  
  (defparameter *fast-reply-headers*
      ;; list of headers for the reply that at stored in slots of
      ;; the http request object
      (let (res)
	(dolist (name '(:date
			:content-type
			:content-length))
	  (push (list name   ;; string name
		      
		      ;; symbol naming slot
		      (read-from-string 
		       (concatenate 'string (symbol-name :reply-) 
				    (string name)))
		      
		      ;; accessor name
		      (read-from-string 
			    (format nil "request-reply-~a" name))) res))
	res))
  
  )

    
	


(defmacro header-slot-value (req name)
  ;; name is a keyword symbol naming the header value.
  ;; retrive the slot's value from the http-request req req.
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-headers* :test #'eq))
       then ; has a fast accesor
	    `(or (,(third ent) ,req)
		 (setf (,(third ent) ,req)
		    (header-buffer-req-header-value ,req ,name)
		       ))
       else ; must get it from the alist
	    `(header-slot-value-other ,req ,name))))

(defun header-slot-value-other (req name)
  ;; handle out of the the 'extra' headers
  (let ((ent (assoc name (request-headers req) :test #'eq)))
    (if* ent
       then (cdr ent)
       else (let ((ans (header-buffer-req-header-value req name)))
	      (push (cons name ans) (request-headers req))
	      ans))))
      



(defsetf header-slot-value (req name) (newval)
  ;; set the header value regardless of where it is stored
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-headers* :test #'eq))
       then `(setf (,(third ent) ,req) ,newval)
       else (let ((genvar (gensym))
		  (nreq (gensym)))
	      `(let* ((,nreq ,req)
		      (,genvar (assoc ,name (request-headers ,nreq) 
				      :test #'eq)))
		 (if* (null ,genvar)
		    then (push (setq ,genvar (cons ,name nil))
			       (request-headers ,nreq)))
		 (setf (cdr ,genvar) ,newval))))))

(defmacro reply-header-slot-value (req name)
  ;; name is a string naming the header value (all lower case)
  ;; retrive the slot's value from the http-request req req.
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-reply-headers* :test #'eq))
       then ; has a fast accesor
	    `(,(third ent) ,req)
       else ; must get it from the alist
	    `(cdr (assoc ,name (request-reply-headers ,req) :test #'eq)))))

(defsetf reply-header-slot-value (req name) (newval)
  ;; set the header value regardless of where it is stored
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-reply-headers* :test #'eq))
       then `(setf (,(third ent) ,req) ,newval)
       else (let ((genvar (gensym))
		  (nreq (gensym)))
	      `(let* ((,nreq ,req)
		      (,genvar (assoc ,name (request-reply-headers ,nreq) 
				      :test #'eq)))
		 (if* (null ,genvar)
		    then (push (setq ,genvar (cons ,name nil))
			       (request-reply-headers ,nreq)))
		 (setf (cdr ,genvar) ,newval))))))


(defun header-name-error (name)
  (error "You attempted to reference header ~s.  Headers are now named
by keyword symbols and not by strings"
	 name))

(defmacro header-slot-value-integer (req name)
  ;; if the header value exists and has an integer value
  ;; return two values: the value of the integer and t
  ;; else return nil
  
  `(header-decode-integer (header-slot-value ,req ,name)))



    


(defclass http-header-mixin ()
  ;; List of all the important headers we can see in any of the protocols.
  ;; 
  #.(let (res)
      ;; generate a list of slot descriptors for all of the 
      ;; fast header slots
      (dolist (head *fast-headers*)
	(push `(,(third head) :accessor ,(third head) 
			    :initform nil
			    :initarg
			    ,(intern (symbol-name (second head)) :keyword))
	      res))
      res))
   
	   
	   
   







(defclass http-request (http-header-mixin)
  ;;
  ;; incoming request and information about the reply we send to it
  ;;
  (
   ;;
   ;; -- external slots --
   ;;  (accessors exported)
   
   (method  ;; keyword giving the command in this request :get .. etc.
    :initarg :method
    :reader request-method)
   
   (uri  ;; uri object holding the current request with the scheme, host
         ;; and port filled in.
    :initarg :uri
    :accessor request-uri)

   (raw-uri  ;; uri object holding the actual uri from the command
    :initarg :raw-uri
    :accessor request-raw-uri)
   
   (protocol ;; symbol naming the http protocol  (e.g. :http/1.0)
    :initarg :protocol
    :reader request-protocol)
   
   (protocol-string ;; string naming the protcol requested
    :initarg :protocol-string
    :reader request-protocol-string)
   
   (socket ;; the socket we're communicating through
    :initarg :socket
    :reader request-socket)
   
   (wserver ;; wserver object for web server this request came to
    :initarg :wserver
    :reader request-wserver)
   
   (raw-request  ;; the actual command line from the browser
    :initarg :raw-request
    :reader request-raw-request)
   
   ;;
   ;; -- internal slots --
   ;;
   
   (query-alist 
    ;; list of conses (name . value) for the query part of the 
    ;; current uri.  This slot is filled in when information
    ;; is first requested by  the  request-query function
    :initform :empty
    :accessor request-query-alist)
   
   
   (headers ;; alist of headers *not* stored in slots
    ;* use header-slot-value to retrieve header values 
    ;  rather than looking here since not all headers are stored 
    ;  here
    :initform nil
    :accessor request-headers)

   (header-block  ;; *header-block-sresource* object
    :initform nil
    :accessor request-header-block)
    
   (request-body 
    ;; if we've read the request body then this 
    ;; is the string holding it.
    :initform nil
    :accessor request-request-body)
   
   
   

   ;; response
   (reply-code   ;; one of the *response-xx* objects
    :initform nil
    :accessor request-reply-code)
   
   (reply-date
    :initform (get-universal-time)  ; when we're responding
    :reader request-reply-date)
   
   (reply-headers  ;; alist of headers to send out
    :initform nil
    :accessor request-reply-headers)
   
   (reply-content-type ;; mime type of the response
    :initform nil
    :accessor request-reply-content-type)
   
   (reply-stream   ;; stream to which to send response
    :initform nil
    :accessor request-reply-stream)
   
   (reply-content-length
    :initform nil  ;; nil means "i don't know"
    :accessor request-reply-content-length)
   
   (reply-strategy  ;; list of strategy objects
    :initform nil
    :accessor request-reply-strategy)
   
   (reply-plist    ;; general stuff in a property list form
    :initform nil
    :accessor request-reply-plist)

   (reply-protocol-sring
    ;; A web server announces the highest minor level of the 
    ;; major level of the protocol that was requested by the client.
    ;; Thus for now we're always http/1.1
    :initform "HTTP/1.1"
    :accessor request-reply-protocol-string)
   )
  
  
		
  )


(defstruct (response (:constructor make-resp (number desc)))
  number
  desc)

(defparameter *response-continue* (make-resp 100 "Continue"))
(defparameter *response-ok* (make-resp 200 "OK"))
(defparameter *response-created* (make-resp 201 "Created"))
(defparameter *response-accepted* (make-resp 202 "Accepted"))

(defparameter *response-moved-permanently* (make-resp 301 "Moved Permanently"))
(defparameter *response-found* (make-resp 302 "Found"))
(defparameter *response-see-other* (make-resp 303 "See Other"))
(defparameter *response-not-modified* (make-resp 304 "Not Modified"))
(defparameter *response-temporary-redirect* 
    (make-resp 307 "Temporary Redirect"))
(defparameter *response-bad-request* (make-resp 400 "Bad Request"))
(defparameter *response-unauthorized* (make-resp 401 "Unauthorized"))
(defparameter *response-not-found* (make-resp 404 "Not Found"))

(defparameter *response-internal-server-error*
    (make-resp 500 "Internal Server Error"))
(defparameter *response-not-implemented* (make-resp 501 "Not Implemented"))

(defparameter *responses*
    (list *response-continue*
	  *response-ok*
	  *response-created*
	  *response-accepted*
	  *response-moved-permanently*
	  *response-found*
	  *response-see-other*
	  *response-not-modified*
	  *response-temporary-redirect*
	  *response-bad-request*
	  *response-unauthorized*
	  *response-not-found*))

(defvar *crlf* (make-array 2 :element-type 'character :initial-contents
			   '(#\return #\linefeed)))

(defvar *read-request-timeout* 20)
(defvar *read-request-body-timeout* 60)
(defvar *http-response-timeout* 120) ; amount of time for an http response

(defvar *thread-index*  0)      ; globalcounter to gen process names

(defvar *wserver*)   ; set to last server created

				    
			      
(defun start (&key (port 80 port-p) 
		   (listeners 5)
		   (chunking t)
		   (keep-alive t)
		   (server *wserver*)
		   debug      ; set debug level
		   setuid
		   setgid
		   proxy
		   cache       ; enable proxy cache
		   restore-cache ; restore a proxy cache
		   debug-stream  ; stream to which to send debug messages
		   accept-hook
		   ssl		 ; enable ssl
		   )
  ;; -exported-
  ;;
  ;; start the web server
  ;; return the server object
  #+mswindows
  (declare (ignore setuid setgid))
  
  (declare (ignore debug))  ; for now

  (if* debug-stream 
     then (setq *aserve-debug-stream* 
	    (if* (eq debug-stream t)
	       then *standard-output*
	       else debug-stream)))
  
  (if* (eq server :new)
     then (setq server (make-instance 'wserver)))

  (if* ssl
     then (if* (pathnamep ssl)
	     then (setq ssl (namestring ssl)))
	  
	  (if* (not (stringp ssl))
	     then (error "The ssl argument should be a string or pathname holding the filename of the certificate and private key file"))
	  
	  (setq accept-hook 
	    #'(lambda (socket)
		(funcall 'socket::make-ssl-server-stream socket
			 :certificate ssl)))
	  (setq chunking nil) ; doesn't work well through ssl
	  (if* (not port-p)
	     then ; ssl defaults to port 443
		  (setq port 443)))
	    
  (if* accept-hook
     then (setf (wserver-accept-hook server) accept-hook))
  
  
  ; shut down existing server
  (shutdown :server server) 

  (if* proxy 
     then (enable-proxy :server server))

  (if* restore-cache
     then (restore-proxy-cache restore-cache :server server)
  elseif cache
     then ; cache argument can have many forms
	  (let ((memory-size #.(* 10 1024 1024)) ; default 10mb
		(disk-caches nil))
	    (if* (atom cache)
	       then (if* (integerp cache)
		       then (setq memory-size cache))
	       else (do ((info cache (cddr info)))
			((null info))
		      (case (car info)
			(:memory (setq memory-size (cadr info)))
			(:disk (let ((dsize (cadr info)))
				 (if* (atom dsize)
				    then (if* (not (integerp dsize))
					    then (setq dsize 
						   #.(* 10 1024 1024)))
					 
					 (push (cons nil dsize) disk-caches)
							     
				    else (push dsize disk-caches))))
			(t (error "unknown disk cache info specifier: ~s"
				  (car info))))))
		    
	    (create-proxy-cache :server server :size memory-size)
	    (dolist (disk-cache disk-caches)
	      (add-disk-cache :server server
			      :filename (car disk-cache)
			      :size (cadr disk-cache)))))
	    
  

  
  (let* ((main-socket (socket:make-socket :connect :passive
					  :local-port port
					  :reuse-address t
					  :format :bivalent
					  
					  :type 
					  *socket-stream-type*
					  )))

    #+unix
    (progn
      (if* (fixnump setgid) then (setgid setgid))
      (if* (fixnump setuid) then (setuid setuid)))
    
    (setf (wserver-socket server) main-socket)
    (setf (wserver-terminal-io server) *terminal-io*)
    (setf (wserver-enable-chunking server) chunking)
    (setf (wserver-enable-keep-alive server) keep-alive)
    
    
    (let ((*wserver* server)) ; bind it too for privacy
      (if* (or (null listeners) (eq 0 listeners))
	 then (start-simple-server)
       elseif (and (fixnump listeners) (> listeners 0))
	 then (start-lisp-thread-server listeners)
	 else (error "listeners should be nil or a non-negative fixnum, not ~s"
		     listeners)))
    
    server
    ))


(defun shutdown (&key (server *wserver*) save-cache)
  ;; shutdown the neo server
  ; first kill off old processes if any
  (let ((proc (wserver-accept-thread server)))
    (if* proc
       then ; we want this thread gone and the socket closed 
	    ; so that we can reopen it if we want to.
	    (mp:process-kill proc)
	    (mp:process-allow-schedule)
	    (let ((oldsock (wserver-socket server)))
	      (if* oldsock then (ignore-errors (close oldsock))))
	    (setf (wserver-accept-thread server) nil)))
  
  (dolist (th (wserver-worker-threads server))
    (mp:process-kill th)
    (mp:process-allow-schedule))
  
  (setf (wserver-worker-threads server) nil)
  
  (dolist (hook (wserver-shutdown-hooks server))
    (funcall hook server))
  
  (if* save-cache
     then (save-proxy-cache save-cache :server server)
     else (kill-proxy-cache :server server)))


(defun start-simple-server ()
  ;; do all the serving on the main thread so it's easier to
  ;; debug problems
  (let ((main-socket (wserver-socket *wserver*))
	(ipaddrs (wserver-ipaddrs *wserver*)))
    (unwind-protect
	(loop
	  (restart-case
	      (let ((sock (socket:accept-connection main-socket))
		    (localhost))
		(if* (not (member (setq localhost (socket:local-host sock))
				  ipaddrs))
		   then ; new ip address by which this machine is known
			(push localhost ipaddrs)
			(setf (wserver-ipaddrs *wserver*) ipaddrs))
		(if* *watch-for-open-sockets*
		   then (schedule-finalization 
			 sock 
			 #'check-for-open-socket-before-gc))
		(process-connection sock))
	    
	    (:loop ()  ; abort out of error without closing socket
	      nil)))
      (close main-socket))))

	
(defun start-lisp-thread-server (listeners)
  ;; start a server that consists of a set of lisp threads for
  ;; doing work and a lisp thread for accepting connections
  ;; and farming out the work
  
  ; create worker threads
  (setf (wserver-free-workers *wserver*) 0)
  (dotimes (i listeners) (make-worker-thread))
  
  
  ; create accept thread
  (setf (wserver-accept-thread *wserver*)
    (mp:process-run-function 
     (list :name (format nil "aserve-accept-~d" (incf *thread-index*))
	   :initial-bindings
	   `((*wserver*  . ',*wserver*)
	     #+ignore (*debug-io* . ',(wserver-terminal-io *wserver*))
	     ,@excl:*cl-default-special-bindings*))
     #'http-accept-thread)))

(defun make-worker-thread ()
  (let* ((name (format nil "~d-aserve-worker" (incf *thread-index*)))
	 (proc (mp:make-process :name name
				:initial-bindings
				`((*wserver*  . ',*wserver*)
				  #+ignore (*debug-io* . ',(wserver-terminal-io 
						   *wserver*))
				  ,@excl:*cl-default-special-bindings*)
				)))
    (mp:process-preset proc #'http-worker-thread)
    (push proc (wserver-worker-threads *wserver*))
    (atomic-incf (wserver-free-workers *wserver*))
    (setf (getf (mp:process-property-list proc) 'short-name) 
      (format nil "w~d" *thread-index*))
    ))


(defun http-worker-thread ()
  ;; made runnable when there is an socket on which work is to be done
  (let ((*print-level* 5))
    ;; lots of circular data structures in the caching code.. we 
    ;; need to restrict the print level
    (loop

      (let ((sock (car (mp:process-run-reasons sys:*current-process*))))
	(restart-case
	    (if* (not (member :notrap *debug-current* :test #'eq))
	       then (handler-case (process-connection sock)
		      (error (cond)
			(logmess (format nil "~s: got error ~a~%" 
					 (mp:process-name sys:*current-process*)
					 cond))))
	       else (process-connection sock))
	  (abandon ()
	      :report "Abandon this request and wait for the next one"
	    nil))
	(atomic-incf (wserver-free-workers *wserver*))
	(mp:process-revoke-run-reason sys:*current-process* sock))
    
      )))

(defun http-accept-thread ()
  ;; loop doing accepts and processing them
  ;; ignore sporatic errors but stop if we get a few consecutive ones
  ;; since that means things probably aren't going to get better.
  (let* ((error-count 0)
	 (workers nil)
	 (server *wserver*)
	 (main-socket (wserver-socket server))
	 (ipaddrs (wserver-ipaddrs server)))
    (unwind-protect

	(loop
	  (handler-case
	      (let ((sock (socket:accept-connection main-socket))
		    (localhost))
		
		; optional.. useful if we find that sockets aren't being
		; closed
		(if* *watch-for-open-sockets*
		   then (schedule-finalization 
			 sock 
			 #'check-for-open-socket-before-gc))
		
		; track all the ipaddrs by which we're reachable
		(if* (not (member (setq localhost (socket:local-host sock))
				  ipaddrs))
		   then ; new ip address by which this machine is known
			(push localhost ipaddrs)
			(setf (wserver-ipaddrs *wserver*) ipaddrs))
		
		; another useful test to see if we're losing file
		; descriptors
		(let ((fd (excl::stream-input-fn sock)))
		  (if* (> fd *max-socket-fd*)
		     then (setq *max-socket-fd* fd)
			  (logmess (format nil 
					   "Maximum socket file desciptor number is now ~d" fd))))
		
		
		(setq error-count 0) ; reset count
	
		; find a worker thread
		; keep track of the number of times around the loop looking
		; for one so we can handle cases where the workers are all busy
		(let ((looped 0))
		  (loop
		    (if* (null workers) 
		       then (case looped
			      (0 nil)
			      ((1 2 3) (logmess "all threads busy, pause")
				       (sleep 1))
			     
			      (4 (logmess "forced to create new thread")
				 (make-worker-thread))
		    
			      (5 (logmess "can't even create new thread, quitting")
				 (return-from http-accept-thread nil)))
			   
			    (setq workers (wserver-worker-threads server))
			    (incf looped))
		    (if* (null (mp:process-run-reasons (car workers)))
		       then (atomic-decf (wserver-free-workers server))
			    (mp:process-add-run-reason (car workers) sock)
			    (pop workers)
			    (return) ; satisfied
			    )
		    (pop workers))))
	  
	    (error (cond)
	      (logmess (format nil "accept: error ~s on accept ~a" 
			       error-count cond))
	      ;; we seem to get a string of connection reset by peers,
	      ;; perhaps due to connections that piled up before
	      ;; we started work. So we don't want to close down
	      ;; the accepting loop ever, thus we'll ignore the 
	      ;; code below.
	      #+ignore
	      (if* (> (incf error-count) 4)
		 then (logmess "accept: too many errors, bailing")
		      (return-from http-accept-thread nil)))))
      (ignore-errors (progn
		       (mp:without-scheduling
			 (if* (eql (wserver-socket server) main-socket)
			    then (setf (wserver-socket server) nil)))
		       (close main-socket))))))
      
  
    
    

(defun start-cmd ()
  ;; start using the command line arguments
  (let ((port 8001))
    (do* ((args (cdr (sys:command-line-arguments)) (cdr args))
	  (arg (car args) (car args)))
	((null args))
      (if* (equal "-f" arg)
	 then (load (cadr args))
	      (pop args)
       elseif (equal "-p" arg)
	 then (setq port (read-from-string (cadr args)))
	      (pop args)
       elseif (equal "-I" arg)
	 then (pop args)
	 else (warn "unknown arg ~s" arg)))
    (dotimes (i 20)
      (handler-case (progn (start :port port) (loop (sleep 100000)))
	(error (cond)
	  (format t " got error ~a~%" cond)
	  (format t "restarting~%"))))))


(defun process-connection (sock)
  ;; read an http request from the socket and process
  ;; it.
  ;; If the response indicates 'keep alive' then loop around for
  ;; another request.
  ;; When this function returns the given socket has been closed.
  ;;
  
  ; run the accept hook on the socket if there is one
  (let ((ahook (wserver-accept-hook *wserver*)))
    (if* ahook then (setq sock (funcall ahook sock))))
  
	
  (unwind-protect
      (let ((req))
	;; get first command
	(loop
	  (mp:with-timeout (*read-request-timeout* 
			    (debug-format :info "request timed out on read~%")
			    ; this is too common to log, it happens with
			    ; every keep alive socket when the user stops
			    ; clicking
			    ;;(log-timed-out-request-read sock)
			    (return-from process-connection nil))
	    (setq req (read-http-request sock)))
	  (if* (null req)
	     then ; end of file, means do nothing
		  ; (logmess "eof when reading request")
		  ; end this connection by closing socket
		  (return-from process-connection nil)
	     else ;; got a request
		  (handle-request req)
		  (force-output (request-socket req))
		  
		  (log-request req)
		  
		  (free-req-header-block req)
		  
		  (let ((sock (request-socket req)))
		    (if* (member :keep-alive
				 (request-reply-strategy req)
				 :test #'eq)
		       then ; continue to use it
			    (debug-format :info "request over, keep socket alive~%")
			    (force-output sock)
		       else (return))))))
    ;; do it in two stages since each one could error and both have
    ;; to be attempted
    (ignore-errors (force-output sock))
    (ignore-errors (close sock :abort t))))


(defun read-http-request (sock)
  ;; read the request from the socket and return and http-request
  ;; object
  
  (let ((buffer (get-request-buffer))
	(req)
	(end)
	(raw-cmd))
    
    (unwind-protect
	(progn
	  (loop
	    ; loop until a non blank line is seen and is stored in
	    ; the buffer
	    ;
	    ; we handle the case of a blank line before the command
	    ; since the spec says that we should (even though we don't have to)
      
	    (multiple-value-setq (buffer end)
	      (read-sock-line sock buffer 0))
      
	    (if* (null end)
	       then ; eof or error before crlf
		    (return-from read-http-request nil))
      
	    
	    (debug-format  :info "got line of size ~d: " end)
	    (if-debug-action :info
			     (dotimes (i end) (write-char (schar buffer i) 
							  *initial-terminal-io*))
			     (terpri *initial-terminal-io*) (force-output *initial-terminal-io*))
      
	    (if* (not (eql 0 end))
	       then (return) ; out of loop
		    ))
	  
	  (setq raw-cmd (buffer-substr buffer 0 end))
	  
	  (multiple-value-bind (cmd uri protocol)
	      (parse-http-command buffer end)
	    (if* (or (null cmd) (null protocol))
	       then ; no valid command found
		    (return-from read-http-request nil))

	    (if* (null (net.uri:uri-path uri))
	       then (setf (net.uri:uri-path uri) "/"))
	    
	    (setq req (make-instance 'http-request
			:method cmd
			:uri (net.uri:copy-uri uri)
			:raw-uri uri
			:protocol protocol
			:protocol-string (case protocol
					   (:http/1.0 "HTTP/1.0")
					   (:http/1.1 "HTTP/1.1")
					   (:http/0.9 "HTTP/0.9"))
			:socket sock
			:wserver *wserver*
			:raw-request raw-cmd
			))
	    
	    
	    (if* (and (not (eq protocol :http/0.9))
		      #+ignore (null (read-request-headers req sock buffer))
		      (null (new-read-request-headers req sock))
		      )
	       then (debug-format :info "no headers, ignore~%")
		    (return-from read-http-request nil))
	    
	    ; insert the host name and port into the uri
	    (let ((host (header-slot-value req :host)))
	      (if* host
		 then (let ((colonpos (find-it #\: host 0 (length host)))
			    (uri (request-uri req))
			    (port))
			(if* colonpos
			   then ; host:port
				(setq 
				    port (string-to-number
					  host (1+ colonpos)
					  (length host))
				    host (buffer-substr host
							0 colonpos)))
			(if* (null (uri-host uri))
			   then (setf (uri-host uri) host)
				(if* port
				   then (setf (uri-port uri) port)))
			
			(setf (uri-scheme uri) :http)  ; always http
			))))
	  
	    
	  req  ; return req object
	  )
    
      ; cleanup forms
      (if* buffer then (free-request-buffer buffer)))))


				      
    
		    
      
   
    
    
    




(defvar *http-command-list*
    '(("GET " . :get)
      ("HEAD " . :head)
      ("POST " . :post)
      ("PUT "  . :put)
      ("OPTIONS " . :options)
      ("DELETE " .  :delete)
      ("TRACE "  .  :trace)
      ("CONNECT " . :connect)))
  

	    

(defmethod get-request-body ((req http-request))
  ;; return a string that holds the body of the http-request
  ;;  cache it for later too
  (or (request-request-body req)
      (setf (request-request-body req)
	(if* (member (request-method req) '(:put :post))
	   then (multiple-value-bind (length believe-it)
		    (header-slot-value-integer req :content-length)
		  (if* believe-it
		     then ; we know the length
			  (prog1 (let ((ret (make-string length)))
				   (read-sequence-with-timeout 
				    ret length 
				    (request-socket req)
				    *read-request-body-timeout*))
	    
			    ; netscape (at least) is buggy in that 
			    ; it sends a crlf after
			    ; the body.  We have to eat that crlf.  
			    ; We could check
			    ; which browser is calling us but it's 
			    ; not clear what
			    ; is the set of buggy browsers 
			    (let ((ch (read-char-no-hang (request-socket req)
							 nil nil)))
			      (if* (eq ch #\return)
				 then ; now look for linefeed
				      (setq ch (read-char-no-hang 
						(request-socket req) nil nil))
				      (if* (eq ch #\linefeed)
					 thenret 
					 else (unread-char 
					       ch (request-socket req)))
			       elseif ch
				 then (unread-char ch (request-socket req)))))
				      
				      
		     else ; no content length given
			  
			  (if* (equalp "keep-alive" 
				       (header-slot-value req :connection))
			     then ; must be no body
				  ""
			     else ; read until the end of file
				  (mp:with-timeout 
				      (*read-request-body-timeout* 
				       nil)
				    (let ((ans (make-array 
						2048 
						:element-type 'character
						:fill-pointer 0))
					  (sock (request-socket req))
					  (ch))
				      (loop (if* (eq :eof 
						     (setq ch (read-char 
							       sock nil :eof)))
					       then (return  ans)
					       else (vector-push-extend ch ans))))))))
	   else "" ; no body
		))))



;; multipart code
;; used when enctype=multipart/form-data is used

; new version that uses binary mode to transfer data

(defstruct mp-info
  buffer	; usb8 buffer if we're active
  left		; bytes of content-length left to read
  state		; state where the buffer pointer is pointed
  cur		; current buffer pointer
  end		; index after last byte in the buffer
  boundary	; boundary vector
  socket	; socket we're reading from
  )

(defmethod start-multipart-capture ((req http-request))
  ;; begin the grab of the body.
  ;; user doesn't have to call this, it's called automatically
  (let* ((ctype (header-slot-value req :content-type))
	 (parsed (and ctype (parse-header-value ctype)))
	 (boundary (and (equalp "multipart/form-data" (cadar parsed))
			(cdr (assoc "boundary" (cddar parsed) :test #'equal))))
	 (len (header-slot-value-integer req :content-length))
	 (mpbuffer))
    
    (if* (null boundary)
       then ; not in the right form, give up
	    (return-from start-multipart-capture nil))
    
    (setq mpbuffer (get-sresource *header-block-sresource*))
    
    (setf (aref mpbuffer 0) #.(char-code #\return))
    (setf (aref mpbuffer 1) #.(char-code #\linefeed))
    
    (setf (getf (request-reply-plist req) 'mp-info)
      (make-mp-info :buffer mpbuffer
		    :left  len
		    :state :start
		    :cur   0  ; we'll start the buffer with cr,lf
		    :end   2  ; since the first boundary may not have it
		    :socket (request-socket req)
		    
		    ;;  boundary is a case insensitive usb8 array
		    ;; <cr><lf>--boundary-downcased
		    ;;
		    :boundary 
		    (let ((array (make-array (+ 2 ; crlf
						2 ; --
						(length boundary))
					     :element-type '(unsigned-byte 8))))
		      (setf (aref array 0) #.(char-code #\return))
		      (setf (aref array 1) #.(char-code #\linefeed))
		      (setf (aref array 2) #.(char-code #\-))
		      (setf (aref array 3) #.(char-code #\-))
		      
		      (dotimes (i (length boundary))
			(setf (aref array (+ i 4))
			  (char-code (char-downcase (schar boundary i)))))
		      array)
		    ))))


(defparameter *crlf-crlf-usb8*
    (make-array 4 :element-type '(unsigned-byte 8)
		:initial-contents
		(list #.(char-code #\return)
		      #.(char-code #\linefeed)
		      #.(char-code #\return)
		      #.(char-code #\linefeed))))

(defmethod get-multipart-header ((req http-request))
  ;; return an alist holding the header info for the next request.
  ;; or nil if this is the end of the line
  
  (let ((mp-info (getf (request-reply-plist req) 'mp-info)))
    
    (if* (null mp-info)
       then (start-multipart-capture req)
	    ; satisify normal requests for the body with an empty string
	    (setf (request-request-body req) "") 
	    (setq mp-info (getf (request-reply-plist req) 'mp-info)))
    
    (if* (null mp-info)
       then ; no headers left
	    (return-from get-multipart-header nil))
    
    (loop
      (case (mp-info-state mp-info)
	(:header (return)) ; ok
	((:body :start) 
	 (loop
	   (multiple-value-bind (pos state)
	       (scan-forward mp-info)
	     (if* (eq state :partial)
		then (if* (not (shift-buffer-up-and-read mp-info))
			then ; no more data, bogus end though
			     (setf (mp-info-state mp-info) :last-boundary)
			     (return))
		else (setf (mp-info-cur mp-info) pos
			   (mp-info-state mp-info) state)
		     (return)))))
	(:boundary  (scan-forward mp-info))
	(:last-boundary ; no more space
	 ; free up the buffer
	 (free-sresource *header-block-sresource* (mp-info-buffer
						   mp-info))
	 (setf (mp-info-buffer mp-info) nil)
	 ;;
	 (return-from get-multipart-header nil))))
    
    ; in the header state, must find the end of the
    ; header block <cr><lf><cr><lf>
    (let* ((buffer (mp-info-buffer mp-info))
	   cur  
	   headers
	   endhead)
      
      (loop
	(setq cur (mp-info-cur mp-info))
	(setq endhead (search-usb8 buffer 
				   cur 
				   (mp-info-end mp-info)
				   *crlf-crlf-usb8*))
	(if* (integerp endhead) 
	   then (incf endhead 4) ; cr lf cr lf
		(return))
	
	(if* (not (shift-buffer-up-and-read mp-info))
	   then ; failed to find the end of the headers
		(error "end of headers not found, badly formed request")))
      
      ; found the end of the headers.
      (let ((ans (get-sresource *header-index-sresource*)))
	(parse-header-block-internal buffer cur endhead ans)
	(dotimes (i (length ans))
	  (dolist (ent (svref ans i))
	    (push (cons (svref *header-keyword-array* i)
			(parse-header-value
			 (buffer-subseq-to-string buffer (car ent) (cdr ent))))
		  headers)))
	(free-sresource *header-index-sresource* ans))
      
      (setf (mp-info-cur mp-info) endhead)
      (setf (mp-info-state mp-info) :body) ; pointing at the body
      
      headers)))

(defun search-usb8 (buffer start end pattern)
  ;; look for the pattern in the buffer
  (do* ((i start (1+ i))
	(patlen (length pattern))
	(realend (- end patlen -1)))
      ((>= i realend))
    (dotimes (j patlen (return-from search-usb8 i))
      (if* (not (eq (aref buffer (+ i j))  (aref pattern j)))
	 then ; no match
	      (return nil)))))
	      
       
  

(defun shift-buffer-up-and-read (mp-info)
  ;; bring bytes from cur to end to the top of the buffer.
  ;;
  ;; read in more bytes.
  ;;
  ;;
  ;; return true if more any bytes.  nil means that there is
  ;; no more data to read or no more space for data.
  ;;
  (let ((cur (mp-info-cur mp-info))
	(mpbuffer (mp-info-buffer mp-info))
	(left))
    
    (if* (zerop (setq left (mp-info-left mp-info)))
       then nil ; nothing to do, since no more data left
       else ; shift up buffer if needed
	    (if* (> cur 0)
	       then (do ((end (mp-info-end mp-info))
			 (i cur (1+ i)))
			((>= i end))
		      (setf (aref mpbuffer (- i cur)) (aref mpbuffer i)))
		    (setf (mp-info-cur mp-info) 0)
		    (decf (mp-info-end mp-info) cur))
    
	    (if* (eql (mp-info-end mp-info) (length mpbuffer))
	       then ; no room to store anything else
		    nil
	       else ; read as much as we acn
		    (let* ((end (mp-info-end mp-info))
			   (pos (rational-read-sequence mpbuffer 
					       (mp-info-socket mp-info)
					       :start end
					       :end (min
						     (length mpbuffer)
						     (+ end left)))))
		      (if* (<= pos end)
			 then ; no bytes read, eof
			      nil
			 else 
			      (if-debug-action 
			       :xmit
			       (format t "~%multipart read ~d bytes~%" 
				       (- pos end))
			       (do ((i end (1+ i)))
				   ((>= i pos))
				 (write-char (code-char (aref mpbuffer i))))
			       (format t "<end-of-read>~%")
			       (force-output))
				   
				      
			      (decf (mp-info-left mp-info) 
				    (- pos end))
			      (setf (mp-info-end mp-info) pos)
			      t ; read something in
			      ))))))

	    
(defun scan-forward (mp-info)
  ;; scan forward to the next interesting thing
  ;;
  ;; If the current state is :boundary then we are being called 
  ;;   to locate the next header.  If we find it we set cur to point
  ;;   to it and set the state to :header.  If no more data is available
  ;;   (and this will never happen if the client works correctly) twe
  ;;   set the state to :last-boundary
  ;;   nil is returned
  ;;
  ;; If the current state is :body or :start
  ;; we look forward to find the point a which the next :boundary
  ;; starts.  We give up at the end of the bytes read into the buffer  
  ;; we don't change cur or the state of the mp-info
  ;; We return two values:
  ;;  the point at which that transition occurs to boundary
  ;;  a symbol describing what we found
  ;;        :body,:state   - found nothing interesting, we're still in the same
  ;;	              state we started in (either :body or :state)
  ;;	    :boundary - we've identified a boundary between items
  ;;	    :last-boundary  - we've indentified the final boundary 
  ;;	    :partial - we've identified something that could be
  ;;		a boundary or last-boundary but don't have enough
  ;;		data to tell.
  (case (mp-info-state mp-info)
    (:boundary
     ;; skip past boundary if it's in the buffer
     ;; this is called only in get-multipart-header
     (loop
       (let ((past (+ (mp-info-cur mp-info)
		      (length (mp-info-boundary mp-info))
		      2 ; for the crlf
		      )))
	 (if* (< past (mp-info-end mp-info))
	    then ; inside the buffer
		 (setf (mp-info-cur mp-info) past)
		 (setf (mp-info-state mp-info) :header)
		 (return)
	    else (if* (not (shift-buffer-up-and-read mp-info))
		    then ; no more data
			 (setf (mp-info-state mp-info) :last-boundary)
			 (return))))))
    (:last-boundary nil)
    
    ((:body  :start)
     ;; looking for a boundary or partial boundary
     (let* ((cur (mp-info-cur mp-info))
	    (end (mp-info-end mp-info))
	    (boundary (mp-info-boundary mp-info))
	    (len-boundary (length boundary))
	    )
       (if* (eql cur end)
	  then (if* (not (shift-buffer-up-and-read mp-info))
		  then ; no more data available
		       (setf (mp-info-state mp-info) :last-boundary)
		       (return-from scan-forward (values 0 :last-boundary))))
       (setq cur (mp-info-cur mp-info)
	     end (mp-info-end mp-info))
       
       (do* ((i cur (1+ i))
	     (mpbuffer (mp-info-buffer mp-info)))
	   ((>= i end)
	    (values end (mp-info-state mp-info)))
	 
	 (let* ((ch (aref mpbuffer i)))
	   (if* (eq ch #.(char-code #\return))
	      then ; this could match
		   (do ((j i (1+ j))
			(ind 0 (1+ ind)))
		       
		       (nil)
		     (if* (>= ind len-boundary)
			then ; matched the whole boundary
			     ; minus the tail... a tail of crlf 
			     ; is a boundary and a --crlf is a last
			     ; boundary
				
			     (if* (< (- end j) 2)
				then ; can't tell yet
				     (return-from scan-forward
				       (values i :partial))
			      elseif (and (eql (aref mpbuffer j) 
					       #.(char-code #\return))
					  (eql (aref mpbuffer (1+ j))
					       #.(char-code #\linefeed)))
				then ; it's a boundary match
				     (return-from scan-forward
				       (values i :boundary))
			      elseif (and (eql (aref mpbuffer j) 
					       #.(char-code #\-))
					  (eql (aref mpbuffer (1+ j))
					       #.(char-code #\-)))
				then ; could be last-boundary
				     (if* (< (- end j) 4)
					then ; not enough to tell
					     (return-from scan-forward
					       (values i :partial))
				      elseif (and (eql (aref mpbuffer (+ 2 j))
						       #.(char-code #\return))
						  (eql (aref mpbuffer (+ 3 j))
						       #.(char-code #\linefeed)))
					then ; hit the end
					     (return-from scan-forward
					       (values i :last-boundary))))
			     ; if here then doesn't match boundary
			     (return)
		      elseif (>= j end)
			then ; end of buffer before end of boundary
			     (return-from scan-forward
			       (values i :partial)))
		     
		     (if* (not (eq (aref mpbuffer j)
				   (aref boundary ind)))
			then (return))))))))))
       
		     
		     
						
						
					 
					 
	   
	   
	       
		       
	       
  
  
    



(defmethod get-multipart-sequence ((req http-request)
				   buffer
				   &key (start 0)
					(end (length buffer))
					(external-format :latin1-base ef-spec))
  ;; fill the buffer with the chunk of data.
  ;; start at 'start' and go no farther than (1- end) in the buffer
  ;; return the index of the first character not placed in the buffer.
  
  
  ;; Since external-format not used in all versions
  (declare (ignorable external-format ef-spec))

  #-(and allegro (version>= 6 0 pre-final 1))
  (if* ef-spec
     then (warn "~
For this version of Lisp, external-format is ignored ~
in get-multipart-sequence"))

  (let* ((mp-info (getf (request-reply-plist req) 'mp-info))
	 mpbuffer 
	 cur
	 pos
	 kind
	 text-mode)

    (typecase buffer
      ((array (unsigned-byte 8) (*))
       )
      ((array character (*))
       (setq text-mode t))
      (t 
       (error 
	"This function only accepts (array (unsigned-byte 8)) or character arrays")))
    (if* (null mp-info)
       then (error "get-multipart-sequence called before get-mulipart-header"))
    
    (setq mpbuffer (mp-info-buffer mp-info)
	  cur      (mp-info-cur mp-info))

    (loop
      (case (mp-info-state mp-info)
	((:header :boundary :last-boundary)
	 ; no data left
	 (return-from get-multipart-sequence nil))
	(:start
	 (error "get-multipart-sequence called before get-mulipart-header"))
	((:body :partial)
	 (if* (eq (mp-info-state mp-info) :partial)
	    then ; this was set below. we will return the partial
		 ; at then end of the buffer
		 (setf (mp-info-state mp-info) :body)
		 (setq pos (mp-info-end mp-info))
	    else (multiple-value-setq (pos kind) (scan-forward mp-info))
		 (setq cur (mp-info-cur mp-info)) ; scan-forward can change
		 )
	 (if* (> pos cur)
	    then ; got something to return
		 (let* ((tocopy (min (- end start) (- pos cur)))
			(items tocopy))
		   (if* text-mode
		      then 
			   ; here is where we should do
			   ; external format processing
			   #+(and allegro (version>= 6 0 pre-final 1))
			   (multiple-value-setq (buffer items tocopy)
			     (octets-to-string
			      mpbuffer
			      :string buffer
			      :start cur
			      :end (+ cur tocopy)
			      :string-start start
			      :string-end (length buffer)
			      :external-format external-format
			      :truncate t))
			   #-(and allegro (version>= 6 0 pre-final 1))
			   (dotimes (i tocopy)
			     (setf (aref buffer (+ start i))
			       (code-char (aref mpbuffer (+ cur i)))))
		      else 
			   (dotimes (i tocopy)
			     (setf (aref buffer (+ start i))
			       (aref mpbuffer (+ cur i)))))
		   (setf (mp-info-cur mp-info) (+ cur tocopy))
		   (return-from get-multipart-sequence (+ start items)))
	  elseif (eq kind :partial)
	    then  ; may be a boundary, can't tell
		 (if* (null (shift-buffer-up-and-read mp-info))
		    then ; no more data, partial will never match
			 ; so return the partial, this special
			 ; state is recognized in this routine
			 (setf (mp-info-state mp-info) :partial)
			 ; loop around
			 )
	  elseif (or (eq kind :boundary)
		     (eq kind :last-boundary))
	    then ; hit a boundary, nothing more to return
		 (setf (mp-info-state mp-info) kind
		       (mp-info-cur   mp-info) pos)
		 (return-from get-multipart-sequence nil)))))))

  



;; end multipart code







		      
	      
(defun read-sequence-with-timeout (string length sock timeout)
  ;; read length bytes into sequence, timing out after timeout
  ;; seconds
  ;; return nil if things go wrong.
  (mp:with-timeout (timeout nil)
    (let ((got 0))
      (loop
	(let ((this (rational-read-sequence string sock :start got)))
	  (if* (<= this 0)
	     then (return nil) ; eof too early
	     else (setq  got    this)
		  (if* (>= got length ) then (return string))))))))
		
    
      

(defun read-sock-line (sock buffer start)
  ;; read a line of data into the socket buffer, starting at start.
  ;; return  buffer and index after last character in buffer.
  ;; get bigger buffer if needed.
  ;; If problems occur free the passed in buffer and return nil.
  ;;
  
  (let ((max (length buffer))
	(prevch))
    (loop
      (let ((ch (read-char sock nil :eof)))
	(if* (eq ch :eof)
	   then (debug-format :info"eof on socket~%")
		(free-request-buffer buffer)
		(return-from read-sock-line nil))
      
	(if* (eq ch #\linefeed)
	   then (if* (eq prevch #\return)
		   then (decf start) ; back up to toss out return
			)
		(setf (schar buffer start) #\null) ; null terminate
		
		; debug output
		; dump out buffer
		(debug-format :info "read on socket: ")
		(if-debug-action :info
				 (dotimes (i start)
				   (write-char (schar buffer i) *initial-terminal-io*))
				 (terpri *initial-terminal-io*))
		;; end debug
			
		(return-from read-sock-line (values buffer start))
	   else ; store character
		(if* (>= start max)
		   then ; must grow the string
			(let ((new-buffer (get-request-buffer (+ max 1024))))
			  (if* (null new-buffer)
			     then ;; too large, give up
				  (free-request-buffer buffer)
				  (return-from read-sock-line nil)
			     else ; got it
				  (dotimes (i start)
				    (setf (schar new-buffer i) 
				      (schar buffer i)))
				  (setq max (length new-buffer))
				  (free-request-buffer buffer)
				  (setq buffer new-buffer))))
		;  buffer is big enough
		(setf (schar buffer start) ch)
		(incf start))
	
	(setq prevch ch)))))
      
		      
				  
(defmethod request-query ((req http-request) &key (post t) (uri t)
						  (external-format
						   :latin1-base))
  ;; decode if necessary and return the alist holding the
  ;; args to this url.  In the alist items the value is the 
  ;; cdr of the alist item.
  ;;
  ;; If uri is true then we look for query information in the uri
  ;; (following a question mark)
  ;; If post is true and this is a post request then we look for
  ;; query information in the body of the query.
  ;; If both are true (and this is a post) then we look both places.
  ;;
  ;;
  (let ((alist (request-query-alist req))
	(signature (cons post uri)))
    
    (if* (not (eq alist :empty))
       then (let ((given-sig (getf (request-reply-plist req) 
				   'request-query-sig)))
	      (if* (equal given-sig signature)
		 then ; same args as before, cached value is legit
		      (return-from request-query alist))))
    
    (let (res)
      (if* uri
	 then (let ((arg (uri-query (request-uri req))))
		(if* arg
		   then (setq res (form-urlencoded-to-query
				   arg
				   :external-format external-format)))))
	      
      (if* post
	 then (if* (and (eq (request-method req) :post)
			(equal (header-slot-value req :content-type)
			    "application/x-www-form-urlencoded"))
		 then (setf res
			(append res
				(form-urlencoded-to-query
				 (get-request-body req)
				 :external-format external-format)))))
      (setf (getf (request-reply-plist req) 'request-query-sig)
	signature)
      (setf (request-query-alist req) res))))
			

(defun request-query-value (key req &key (post t) (uri t) (test #'equal))
  ;; access the value of the given key in the request's 
  ;; request query.  We do this so often that it's useful
  ;; to make this a function
  (cdr (assoc key (request-query req :post post :uri uri) :test test)))


	

(defun header-decode-integer (val)
  ;; if val is a string holding an integer return its value
  ;; and t,
  ;; else nil
  (if* val 
     then (let (ans)
	    (setq ans (string-to-number val 0 (length val)))
	    (if* (integerp ans)
	       then (values ans t)))))


(defun date-to-universal-time (date)
  ;; convert  a date string to lisp's universal time
  ;; we accept all 3 possible date formats

  (flet ((cvt (str start-end)
	   (let ((res 0))
	     (do ((i (car start-end) (1+ i))
		  (end (cdr start-end)))
		 ((>= i end) res)
	       (setq res 
		 (+ (* 10 res)
		    (- (char-code (schar str i)) #.(char-code #\0))))))))
    ;; check preferred type first (rfc1123 (formerly refc822)):
    ;;  	Sun, 06 Nov 1994 08:49:37 GMT
    (multiple-value-bind (ok whole
			  day
			  month
			  year
			  hour
			  minute
			  second)
	(match-regexp 
	 "[A-Za-z]+, \\([0-9]+\\) \\([A-Za-z]+\\) \\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) GMT"
	 date
	 :return :index)
      (declare (ignore whole))
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year)
		 0))))
    
    ;; now second best format (but used by Netscape sadly):
    ;;		Sunday, 06-Nov-94 08:49:37 GMT
    ;;
    (multiple-value-bind (ok whole
			  day
			  month
			  year
			  hour
			  minute
			  second)
	(match-regexp
	 
	 "[A-Za-z]+, \\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) GMT"
	 date
	 :return :index)
      
      (declare (ignore whole))
      
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year) ; cl does right thing with 2 digit dates
		 0))))
    
    
    ;; finally the third format, from unix's asctime
    ;;     Sun Nov  6 08:49:37 1994
    (multiple-value-bind (ok whole
			  month
			  day
			  hour
			  minute
			  second
			  year
			  )
	(match-regexp
	 
	 "[A-Za-z]+ \\([A-Za-z]+\\) +\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\)"
	 date
	 :return :index)
      
      (declare (ignore whole))
      
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year)
		 0))))
      
      
    ))
    
    
    
    
	  
(defun compute-month (str start)
  ;; return the month number given a 3char rep of the string
  
  (case (schar str start)
    (#\A  
     (if* (eq (schar str (1+ start)) #\p)
	then 4 ; april
	else 8 ; august
	     ))
    (#\D 12) ; dec
    (#\F 2 ) ; feb
    (#\J
     (if* (eq (schar str (1+ start)) #\a)
	then 1 ; jan
      elseif (eq (schar str (+ 2 start)) #\l)
	then 7 ; july
	else 6 ; june
	     ))
    (#\M
     (if* (eq (schar str (+ 2 start)) #\r)
	then 3 ; march
	else 5 ; may
	     ))
    (#\N 11) ; nov
    (#\O 10)  ;oct
    (#\S 9) ; sept
    ))
    
     

(defun maybe-universal-time-to-date (ut-or-string &optional (time-zone 0))
  ;; given a ut or a string, only do the conversion on the string
  (if* (stringp ut-or-string) 
     then ut-or-string
     else (universal-time-to-date ut-or-string time-zone)))

(defparameter *saved-ut-to-date* nil)

(defun universal-time-to-date (ut &optional (time-zone 0))
  ;; convert a lisp universal time to rfc 1123 date
  ;;
  (let ((cval *saved-ut-to-date*))
    (if* (and (eql ut (caar cval))
	      (eql time-zone (cdar cval)))
       then ; turns out we often repeatedly ask for the same conversion
	    (cdr cval)
       else
	    (let ((*print-pretty* nil))
	      (multiple-value-bind
		  (sec min hour date month year day-of-week dsp tz)
		  (decode-universal-time ut time-zone)
		(declare (ignore tz dsp))
		(let ((ans
		       (format
			nil
			"~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d~@[ GMT~]"
			(svref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
			       day-of-week)
			date
			(svref '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
				  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
			       month)
			year
			hour
			min
			sec
			(= 0 time-zone))))
		  (setf *saved-ut-to-date* (cons (cons ut time-zone) ans))
		  ans))))))



;; ----- simple resource

(defstruct sresource
  data	 ; list of buffers
  create ; create new object for the buffer
  init	 ; optional - used to init buffers taken off the free list
  )

(defun create-sresource (&key create init)
  (make-sresource :create create :init init))

(defun get-sresource (sresource &optional size)
  ;; get a new resource. If size is given then ask for at least that
  ;; size
  (let (to-return)
    ;; force new ones to be allocated
    (mp:without-scheduling 
      (let ((buffers (sresource-data sresource)))
	(if* size
	   then ; must get one of at least a certain size
		(dolist (buf buffers)
		  (if* (>= (length buf) size)
		     then (setf (sresource-data sresource)
			    (delete buf buffers :test #'eq))
			  (setq to-return buf)
			  (return)))
	    
		; none big enough
	      
	   else ; just get any buffer
		(if* buffers
		   then (setf (sresource-data sresource) (cdr buffers))
			(setq to-return (car buffers)))
		
		)))
  
    (if* to-return
       then ; found one to return, must init
	    
	    (let ((init (sresource-init sresource)))
	      (if* init
		 then (funcall init sresource to-return)))
	    to-return
       else ; none big enough, so get a new buffer.
	    (funcall (sresource-create sresource)
		     sresource
		     size))))
  
(defun free-sresource (sresource buffer)
  ;; return a resource to the pool
  ;; we silently ignore nil being passed in as a buffer
  (if* buffer 
     then (mp:without-scheduling
	    ;; if debugging
	    (if* (member buffer (sresource-data sresource) :test #'eq)
	       then (error "freeing freed buffer"))
	    ;;
	    
	    (push buffer (sresource-data sresource)))))




;; ----- scratch buffer resource:

(defparameter *request-buffer-sresource* 
    (create-sresource 
     :create #'(lambda (sresource &optional size)
			  (declare (ignore sresource))
			  (make-array (or size 2048)
				      :element-type 'character))))

(defun get-request-buffer (&optional size)
  (get-sresource *request-buffer-sresource* size))

(defun free-request-buffer (buffer)
  (free-sresource *request-buffer-sresource* buffer))



	    
;;-----------------


(defun string-to-number (string start end)
  ;; convert the string into a number.
  ;; the number is decimal
  ;; this is faster than creating a string input stream and
  ;; doing a lisp read
  ;; string must be a simple string
  (let ((ans 0))
    (do ((i start (1+ i)))
	((>= i end)
	 ans)
      (let ((digit (- (char-code (schar string i)) #.(char-code #\0))))
	(if* (<= 0 digit 9)
	   then (setq ans (+ (* ans 10) digit))
	   else (return ans))))))

		
	
;;-------------------
;; authorization

(defmethod get-basic-authorization ((req http-request))
  ;; return the basic authorization information for this request, if any
  ;; 
  ;; basic authorization is used when a name/password dialog is
  ;; put up by the browser
  ;;
  ;; if authorization info in found this request, return two values
  ;;  name
  ;;  password
  ;;
  (let ((auth-value (header-slot-value req :authorization)))
    (if* auth-value
       then (let ((words (split-into-words auth-value)))
	      (if* (equalp (car words) "basic")
		 then (setq auth-value 
			(split-on-character (base64-decode (cadr words)) #\:))
		      (values-list auth-value))))))
		      
	      
(defmethod set-basic-authorization ((req http-request) realm)
  ;; declare that you want to get authentication information
  ;; for the given realm.
  ;; This must be called after with-http-response and before
  ;; with-http-body
  (setq realm (string realm))
  (setf (reply-header-slot-value req :www-authenticate)
    (format nil "Basic realm=~s" realm)))
    


;=======

(defun bulk-set-reply-headers (req headers)
  ;; given an alist list of headers to set, set the header info
  ;; in the correct place (given fast vrs slow headers)
  (let ((fast-headers *fast-reply-headers*)
	(current-headers (request-reply-headers req)))
    (dolist (header headers)
      (let ((this (car header))
	    (ent))
	(if* (setq ent (assoc this fast-headers :test #'eq))
	   then ; a fast one
		(setf (slot-value req (second ent)) (cdr header))
	   else ; a slow one
		(if* (null (setq ent (assoc this 
					    current-headers :test #'eq)))
		   then ; not present yet
			(push (setq ent (cons this nil)) current-headers))
		(setf (cdr ent) (cdr header)))))
    (setf (request-reply-headers req) current-headers)))
	

(defun code-to-response (code)
  ;; return response object for the given code
  (let ((obj (find code *responses* :key #'response-number)))
    (if* (null obj)
       then (push (setq obj (make-resp code "unknown code")) *responses*))
    obj))
  
