(in-package :neo)


(defclass entity ()
  ;; an object to be published
  ;; host and port may be nil, meaning "don't care"
  ((host :initarg :host
	 :initform nil
	 :reader host)
   (port :initarg :port
	 :initform nil
	 :reader port)
   (url  :initarg :url
	 :reader url)
   (location :initarg :location
	     :reader location)
   (prefix :initarg :prefix
	   :initform nil
	   :reader prefix)
   (last-modified :initarg :last-modified
		  :accessor last-modified
		  :initform nil ; means always considered new
		  )
   
   )
  )


(defclass file-entity (entity)
    ;; a file to be published
    ((mime-type :initarg :mime-type :reader mime-type)
     (file  :initarg :file :reader file)
     (contents :initarg :contents :reader contents
	       :initform nil)
     
     ))


(defclass computed-entity (entity)
  ;; entity computed each time it's called
  ((function :initarg :function :reader entity-function)))



(defclass directory-entity (entity)
  ;; entity that displays the contents of a directory
  ((directory :initarg :directory ; directory to display
	      :reader entity-directory)
   (recurse   :initarg :recurse	   ; t to descend to sub directories
	      :initform nil
	      :reader recurse
	      ))
  )


; we can specify either an exact url or one that handles all
; urls with a common prefix.
;
(defvar *exact-url* (make-hash-table :test #'equal))
(defvar *prefix-url* (make-hash-table :test #'equal))




;; methods on entity objects

;-- content-length -- how long is the body of the response, if we know

(defmethod content-length ((ent entity))
  ;; by default we don't know, and that's what nil mean
  nil)

(defmethod content-length ((ent file-entity))
  (let ((contents (contents ent)))
    (if* contents
       then (length contents)
       else ; may be a file on the disk, we could
	    ; compute it.. this is
	    ;** to be done
	    nil)))



;- transfer-mode - will the body be sent in :text or :binary mode.
;  use :binary if you're not sure

(defmethod transfer-mode ((ent entity))
  :binary   ; the default
  )








  


;; url exporting

(defun publish (&key host port url function class)
  ;; publish the given url
  ;; if file is given then it specifies a file to return
  ;; 
  (let ((ent (make-instance (or class 'computed-entity)
	       :host host
	       :port port
	       :url  url
	       :function function)))
    (setf (gethash url *exact-url*) ent))
  )
	     

(defun publish-file (&key host port url file mime-type class preload)
  ;; return the given file as the value of the url
  ;; for the given host.
  ;; If host is nil then return for any host
  (let (ent got)
    (if* preload
       then ; keep the content in core for fast display
	    (with-open-file (p file :element-type '(unsigned-byte 8))
	      (let ((size (excl::filesys-size (stream-input-fn p)))
		    (lastmod (excl::filesys-write-date (stream-input-fn p)))
		    (guts))
		(setq guts (make-array size :element-type '(unsigned-byte 8)))
	      
		(if* (not (eql size (setq got (read-sequence guts p))))
		   then (error "~s should have been ~d bytes but was ~d"
			       file
			       size
			       got))
		(setq ent (make-instance (or class 'file-entity)
			    :host host
			    :port port
			    :url  url
			    :file file
			    :mime-type mime-type
			    :contents guts
			    :last-modified lastmod))))
       else (setq ent (make-instance (or class 'file-entity)
			:host host
			:port port
			:url  url
			:file file
			:mime-type mime-type)))
  
    (setf (gethash url *exact-url*) ent)))





(defmethod handle-request ((req http-request-0.9))
  ;; do the server response to the given request
  
  ; look for an exact match
  (let ((entity (gethash (url req) *exact-url*)))
    (if* entity
       then (let ((entity-host (host entity)))
	      (if* entity-host
		 then ; must do a host match
		      (let ((req-host (host req)))
			(if* req-host 
			   then (if* (equal req-host entity-host)
				   then (return-from handle-request
					  (process-entity req entity)))
			   else ; no host given, don't do it
				nil))
		 else ; no host specified in entity, do so it
		      (return-from handle-request
			(process-entity req entity))))))
  
  ; do a partial match
  
  ; fill it in

  ; no match, it failed
  (failed-request req))



(defmethod failed-request ((req http-request-0.9))
  (with-http-response (*response-not-found* req "text/html")
    (html "The request for "
	  (:princ-safe (url req))
	  " was not found on this server.")))


(defmethod handle-request ((req xhttp-request))
  ;; do the server response to the given request
  
  ; look for an exact match
  (let ((entity (gethash (url req) *exact-url*)))
    (if* entity
       then (let ((entity-host (host entity)))
	      (if* entity-host
		 then ; must do a host match
		      (let ((req-host (host req)))
			(if* req-host 
			   then (if* (equal req-host entity-host)
				   then (return-from handle-request
					  (process-entity req entity)))
			   else ; no host given, don't do it
				nil))
		 else ; no host specified in entity, do so it
		      (return-from handle-request
			(process-entity req entity))))))
  
  ; do a partial match
  
  ; fill it in

  ; no match, it failed
  (failed-request req))
	  
				


#+ignore
(defmethod process-entity ((req http-request-0.9) (entity computed-entity))
  ;; 
  (let ((fcn (entity-function entity)))
    (funcall fcn req entity)))

#+ignore
(defmethod process-entity ((req http-request-0.9) (entity file-entity))
  ;; send a file's contents back
  (let ((contents (contents entity)))
    (if* contents
       then ; preloaded
	    (format t "did fast file out~%")
	    (with-fixed-response (*response-ok*
				  req
				  (mime-type entity)
				  :content-length (length contents)
				  :last-modified (last-modified entity)
				  )
	      (socket:set-socket-format *response-stream* :binary)
	      (write-sequence contents *response-stream*)
	      (force-output *response-stream*))
       else 
	    (handler-case (with-open-file (p (file entity) :direction :input
					   :element-type '(unsigned-byte 8))
			    (let ((size (excl::filesys-size (stream-input-fn p))))
			      (with-fixed-response (*response-ok* 
						    req (mime-type entity)
						    :content-length size
						    :last-modified
						    (excl::filesys-write-date
						     (stream-input-fn p)))
				(socket:set-socket-format *response-stream* :binary)
				(let ((buffer (make-array 1024 :element-type
							  '(unsigned-byte 8))))
				  (loop (let ((count (read-sequence buffer p)))
					  (if* (> count 0)
					     then (write-sequence buffer 
								  *response-stream*
								  :end count)
					     else (return)))))
				(force-output *response-stream*))))
	      (error (condition)
		(with-http-response (*response-not-found* req "text/html")
		  (html (:head (:title "Not Found"))
			(:body "The object cannot be found"))))))))



(defmacro with-http-response2 ((req ent
				&key (timeout 60)
				     (check-modified t))
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
	   (,g-check-modified ,check-modified))
       (catch 'with-http-response
	 (compute-strategy ,g-req ,g-ent)
	 ;(keep-alive-check ,g-req ,g-ent)
	 (up-to-date-check ,g-check-modified ,g-req ,g-ent)
	 (mp::with-timeout ((if* (> ,g-timeout 0)
			       then ,g-timeout
			       else 9999999)
			    (timedout-response ,g-req ,g-ent))
	   ,@body
	   )))))


(defmacro with-http-body ((req ent &key format)
			  &rest body)
    (let ((g-req (gensym))
	(g-ent (gensym))
	  (g-format (gensym)))
      `(let ((,g-req ,req)
	     (,g-ent ,ent)
	     (,g-format ,format))
	 (declare (ignore-if-unused ,g-req ,g-ent ,g-format))
	 ; (compute-response-headers ,g-req ,g-ent)
	 ,(if* body 
	     then `(compute-response-stream ,g-req ,g-ent))
	 (send-response-headers ,g-req ,g-ent :pre)
	 (if* (not (member :omit-body (resp-strategy ,g-req)))
	    then (progn ,@body))
	 (send-response-headers ,g-req ,g-ent :post))))
			  
	 
	       
	       
       
       
  
(defmethod process-entity ((req xhttp-request) (ent file-entity))
    
  (let ((contents (contents ent)))
    (if* contents
       then ;(preloaded)
	    ; set the response code and 
	    ; and header fields then dump the value
	      
	    ; * should check for range here
	    ; for now we'll send it all
	    (with-http-response2 (req ent)
	      (setf (resp-code req) *response-ok*)
	      (setf (resp-content-length req) (length contents))
	      (setf (resp-content-type req) (mime-type ent))
	      (push (cons "Last-Modified"
			  (universal-time-to-date 
			   (min (resp-date req) 
				(last-modified ent))))
		    (resp-headers req))
	      
	      (with-http-body (req ent :format :binary)
		;; at this point the header are out and we have a stream
		;; to write to 
		(write-sequence contents (resp-stream req))
		))
       else ; the non-preloaded case
	    (let (p)
	      
	      (setf (last-modified ent) nil) ; forget previous cached value
	      
	      (if* (null (errorset 
			  (setq p (open (file ent) 
					:direction :input
					:element-type '(unsigned-byte 8)))))
		 then ; file not readable
		      (with-http-response2 (req ent)
			(setf (resp-code req) *response-not-found*)
			(with-http-body (req ent)))
		      (return-from process-entity nil))
	      
	      (unwind-protect 
		  (progn
		    (let ((size (excl::filesys-size (stream-input-fn p)))
			  (lastmod (excl::filesys-write-date 
				    (stream-input-fn p)))
			  (buffer (make-array 1024 
					      :element-type '(unsigned-byte 8))))
		      (declare (dynamic-extent buffer))
		      
		      (setf (last-modified ent) lastmod)
		      (with-http-response2 (req ent)

			(setf (resp-code req) *response-ok*)
			(setf (resp-content-length req) size)
			(push (cons "Last-Modified"
				    (universal-time-to-date 
				     (min (resp-date req) lastmod)))
			      (resp-headers req))
			(setf (resp-content-type req) (mime-type ent))
			
			(with-http-body (req ent :format :binary)
			  (loop
			    (if* (<= size 0) then (return))
			    (let ((got (read-sequence buffer 
						      p :end 
						      (min size 1024))))
			      (if* (<= got 0) then (return))
			      (write-sequence buffer (resp-stream req)
					      :end got)
			      (decf size got)))))))
		      
		      
		(close p))))))
	      
		


		
(defun up-to-date-check (doit req ent)
  ;; if doit is true and the request req has an
  ;; if-modified-since or if-unmodified-since then
  ;; check if it applies and this resuits in a response
  ;; we can return right away then do it and 
  ;; throw to abort the rest of the body being run
  
  ; to be done
  
  (if* (not doit)
     then ; we dont' even care
	  (return-from up-to-date-check nil))
  
  (let ((if-modified-since (header-slot-value "if-modified-since" req)))
    (if* if-modified-since
       then (setq if-modified-since
	      (date-to-universal-time if-modified-since)))
    
    (if* if-modified-since
       then ; valid date, do the check
	    (if* (and (last-modified ent) 
		      (<= (last-modified ent) if-modified-since))
	       then ; send back a message that it is already
		    ; up to date
		    (setf (resp-code req) *response-not-modified*)
		    (with-http-body (req ent)
		      ;; force out the header
		      )
		    (throw 'with-http-response nil) ; and quick exit
		    ))))

    
		    
    
    



(defvar *enable-keep-alive* t)

#+ignore
(defmethod keep-alive-check ((req xhttp-request) (ent entity))
  ;; check to see if our response should be a keep alive
  ;; kinda response
  ;; this may be overruled later on.
  ;;
  ;; our strategy:
  ;;   Initially we'll do keep alive if the caller wants it
  ;; 
  ;; other factors:
  ;;   if we're running a one thread simple web server then we
  ;;   don't want keep alives since that will prevent other
  ;;   requests while the process with the keep alive has it.
  ;;  
  ;;   for multithread servers we would want to disallow keep alives
  ;;	we we started to run out of free threads.
  (setf (resp-keep-alive req) 
    (and *enable-keep-alive*
	 (equalp "keep-alive" (header-slot-value "connection" req)))))
  

(defvar *enable-chunking* t) ; until we can figure it out

#+ignore 
(defmethod compute-response-headers ((req xhttp-request) (ent entity))
  ;; may fill this in later on
  (if* (and ; (resp-keep-alive req) 
	    *enable-chunking*
	    (eq (protocol req) :http/1.1))
     then (setf (resp-transfer-encoding req) :chunked)
	  (logmess "using chunking"))
  nil)



(defmethod compute-strategy ((req xhttp-request) (ent entity))
  ;; determine how we'll respond to this request
  
  (let ((strategy nil))
    (if* (eq (command req) :head)
       then ; head commands are particularly easy to reply to
	    (setq strategy '(:use-socket-stream
			     :omit-body))
	    
	    (if* (and *enable-keep-alive*
		      (equalp "keep-alive" 
			      (header-slot-value "connection" req)))
	       then (push :keep-alive strategy))
	    
     elseif (and  ;; assert: get command
	     *enable-chunking* 
	     (eq (protocol req) :http/1.1)
	     (null (content-length ent)))
       then (setq strategy '(:chunked :use-socket-stream))
       else ; can't chunk, let's see if keep alive is requested
	    (if* (and *enable-keep-alive*
		      (equalp "keep-alive" 
			      (header-slot-value "connection" req)))
	       then ; a keep alive is requested..
		    ; we may want reject this if we are running
		    ; short of processes to handle requests.
		    ; For now we'll accept it if we can.
		    
		    (if* (eq (transfer-mode ent) :binary)
		       then ; can't create binary stream string
			    ; must not keep alive
			    (setq strategy
			      '(:use-socket-stream
				; no keep alive
				))
		       else ; can build string stream
			    (setq strategy
			      '(:string-output-stream
				:keep-alive
				:post-headers)))
		    
		    ; keep alive not requested
		    (setq strategy '(:use-socket-stream
				     ))))
    
    ;;  save it
    
    (setf (resp-strategy req) strategy)
    
    ))
			     
		    
		    
		    
		    
    
    
	    

(defmethod send-response-headers ((req xhttp-request) (ent entity) time)
  ;; we have all the info to send out the headers of our response
    
  (mp:with-timeout (60 (logmess "timeout during header send")
		       (setf (resp-keep-alive req) nil)
		       (throw 'with-http-response nil))
    (let* ((sock (socket req))
	   (strategy (resp-strategy req))
	   (post-headers (member :post-headers strategy :test #'eq))
	   (content)
	   (chunked-p nil)
	   (send-headers
	    (if* post-headers
	       then (eq time :post)
	       else (eq time :pre))
	    ))
      
      
      
      (if* send-headers
	 then (let ((code (resp-code req)))
		(format sock "~a ~d  ~a~a"
			(protocol-string req)
			(response-number code)
			(response-desc   code)
			*crlf*)))
      
      (if* (and post-headers
		(eq time :post)
		(member :string-output-stream strategy :test #'eq))
	 then ; must get data to send from the string output stream
	      (setq content (get-output-stream-string 
			     (resp-stream req)))
	      (setf (resp-content-length req) (length content)))
      	
      (if* (and send-headers
		(not (eq (protocol req) :http/0.9)))
	 then ; can put out headers
	      (format sock "Date: ~a~a" 
		      (universal-time-to-date (resp-date req))
		      *crlf*)

	      (if* (member :keep-alive strategy :test #'eq)
		 then (format sock "Connection: Keep-Alive~aKeep-Alive: timeout=~d~a"
			      *crlf*
			      *read-request-timeout*
			      *crlf*)
		 else (format sock "Connection: Close~a" *crlf*))
      
	      (format sock "Server: neo/0.1~a" *crlf*)
      
	      (if* (resp-content-type req)
		 then (format sock "Content-Type: ~a~a" 
			      (resp-content-type req)
			      *crlf*))

	      (if* (member :chunked strategy :test #'eq)
		 then (format sock "Transfer-Encoding: Chunked~a"
			      *crlf*)
		      (setq chunked-p t))
	      
	      (if* (and (not chunked-p)
			(resp-content-length req))
		 then (format sock "Content-Length: ~d~a"
			      (resp-content-length req)      
			      *crlf*))
	      
	      (dolist (head (resp-headers req))
		(format sock "~a: ~a~a"
			(car head)
			(cdr head)
			*crlf*))
	      (write-string *crlf* sock))
      
      (if* (and send-headers chunk-p)
	      (if* (eq time :pre))
	 then (force-output sock)
	      (socket:socket-control sock :output-chunking t)
	 else ; shut down chunking
	      (socket:socket-control sock :output-chunking-eof t)
	      (write-sequence *crlf* sock))
      
      
      ; if we did post-headers then there's a string input
      ; stream to dump out.
      (if* content
	 then (write-sequence content sock)))))

      	
      
(defmethod compute-response-stream ((req xhttp-request) (ent file-entity))
  ;; send directly to the socket since we already know the length
  ;; may have to switch to chunking however.
  ;;
  (let ((sock (socket req)))
    (if* (eq :chunked (resp-transfer-encoding req))
       then (force-output sock)
	    (socket:socket-control sock :output-chunking t))
    (setf (resp-stream req) sock)))

(defmethod finish-response-stream ((req xhttp-request) (ent file-entity))
  ;; we've finished the body.
  (let ((sock (socket req)))
    (if* (eq :chunked (resp-transfer-encoding req))
       then (socket:socket-control sock :output-chunking-eof t)
	    (write-sequence *crlf* sock))))



  
  

	  
      



		    
		     
		     
		    
		  
    
