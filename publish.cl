;; neo
;; url publishing
;;
;; $Id: publish.cl,v 1.11 1999/08/10 17:16:37 jkf Exp $
;;


(in-package :neo)


(defclass entity ()
  ;; an object to be published
  ;; host and port may be nil, meaning "don't care", or a list of
  ;; items or just an item
  ((host 
    :initarg :host
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
   (format :initarg :format  ;; :text or :binary
	   :reader  entity-format)
   
   (content-type :initarg :content-type
		 :reader content-type
		 :initform nil)
   
   )
  )


(defclass file-entity (entity)
    ;; a file to be published
    (
     (file  :initarg :file :reader file)
     (contents :initarg :contents :reader contents
	       :initform nil)
     (dependencies 
      ;; list of (filename . lastmodifiedtime) 
      ;; for each of the files that this file includes
      :initarg :dependencies
		   :initform nil
		   :accessor dependencies)
     
     ))


(defclass computed-entity (entity)
  ;; entity computed each time it's called
  ((function :initarg :function :reader entity-function)))



(defclass directory-entity (entity)
  ;; entity that displays the contents of a directory
  ((directory :initarg :directory ; directory to display
	      :reader entity-directory)
   (prefix    :initarg :prefix   ; url prefix pointing to ths dir
	      :reader prefix
	      :initform "")
   (recurse   :initarg :recurse	   ; t to descend to sub directories
	      :initform nil
	      :reader recurse
	      ))
  )


; we can specify either an exact url or one that handles all
; urls with a common prefix.
;

(defvar *mime-types* (make-hash-table :test #'equal))
(setf (gethash "html" *mime-types*) "text/html")
(setf (gethash "htm"  *mime-types*) "text/html")
(setf (gethash "gif"  *mime-types*) "image/gif")
(setf (gethash "jpg"  *mime-types*) "image/jpeg")



(defun unpublish (&key all)
  (if* all
     then (clrhash (wserver-exact-url *wserver*))
	  (setf (wserver-prefix-url *wserver*) nil)
     else (error "not done yet")))

  
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
  (or (entity-format ent) :binary)
  )








  


;; url exporting

(defun publish (&key host port url function class format
		     content-type
		     (server *wserver*))
  ;; publish the given url
  ;; if file is given then it specifies a file to return
  ;; 
  (let ((ent (make-instance (or class 'computed-entity)
	       :host host
	       :port port
	       :url  url
	       :function function
	       :format format
	       :content-type content-type)))
    (setf (gethash url (wserver-exact-url server)) ent))
  )
	     

(defun publish-file (&key (server *wserver*)
			  host port url file content-type class preload)
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
			    :content-type content-type
			    :contents  guts
			    :last-modified lastmod))))
       else (setq ent (make-instance (or class 'file-entity)
			:host host
			:port port
			:url  url
			:file file
			:content-type content-type)))
  
    (setf (gethash url (wserver-exact-url server)) ent)))





(defun publish-directory (&key prefix 
			       host
			       port
			       destination
			       (server *wserver*)
			       
			       )
  
  ;; make a whole directory available
  (push (cons prefix (make-instance 'directory-entity 
		       :directory destination
		       :prefix prefix
		       :host host
		       :port port
		       ))
	  (wserver-prefix-url server)))


			   






(defmethod handle-request ((req http-request))
  ;; do the server response to the given request
  
  ; look for an exact match
  (let ((entity (gethash (url req) (wserver-exact-url *wserver*))))
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
  
  (let* ((url (url req))
	 (len-url (length url)))
	     
    (dolist (entpair (wserver-prefix-url *wserver*))
      (if* (and (>= len-url (length (car entpair)))
		(buffer-match url 0 (car entpair)))
	 then ; we may already be a weiner
	      (if* (process-entity req (cdr entpair))
		 then ; successful
		      (return-from handle-request nil)))))
  

  ; no match, it failed
  (let ((ent (gethash nil (wserver-exact-url *wserver*))))
    (if* (null ent)
       then ; no  global handler, create one
	    (setq ent (publish 
		       :function #'(lambda (req ent)
				     (with-http-response 
					 (req ent
					      :response *response-not-found*)
				       (with-http-body (req ent)
					 (html "The request for "
					       (:princ-safe (url req))
					       " was not found on this server."))))
				     
		       :content-type "text/html"))
	    (setf (gethash nil (wserver-exact-url *wserver*)) ent)
	    )
    (process-entity req ent)))
	  
				



(defmethod process-entity ((req http-request) (entity computed-entity))
  ;; 
  (let ((fcn (entity-function entity)))
    (funcall fcn req entity)))




       
  
(defmethod process-entity ((req http-request) (ent file-entity))
    
  (let ((contents (contents ent)))
    (if* contents
       then ;(preloaded)
	    ; set the response code and 
	    ; and header fields then dump the value
	      
	    ; * should check for range here
	    ; for now we'll send it all
	    (with-http-response (req ent
				     :content-type (content-type ent))
	      (setf (resp-content-length req) (length contents))
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
		      (with-http-response (req ent)
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
		      (with-http-response (req ent)

			(setf (resp-content-length req) size)
			(push (cons "Last-Modified"
				    (universal-time-to-date 
				     (min (resp-date req) lastmod)))
			      (resp-headers req))
			
			
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
	      
		
(defmethod process-entity ((req http-request) (ent directory-entity))
  ;; search for a file in the directory and then create a file
  ;; entity for it so we can track last modified and stu
  
  ; remove the prefix and tack and append to the given directory
  
  (let ((realname (concatenate 'string
		    (entity-directory ent)
		    (subseq (url req) (length (prefix ent)))))
	(newname))
    (debug-format 10 "directory request for ~s~%" realname)
    
    (let ((type (excl::filesys-type realname)))
      (if* (null type)
	 then ; not present
	      (return-from process-entity nil)
       elseif (eq :directory type)
	 then ; we have to try index.html and index.htm
	      (if* (not (eq #\/ (schar realname (1- (length realname)))))
		 then (setq realname (concatenate 'string realname "/")))
	      
	      (if* (eq :file (excl::filesys-type
			      (setq newname
				(concatenate 'string realname "index.html"))))
		 then (setq realname newname)
		      elseif (eq :file (excl::filesys-type
			      (setq newname
				(concatenate 'string realname "index.htm"))))
		 then (setq realname newname)
		 else ; failure
		      (return-from process-entity nil))
       elseif (not (eq :file type))
	 then  ; bizarre object
	      (return-from process-entity nil)))
    
    ;; ok realname is a file.
    ;; create an entity object for it, publish it, and dispatch on it
    
    ; must compute the mime type
    (let ((chpos (find-it-rev #\. realname 0 (length realname)))
	  (mtype "application/octet-stream"))
      (if* chpos
	 then  (let ((ext (subseq realname (1+ chpos))))
		 (setq mtype (or (gethash ext *mime-types*) mtype))))
      
      (process-entity req (publish-file :url (url req) 
		    :file realname
		    :content-type mtype)))
      
    t))
    
     
      
		      
		      
	      
	      
		    
  
  

		
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
  
  (let ((if-modified-since (header-slot-value req "if-modified-since")))
    (if* if-modified-since
       then (setq if-modified-since
	      (date-to-universal-time if-modified-since)))
    
    (if* if-modified-since
       then ; valid date, do the check
	    (if* (and (last-modified ent) 
		      (<= (last-modified ent) if-modified-since))
	       then ; send back a message that it is already
		    ; up to date
		    (debug-format 10 "entity is up to date~%")
		    (setf (resp-code req) *response-not-modified*)
		    (with-http-body (req ent)
		      ;; force out the header
		      )
		    (throw 'with-http-response nil) ; and quick exit
		    ))))

    


(defmethod compute-strategy ((req http-request) (ent entity))
  ;; determine how we'll respond to this request
  
  (let ((strategy nil))
    (if* (eq (command req) :head)
       then ; head commands are particularly easy to reply to
	    (setq strategy '(:use-socket-stream
			     :omit-body))
	    
	    (if* (and (wserver-enable-keep-alive *wserver*)
		      (>= (wserver-free-workers *wserver*) 2)
		      (equalp "keep-alive" 
			      (header-slot-value req "connection" )))
	       then (push :keep-alive strategy))
	    
     elseif (and  ;; assert: get command
	     (wserver-enable-chunking *wserver*)
	     (eq (protocol req) :http/1.1)
	     (null (content-length ent)))
       then (setq strategy '(:chunked :use-socket-stream))
       else ; can't chunk, let's see if keep alive is requested
	    (if* (and (wserver-enable-keep-alive *wserver*)
		      (>= (wserver-free-workers *wserver*) 2)
		      (equalp "keep-alive" 
			      (header-slot-value req "connection")))
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
	       else ; keep alive not requested
		    (setq strategy '(:use-socket-stream
				     ))))
    
    ;;  save it

    (debug-format 10 "strategy is ~s~%" strategy)
    (setf (resp-strategy req) strategy)
    
    ))
			     
		    
(defmethod compute-strategy ((req http-request) (ent file-entity))
  ;; for files we can always use the socket stream and keep alive
  ;; since we konw the file length ahead of time
  
  (let ((keep-alive (and (wserver-enable-keep-alive *wserver*)
			 (>= (wserver-free-workers *wserver*) 2)
			 (equalp "keep-alive" 
				 (header-slot-value req "connection"))))
	(strategy))
    
    (if*  (eq (command req) :get)
       then (setq strategy (if* keep-alive
			      then '(:use-socket-stream :keep-alive)
			      else '(:use-socket-stream)))
       else (setq strategy (call-next-method)))
    
    (debug-format 10 "file strategy is ~s~%" strategy)
    (setf (resp-strategy req) strategy)))

	    
	    
  
		    
		    
    
    
	    

(defmethod send-response-headers ((req http-request) (ent entity) time)
  ;;
  ;; called twice (from with-http-body) in the generation of a response 
  ;; to an http request
  ;; 1. before the body forms are run.  in this case time eq :pre
  ;; 2. after the body forms are run.  in this case  time eq :post
  ;;
  ;; we send the headers out at the time appropriate to the 
  ;; strategy.  We also deal with a body written to a
  ;; string output stream
  ;;
    
  (mp:with-timeout (60 (logmess "timeout during header send")
		       (setf (resp-keep-alive req) nil)
		       (throw 'with-http-response nil))
    (let* ((sock (socket req))
	   (strategy (resp-strategy req))
	   (post-headers (member :post-headers strategy :test #'eq))
	   (content)
	   (chunked-p nil)
	   (code (resp-code req))
	   (send-headers
	    (if* post-headers
	       then (eq time :post)
	       else (eq time :pre))
	    ))
      
      
      
      (if* send-headers
	 then (dformat sock "~a ~d  ~a~a"
			 (protocol-string req)
			 (response-number code)
			 (response-desc   code)
			 *crlf*))
      
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
	      (dformat sock "Date: ~a~a" 
		       (universal-time-to-date (resp-date req))
		       *crlf*)

	      (if* (member :keep-alive strategy :test #'eq)
		 then (dformat sock "Connection: Keep-Alive~aKeep-Alive: timeout=~d~a"
			       *crlf*
			       *read-request-timeout*
			       *crlf*)
		 else (dformat sock "Connection: Close~a" *crlf*))
      
	      (dformat sock "Server: neo/0.1~a" *crlf*)
      
	      (if* (resp-content-type req)
		 then (dformat sock "Content-Type: ~a~a" 
			       (resp-content-type req)
			       *crlf*))

	      (if* (member :chunked strategy :test #'eq)
		 then (dformat sock "Transfer-Encoding: Chunked~a"
			       *crlf*)
		      (setq chunked-p t))
	      
	      (if* (and (not chunked-p)
			(resp-content-length req))
		 then (dformat sock "Content-Length: ~d~a"
			       (resp-content-length req)      
			       *crlf*)
		      (logmess (format nil 
				       "~d ~s - ~d bytes" 
				       (response-number code)
				       (response-desc   code)
				       (resp-content-length req)))
	       elseif chunked-p
		 then (logmess (format nil 
				       "~d ~s - chunked" 
				       (response-number code)
				       (response-desc   code)
				       ))
		 else (logmess (format nil 
				       "~d ~s - unknown length" 
				       (response-number code)
				       (response-desc   code)
				       )))
	      
	      (dolist (head (resp-headers req))
		(dformat sock "~a: ~a~a"
			 (car head)
			 (cdr head)
			 *crlf*))
	      (dformat sock "~a" *crlf*))
      
      (if* (and send-headers chunked-p)
	 then (if* (eq time :pre)
		 then (force-output sock)
		      (socket:socket-control sock :output-chunking t)
		 else ; shut down chunking
		      (socket:socket-control sock :output-chunking-eof t)
		      (write-sequence *crlf* sock)))
      
      
      ; if we did post-headers then there's a string input
      ; stream to dump out.
      (if* content
	 then (write-sequence content sock)))))

      	
      
(defmethod compute-response-stream ((req http-request) (ent file-entity))
  ;; send directly to the socket since we already know the length
  ;;
  (setf (resp-stream req) (socket req)))

(defmethod compute-response-stream ((req http-request) (ent computed-entity))
  ;; may have to build a string-output-stream
  (if* (member :string-output-stream (resp-strategy req) :test #'eq)
     then (setf (resp-stream req) (make-string-output-stream))
     else (setf (resp-stream req) (socket req))))







  
  

	  
      



		    
		     
		     
		    
		  
    
