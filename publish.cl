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
	   :reader prefix))
  )


(defclass file-entity (entity)
    ;; a file to be published
    ((mime-type :initarg :mime-type :reader mime-type)
     (file  :initarg :file :reader file)
     (contents :initarg :contents :reader contents
	       :initform nil)
     (last-modified :initarg :last-modified
		    :reader last-modified
		    :initform 0)
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

	  
				



(defmethod process-entity ((req http-request-0.9) (entity computed-entity))
  ;; 
  (let ((fcn (entity-function entity)))
    (funcall fcn req entity)))


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

			
		    
		    
		     
		     
		    
		  
    
