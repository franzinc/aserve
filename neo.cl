(defpackage :neo
  (:use :common-lisp :excl :htmlgen)
  (:export
   #:decode-form-urlencoded
   #:publish
   #:publish-file
   #:with-http-response
   #:*response-ok*
	  )
  )

(in-package :neo)


(defvar *ndebug* t)   ; print debugging stuff



(defclass http-header-mixin ()
  ;; List of all the important headers we can see in any of the protocols.
  ;; 
  ((connection :accessor connection :initform nil)
   (date :accessor date :initform nil)
   (transfer-encoding :accessor transfer-encoding :initform nil)
   (accept :accessor accept :initform nil)
   (host :accessor host :initform nil :initarg :host)
   (user-agent :accessor user-agent :initform nil)
   (content-length :accessor content-length :initform nil)))
   
	   
	   
   
  
(defclass http-request-0.9 (http-header-mixin)
  ;; http protocol 0.9 doesn't support mime headers for requests
  ((command  ;; keyword giving the command in this request
    :initarg :command
    :reader command)
   (url ;; string - the argument to the command
    :initarg :url
    :reader url)
   (args  ;; alist of arguments if there were any after a ? on the url
    :initarg :args
    :reader args)
   (protocol ;; symbol naming the http protocol 
    :initarg :protocol
    :reader protocol)
   (protocol-string ;; string naming the protcol
    :initarg :protocol-string
    :reader protocol-string)
   (body  ;; string buffer holding the data for this command
    :initarg :body
    :accessor body)
   (alist ;; alist of headers not stored in slots
    :initform nil
    :accessor alist)
   (client-ipaddr  ;; who is connecting to us
    :initarg :client-ipaddr)
   
   (response ;; stream to which to send the response
    :accessor response)
   
   (finished ;; true when this request has been handled
    :initform nil	
    :accessor finished)
   
   (socket ;; the socket we're communicating throgh
    :initarg :socket
    :reader socket)
   
   (mime-type ;; the mime type of the response to this request
    :accessor mime-type)
   
   ))


(defclass http-request-1.0 (http-request-0.9)
  ;; http 1.0 uses mime headers in a request to set parameters
  ;; there are some ad hoc persistent connection things
  ()
  )

(defclass http-request-1.1 (http-request-1.0)
  ;; http 1.1 uses chunked transfers for blocks of data where
  ;; you don't want to specify the size in advance
  ()
  )


(defclass xhttp-request (http-header-mixin)
  ;; incoming
  ((command  ;; keyword giving the command in this request
    :initarg :command
    :reader command)
   (url ;; string - the argument to the command
    :initarg :url
    :reader url)
   (args  ;; alist of arguments if there were any after a ? on the url
    :initarg :args
    :reader args)
   (protocol ;; symbol naming the http protocol 
    :initarg :protocol
    :reader protocol)
   (protocol-string ;; string naming the protcol
    :initarg :protocol-string
    :reader protocol-string)
   (alist ;; alist of headers not stored in slots
    :initform nil
    :accessor alist)
   (socket ;; the socket we're communicating throgh
    :initarg :socket
    :reader socket)
   (client-ipaddr  ;; who is connecting to us
    :initarg :client-ipaddr)

   ;; response
   (resp-code   ;; one of the *response-xx* objects
    :initform nil
    :accessor resp-code)
   (resp-date
    :initform (get-universal-time)  ; when we're responding
    :reader resp-date)
   (resp-headers  ;; alist of headers to send out
    :initform nil
    :accessor resp-headers)
   (resp-content-type ;; mime type of the response
    :initform nil
    :accessor resp-content-type)
   (resp-stream   ;; stream to which to send response
    :initform nil
    :accessor resp-stream)
   (resp-keep-alive   ;; true if we are not shutting down the connection
    :initform nil
    :accessor resp-keep-alive)
   (resp-transfer-encoding ;; encoding for sending the body
    :initform :identity
    :accessor resp-transfer-encoding)
   )
  
  
		
  )


(defstruct (response (:constructor make-resp (number desc)))
  number
  desc)

(defparameter *response-ok* (make-resp 200 "OK"))
(defparameter *response-created* (make-resp 201 "Created"))
(defparameter *response-accepted* (make-resp 202 "Accepted"))

(defparameter *response-bad-request* (make-resp 400 "Bad Request"))
(defparameter *response-unauthorized* (make-resp 401 "Unauthorized"))
(defparameter *response-not-found* (make-resp 404 "Not Found"))

(defparameter *response-internal-server-error*
    (make-resp 500 "Internal Server Error"))
(defparameter *response-not-implemented* (make-resp 501 "Not Implemented"))

(defvar *crlf* (make-array 2 :element-type 'character :initial-contents
			   '(#\return #\linefeed)))

(defvar *read-request-timeout* 20)


(defmacro with-http-response ((kind request mime-type &rest args) &rest body)
  ;; write a response back to the server
  ;; kind is a http response value
  ;; request is the request object to which we're replying
  (declare (ignore args))
  
  (let ((str (gensym))
	(req (gensym)))
    `(let* ((,req ,request)
	    (,str (make-response-stream ,req))
	    (*response-stream* ,str)
	    )
       (setf (response ,req) ,str)
       (setf (mime-type ,req) ,mime-type)
       (format (socket ,req) "~a ~d ~a~a"
	       (protocol-string ,req)
	       (response-number ,kind)
	       (response-desc   ,kind)
	       *crlf*)
       ,@body
       
       (post-process-request ,req)
       )))
       

(defmacro with-fixed-response ((kind request mime-type
				&key content-length
				     last-modified) &rest body)
  ;; used respond to an http request where the object to
  ;; send back is a fixed size object
  (let ((req (gensym))
	(clen (gensym)))
    `(let* ((,req ,request)
	    (*response-stream* (socket ,req))
	    (,clen ,content-length))
       
       (setf (response ,req) (socket ,req))
       (setf (mime-type ,req) ,mime-type)
       (socket:set-socket-format *response-stream* :text)
       (format (socket ,req) "~a ~d ~a~a"
	       (protocol-string ,req)
	       (response-number ,kind)
	       (response-desc   ,kind)
	       *crlf*)
       (format *response-stream* "Content-Type: ~a~a"
	       ,mime-type *crlf*)
       
       (if* ,clen
	  then (format *response-stream* "Content-Length: ~d~a"
		       ,clen *crlf*))
       
       ,(if* last-modified
	   then `(format *response-stream* "Last-Modified: ~a~a"
			 (universal-time-to-date ,last-modified)
			 *crlf*))
       
       (format *response-stream* "~a" *crlf*)
       
       (format t "command is ~s~%" (command ,req))
       
       (if* (eq (command ,req) :get)
	  then ,@body)
       
       )))
				    
			      
(defun start (&key (port 80))
  ;; start the web server
  
  (let ((main-socket (socket:make-socket :connect :passive
					 :local-port port
					 :reuse-address t
					 :format :bivalent)))
    
    (unwind-protect
	(loop
	  (restart-case
	      (process-connection (socket:accept-connection main-socket))
	    (:loop ()  ; abort out of error without closing socket
	      nil)))
      (close main-socket))))


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
      (handler-case (start :port port)
	(error (cond)
	  (format t " got error ~s~%" cond)
	  (format t "restarting~%"))))))


(defun process-connection (sock)
  (unwind-protect
      (let ((req))
	;; get first command
	(tagbody  again
	  (mp:with-timeout (*read-request-timeout* 
			    (log-timed-out-request-read sock)
			    (return-from process-connection nil))
	    (setq req (read-http-request sock)))
	  (if* (null req)
	     then ; failed command
		  (logmess
		   (format nil "non http request from ~a"
			   (socket::ipaddr-to-dotted (socket::remote-host sock))))
		  ; end this connection by closing socket
		  (return-from process-connection nil)
	     else ;; got a request
		  (logmess "got valid request")
		  (handle-request req)
		  (let ((sock (socket req)))
		    (if* (resp-keep-alive req)
		       then ; continue to use it
			    (force-output sock)
			    (go again))))))
    (ignore-errors (close sock))))


(defun read-http-request (sock)
  ;; read the request from the socket and return and http-request
  ;; object
  
  (let ((buffer (get-request-buffer))
	(req)
	(end))
    
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
      
	    (if* *ndebug* 
	       then (format *debug-io* "got line of size ~d: " end)
		    (dotimes (i end) (write-char (schar buffer i) *debug-io*))
		    (terpri *debug-io*) (force-output *debug-io*)
		    )
      
	    (if* (not (eql 0 end))
	       then (return) ; out of loop
		    ))
	  
	  (multiple-value-bind (cmd url protocol)
	      (parse-http-command buffer end)
	    (if* (or (null cmd) (null protocol))
	       then ; no valid command found
		    (return-from read-http-request nil))
	    (multiple-value-bind (host newurl args)
		(parse-url url)
	      
	      (setq req (make-instance 'xhttp-request
			  :command cmd
			  :url newurl
			  :args args
			  :host host
			  :protocol protocol
			  :protocol-string (case protocol
					     (:http/1.0 "HTTP/1.0")
					     (:http/1.1 "HTTP/1.1")
					     (:http/0.9 "HTTP/0.9"))
			  :socket sock
			  :client-ipaddr (socket:remote-host sock))))
	    
	    
	    (if* (and (not (eq protocol :http/0.9))
		      (null (read-request-headers req sock buffer)))
	       then (return-from read-http-request nil))
	    
	    ;; let the handler do this
	    #+ignore
	    (if* (null (read-entity-body req sock))
	       then (return-from read-http-request nil)))
	  
	    
	  req  ; return req object
	  )
    
      ; cleanup forms
      (if* buffer then (free-request-buffer buffer)))))


				      
    
		    
      
   
    
    
    


;; determine the class of the request object for a give protocol
(defmethod http-request-class ((protocol (eql :http/0.9)))
  'http-request-0.9)

(defmethod http-request-class ((protocol (eql :http/1.0)))
  'http-request-1.0)

(defmethod http-request-class ((protocol (eql :http/1.1)))
  'http-request-1.1)


(defvar *http-command-list*
    '(("GET " . :get)
      ("HEAD " . :head)
      ("POST " . :post)
      ("PUT "  . :put)
      ("OPTIONS " . :options)
      ("DELETE " .  :delete)
      ("TRACE "  .  :trace)
      ("CONNECT " . :connect)))
  


    
    
    

(defmethod read-entity-body ((req http-request-1.1) sock)
  ;; http/1.1 offers up the chunked transfer encoding
  ;; we can't handle that at present...
  (declare (ignore sock)) ; but they are passed to the next method
  (if* (equalp "chunked" (transfer-encoding req))
     then (with-http-response (*response-not-implemented*
			       req "text/html" :close t)
	    (format (response req) "chunked transfer is not supported yet"))
	  t
     else ; do the http/1.0 thing
	  (call-next-method)))


  
	  
	  
	  
			     
(defmethod read-entity-body ((req http-request-1.0) sock)
  ;; Read the message following the request header, if any.
  ;; Here's my heuristic
  ;;   if Content-Length is given then it specifies the length.
  ;;   if there is no Content-Length but there is a Connection: Keep-Alive
  ;;      then we assume there is no entity body
  ;;  
  ;;   if there is no Content-Length and no Connection: Keep-Alive
  ;;      then we read what follows and assumes that the body
  ;;	  (this is the http/0.9 behavior).
  ;;
  (let ((cl (content-length req)))
    (if* cl
       then (let ((len (content-length-value req))
		  (ret))
	      (setq ret (make-string len))
	      (let ((got-len (read-sequence ret sock)))
		(if* (not (eql got-len len))
		   then ; failed to get all the data
			nil
		   else (setf (body req) ret)
			t)))
       else ; no content length
	    (if* (not (equalp (connection req) "keep-alive"))
	       then (call-next-method) ; do the 0.9 thing
	       else ; there is no body
		    t))))

(defmethod read-entity-body ((req http-request-0.9) sock)
  ;; read up to end of file, that will be the body
  (let ((ans (make-array 2048 :element-type 'character
			 :fill-pointer 0))
	(ch))
    (loop (if* (eq :eof (setq ch (read-char sock nil :eof)))
	     then (setf (body req) ans)
		  (return t)
	     else (vector-push-extend ans ch)))))

	    


      

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
	   then (free-request-buffer buffer)
		(return-from read-sock-line nil))
      
	(if* (eq ch #\linefeed)
	   then (if* (eq prevch #\return)
		   then (decf start) ; back up to toss out return
			)
		(setf (schar buffer start) #\null) ; null terminate
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
      
		      
				  
				
				
(defmethod make-response-stream ((req http-request-0.9))
  ;; use the actual stream
  (socket req))

(defmethod make-response-stream ((req http-request-1.0))
  ;; build a string output stream so we can
  ;; determine the response size
  (make-string-output-stream))

;; for http/1.1 we'll want to do chunking here


(defmethod post-process-request ((req http-request-0.9))
  nil)

(defmethod post-process-request ((req http-request-1.0))
  ;; put out headers and then the body that's been generated.
  (let ((ans (get-output-stream-string (response req))))
    (format (socket req) "Content-Type: ~a~a"
	    (mime-type req)
	    *crlf*)
    (format (socket req) "Content-Length: ~d~a" (length ans) *crlf*)
    (write-string *crlf* (socket req))
    (write-sequence ans (socket req))))
  
  




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
    
     
     
(defun universal-time-to-date (ut)
  ;; convert a lisp universal time to rfc 1123 date
  ;;
  (multiple-value-bind
      (sec min hour date month year day-of-week dsp time-zone)
      (decode-universal-time ut 0)
    (declare (ignore time-zone dsp))
    (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
	    (svref
	     '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	     day-of-week)
	    date
	    (svref
	     '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		"Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
	     month
	     )
	    year
	    hour
	    min
	    sec)))

	    


;; ----- scratch buffer resource:
 
(defvar *rq-buffers* nil)
(defparameter *max-buffer-size* #.(* 5 1024)) ; no line should be this long
    
(defun get-request-buffer (&optional size)
  ;; get a string buffer.
  ;; if size is important, it is specified.
  ;; if the size is too great, then refuse to create one and return
  ;; nil
  (mp:without-scheduling 
    (if* size
       then ; must get one of at least a certain size
	    (if* (> size *max-buffer-size*)
	       then (return-from get-request-buffer nil))
	    
	    (dolist (buf *rq-buffers*)
	      (if* (>= (length buf) size)
		 then (setq *rq-buffers* (delete buf *rq-buffers* :test #'eq))
		      (return-from get-request-buffer  buf)))
	    
	    ; none big enough
	    (make-array size :element-type 'character)
       else ; just get any buffer
	    (if* (pop *rq-buffers*)
	       thenret
	       else (make-array 2048 :element-type 'character)))))


(defun free-request-buffer (buffer)
  ;; return buffer to the free pool
  (mp:without-scheduling (push buffer *rq-buffers*)))


	    
;;-----------------
