(in-package :user)

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




;; macros to speed up some common character operations
(defmacro find-it (ch buff start end)
  ;; return position of ch in buff from [start end}
  ;;
  (let ((pos (gensym)))
    `(do ((,pos ,start (1+ ,pos)))
	 ((>= ,pos ,end))
       (if* (eq (schar ,buff ,pos) ,ch)
	  then (return ,pos)))))

(defmacro buffer-substr (buff start end)
  ;; return a string holding the chars in buff from [start end }
  ;;
  (let ((res (gensym))
	(i (gensym))
	(pos (gensym)))
    `(let ((,res (make-string (- ,end ,start))))
       (do ((,i 0 (1+ ,i))
	    (,pos ,start (1+ ,pos)))
	   ((>= ,pos ,end))
	 (setf (schar ,res ,i) (schar ,buff ,pos)))
       ,res)))

(defmacro buffer-match (buff start str)
  ;; return t if the buffer buff contains the same string as str
  (let ((pos (gensym))
	(i (gensym))
	(len (gensym)))
	   
    `(do ((,pos ,start (1+ ,pos))
	  (,i 0 (1+ ,i))
	  (,len (length ,str)))
	 ((>= ,i ,len) t)
       (if* (not (eq (schar ,buff ,pos) (schar ,str ,i)))
	  then (return nil)))))

(defmacro buffer-match-ci (buff start str)
  ;; return t if the buffer buff contains the same string as str
  ;; case insensitive version where str contains each char doubled
  (let ((pos (gensym))
	(i (gensym))
	(len (gensym))
	(xchar (gensym)))
	   
    `(do ((,pos ,start (1+ ,pos))
	  (,i 0 (+ 2 ,i))
	  (,len (length ,str)))
	 ((>= ,i ,len) t)
       (let ((,xchar (schar ,buff ,pos)))
	 (if* (not (or (eq ,xchar (schar ,str ,i))
		       (eq ,xchar (schar ,str (1+ ,i)))))
	    then (return nil))))))


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


(defvar *response-stream* ) ; bound to the stream to write html to.

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
					 :reuse-address t)))
    
    (unwind-protect
	(loop
	  (restart-case
	      (process-connection (socket:accept-connection main-socket))
	    (:loop ()  ; abort out of error without closing socket
	      nil)))
      (close main-socket))))





(defun process-connection (sock)
  (unwind-protect
      (let ((req))
	;; get first command
	(setq req (read-http-request sock))
	(if* (null req)
	   then ; failed command
		(with-http-response (*response-bad-request* req
							    "text/html")
		  (html "The request had a format " (:i "not understood")
			" by this server")
		  )
		; end this connection by closing socket
		(return-from process-connection nil)
	   else ;; got a request
		(handle-request req)
		#+ignore
		(with-http-response (*response-ok* req "text/html")
		  (html (:html
			 (:head (:title "my test page"))
			 (:body 
			  "Ok, " 
			  (:b "I've got it ") 
			  ((:table :border 5
				   :bgcolor 'red
				   :bordercolor 'green)
			   (:tr (:td "foo") 
				(:td "bar"))
			   (:tr (:td "aaa") 
				(:td "bbb")))
			  "now."
			  :br
			  ((:font :color 'blue)
			   (format *response-stream*
				   "Universal time is ~d~%"
				   (get-universal-time)))
			  ))))))
    (close sock)))


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
	    (socket:set-socket-format sock :text)  ; ensure in text mode
      
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
	      
	      (setq req (make-instance (http-request-class protocol)
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
	    
	    
	    (if* (null (read-request-headers req sock buffer))
	       then (return-from read-http-request nil))
	    
	    (if* (null (read-entity-body req sock))
	       then (return-from read-http-request nil)))
	  
	    
	  req  ; return req object
	  )
    
      ; cleanup forms
      (if* buffer then (free-request-buffer buffer)))))

(defun parse-url (url)
  ;; look for http://blah/........  and remove the http://blah  part
  ;; look for /...?a=b&c=d  and split out part after the ?
  ;;
  ;; return  values host, url, args
  (let ((urlstart 0)
	(host)
	(args))
    (multiple-value-bind (match whole hostx urlx)
	(match-regexp "^http://\\(.*\\)\\(/\\)" url :shortest t 
		      :return :index)
      (declare (ignore whole))
      (if* match
	 then ; start past the http thing
	      (setq host (buffer-substr url (car hostx) (cdr hostx)))
	      (setq urlstart (car urlx))))
    

    ; look for args
    (multiple-value-bind (match argsx)
	(match-regexp "?.*" url :start urlstart :return :index)
    
      (if* match
	 then (setq args (buffer-substr url (1+ (car argsx)) (cdr argsx))
		    url  (buffer-substr url urlstart (car argsx)))
	 else ; may still have a partial url
	      (if* (> urlstart 0)
		 then (setq url (buffer-substr url urlstart (length url))))
	      ))
  
    (values host url args)))
				      
    
		    
      
   
    
    
    


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
  

(defun parse-http-command (buffer end)
  ;; buffer is a string buffer, with 'end' bytes in it.  
  ;; return 3 values
  ;;	command  (kwd naming it or nil if bogus)
  ;;    url      string
  ;;    protocol  (kwd naming it or nil if bogus)
  ;;
  (let ((blankpos)
	(cmd)
	(urlstart))

    ; search for command first
    (dolist (possible *http-command-list* 
	      (return-from parse-http-command nil) ; failure
	      )
      (let ((str (car possible)))
	(if* (buffer-match buffer 0 str)
	   then ; got it
		(setq cmd (cdr possible))
		(setq urlstart (length (car possible)))
		(return))))
    
    
    (setq blankpos (find-it #\space buffer urlstart end))
    
    (if* (eq blankpos urlstart)
       then ; bogus, no url
	    (return-from parse-http-command nil))
    
    
    (if* (null blankpos)
       then ; must be http/0.9
	    (return-from parse-http-command (values cmd 
						    (buffer-substr buffer
								   urlstart
								   end)
						    :http/0.9)))
    
    (let ((url (buffer-substr buffer urlstart blankpos))
	  (prot))
      (if* (buffer-match buffer (1+ blankpos) "HTTP/1.")
	 then (if* (eq #\0 (schar buffer (+ 8 blankpos)))
		 then (setq prot :http/1.0)
	       elseif (eq #\1 (schar buffer (+ 8 blankpos)))
		 then (setq prot :http/1.1)))
      
      (values cmd url prot))))
    
    
    
(eval-when (compile load eval)
  (defun dual-caseify (str)
    ;; create a string with each characater doubled
    ;; but with upper case following the lower case
    (let ((newstr (make-string (* 2 (length str)))))
      (dotimes (i (length str))
	(setf (schar newstr (* 2 i)) (schar str i))
	(setf (schar newstr (1+ (* 2 i))) (char-upcase (schar str i))))
      newstr)))


(defparameter *header-to-slot*
    ;; headers that are stored in specific slots
    '((#.(dual-caseify "date:") . date)
      (#.(dual-caseify "user-agent:") . user-agent)
      (#.(dual-caseify "host:")  . host)
      (#.(dual-caseify "connection:") . connection)
      (#.(dual-caseify "transfer-encoding:") . transfer-encoding)
      (#.(dual-caseify "accept:") . accept)
      (#.(dual-caseify "content-length") . content-length)
      ))
      
(defmethod read-request-headers ((req http-request-1.0) sock buffer)
  ;; read in the headers following the command and put the
  ;; info in the req object
  ;; if an error occurs, then return nil
  ;;
  (let ((last-value-slot nil)
	(last-value-assoc nil)
	(end))
    (loop
      (multiple-value-setq (buffer end)(read-sock-line sock buffer 0))
      (if* (null end) 
	 then ; error
	      (return-from read-request-headers nil))
      (if* (eq 0 end)
	 then ; blank line, end of headers
	      (return t))
    
      (if* (eq #\space (schar buffer 0))
	 then ; continuation of previous line
	      (if* last-value-slot
		 then ; append to value in slot
		      (setf (slot-value req last-value-slot)
			(concatenate 
			    'string
			  (slot-value req last-value-slot)
			  (buffer-substr buffer 0 end)))
	       elseif last-value-assoc
		 then (setf (cdr last-value-assoc)
			(concatenate 'string
			  (cdr last-value-assoc) (buffer-substr buffer 0 end)))
		 else ; continuation with nothing to contine
		      (return-from read-request-headers nil))
	 else ; see if this is one of the special header lines
	    
	      (setq last-value-slot nil)
	      (dolist (possible *header-to-slot*)
		(if* (buffer-match-ci buffer 0 (car possible))
		   then ; store in the slot
			(setf (slot-value req (cdr possible))
			  (concatenate 
			      'string
			    (or (slot-value req (cdr possible)) "")
			    (buffer-substr buffer
					   (1+ (ash (the fixnum 
						      (length (car possible)))
						    -1))
					   end)))
					
			(setq last-value-slot (cdr possible))
			(return)))
	    
	      (if* (null last-value-slot)
		 then ; wasn't a built in header, so put it on
		      ; the alist
		      (let ((colonpos (find-it #\: buffer 0 end))
			    (key)
			    (value))
			  
			(if* (null colonpos)
			   then ; bogus!
				(return-from read-request-headers nil)
			   else (setq key (buffer-substr
					   buffer
					   0
					   colonpos)
				      value
				      (buffer-substr
				       buffer
				       (+ 2 colonpos)
				       end)))
			; downcase the key
			(dotimes (i (length key))
			  (let ((ch (schar key i)))
			    (if* (upper-case-p ch)
			       then (setf (schar key i) 
				      (char-downcase ch)))))
			
			; now add or append
			
			(let* ((alist (alist req))
			       (ent (assoc key alist :test #'equal)))
			  (if* (null ent)
			     then (push (setq ent (cons key "")) alist)
				  (setf (alist req) alist))
			  (setf (cdr ent)
			    (concatenate 'string
			      (cdr ent)
			      value))
			  
			  (setq last-value-assoc ent)
			  )))))))

(defmethod read-request-headers ((req http-request-0.9) sock buffer)
  ;; no headers for this kind of request
  (declare (ignore sock buffer))
  t)

			

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

  (flet ((cvt (str start-end)
	   (let ((res 0))
	     (do ((i (car start-end) (1+ i))
		  (end (cdr start-end)))
		 ((>= i end) res)
	       (setq res 
		 (+ (* 10 res)
		    (- (char-code (schar str i)) #.(char-code #\0))))))))
    
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
	 then (encode-universal-time
	       (cvt date second)
	       (cvt date minute)
	       (cvt date hour)
	       (cvt date day)
	       (compute-month date (car month))
	       (cvt date year)
	       0)
	      #+ignore (values (cvt date day)
		      (compute-month date (car month))
		      (cvt date year)
		      (cvt date hour)
		      (cvt date minute)
		      (cvt date second))))))
	  
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

	    
;------ urlencoding
; there are two similar yet distinct encodings for character strings
; that are referred to as "url encodings".
;
; 1. uri's.   rfc2396 describes the format of uri's 
;       uris use only the printing characters.
;	a url can be broken down into a set of a components using
;	a regular expression matcher.
;	There are a set of characters that are reserved:
;		; / ? : @ & = + $ ,
;	Certain reserved characters have special meanings within
;	certain components of the uri.
;	When a reserved character must be used for its normal character
;	meaning within a component, it is expressed in the form %xy 
;	where xy are hex digits representing the characters ascii value.
;
;       The encoding (converting characters to their $xy form) must be
;	done on a component by component basis for a uri.
;	You can't just give a function a complete uri and say "encode this"
;	because if it's a uri then it's already encoded.   You can
;	give a function a filename to be put into a uri and 
;	say "encode this" and that function
;	could look for reserved characters in the filename and convert them
;	to %xy form.
;
; 2. x-www-form-urlencoded
;	when the result of a form is to be sent to the web server
;	it can be sent in one of two ways:
;	1. the "get" method where the form data is passed in the uri
;	    after a "?".
;	2  the "post" method where the data is stored in the body
;	   of the post with an application/x-www-form-urlencoded  
;	   mime type.
;
;	the form data is sent in this format
;		name=value&name2=value2&name3=value3
;	where each of the name,value items is is encoded
;	such that
;	    alphanumerics are unchanged
;	    space turns into "+"
;	    newline turns into "%0d%0a"
;	    these characters are encoded as %xy:
;		+ # ; / ? : @ = & < > space %
;	    all non-printing ascii characters are encoded as %xy
;	    printing characters not mentioned are passed through
;	
;


(defun decode-form-urlencoded (str)
  ;; decode the x-www-form-urlencoded string returning a list
  ;; of conses, the car being the name and the cdr the value, for
  ;; each form element
  ;;
  (let (res (max (length str)))
    
    (do ((i 0)
	 (start 0)
	 (name)
	 (max-minus-1 (1- max))
	 (seenpct)
	 (ch))
	((>= i max))
      (setq ch (schar str i))
      
      (let (obj)
	(if* (or (eq ch #\=)
		 (eq ch #\&))
	   then (setq obj (buffer-substr str start i))
		(setq start (1+ i))
	 elseif (eql i max-minus-1)
	   then (setq obj (buffer-substr str start (1+ i)))
	 elseif (and (not seenpct) (or (eq ch #\%)
				       (eq ch #\+)))
	   then (setq seenpct t))
      
	(if* obj
	   then (if* seenpct
		   then (setq obj (un-hex-escape obj)
			      seenpct nil))
	      
		(if* name
		   then (push (cons name obj) res)
			(setq name nil)
		   else (setq name obj))))
      
      (incf i))
    
    res))

(defun un-hex-escape (given)
  ;; convert a string with %xx hex escapes into a string without
  ;; also convert +'s to spaces
  (let ((count 0)
	(len (length given)))
    
    ; compute the number of %'s (times 2)
    (do ((i 0 (1+ i)))
	((>= i len))
      (if* (eq (schar given i) #\%) 
	 then (incf count 2)
	      (incf i 2)))

    (macrolet ((cvt-ch (ch)
		 ;; convert hex character to numeric equiv
		 `(let ((mych (char-code ,ch)))
		    (if* (<= mych #.(char-code #\9))
		       then (- mych #.(char-code #\0))
		       else (+ 9 (logand mych 7))))))
			    
      (let ((str (make-string (- len count))))
	(do ((to 0 (1+ to))
	     (from 0 (1+ from)))
	    ((>= from len))
	  (let ((ch (schar given from)))
	    (if* (eq ch #\%)
	       then (setf (schar str to)
		      (code-char (+ (ash (cvt-ch (schar given (1+ from))) 4)
				    (cvt-ch (schar given (+ 2 from))))))
		    (incf from 2)
	     elseif (eq ch #\+)
	       then (setf (schar str to) #\space)
	       else (setf (schar str to) ch))))
      
	str))))
   
    
    


      



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
