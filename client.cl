;; -*- mode: common-lisp; package: net.iserve.client -*-
;;
;; client.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation; 
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
;; $Id: client.cl,v 1.1.2.2 2000/03/16 17:17:17 jkf Exp $

;; Description:
;;   http client code.

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


;; this will evolve into the http client code but for now it's
;; just some simple stuff to allow us to test iserve
;;



(defpackage :net.iserve.client 
  (:use :net.iserve :excl :common-lisp)
  (:export 
   #:send-request
   #:post-request
   #:parse-response
   ))

(in-package :net.iserve.client)



(defmacro with-scan-macros (&rest body)
  ;; define the macros for scanning characters in a string
  `(macrolet ((collect-to (ch &optional downcasep)
		;; return a string containing up to the given char
		`(let ((start i))
		   (loop
		     (if* (>= i max) then (fail))
		     (if* (eql ,ch (schar buffer i)) 
			then (return (buf-substr start i ,downcasep)))
		     (incf i)
		     )))
	      (collect-to-eol ()
		;; return a string containing up to the given char
		`(let ((start i))
		   (loop
		     (if* (>= i max) then (return (buf-substr start i)))
		     (let ((thisch (schar buffer i)))
		       (if* (eq thisch #\return)
			  then (let ((ans (buf-substr start i)))
				 (incf i)  ; skip to newline
				 (return ans))
			elseif (eq thisch #\newline)
			  then (return (buf-substr start i))))
		     (incf i)
		     )))
	      (fail ()
		`(return-from parse-response nil))
	      (skip-to-not (ch)
		;; skip to first char not ch
		`(loop
		   (if* (>= i max) then (fail))
		   (if* (not (eq ,ch (schar buffer i)))
		      then (return))
		   (incf i)))
	      (buf-substr (from to &optional downcasep)
		;; create a string containing [from to }
		;;
		`(let ((res (make-string (- ,to ,from))))
		   (do ((ii ,from (1+ ii))
			(ind 0 (1+ ind)))
		       ((>= ii ,to))
		     (setf (schar res ind)
		       ,(if* downcasep
			   then `(char-downcase (schar buffer ii))
			   else `(schar buffer ii))))
		   res)))
     ,@body))

(defparameter *debug* nil)

(defmacro dformat (&rest args)
  `(progn (if* *debug*
	     then (format t ,@(cdr args)))
	  (format ,@args)))

(defmacro dwrite-sequence (seq stream)
  `(progn (if* *debug*
	     then (write-sequence ,seq *standard-output*))
	  (write-sequence ,seq ,stream)))
		  




(defvar crlf (make-array 2 :element-type 'character
			 :initial-contents '(#\return #\newline)))

(defun send-request (stream
		     kind ;; "GET" "POST" "HEAD"  or :get :post :head
		     url  ;; string that's the url
		     &key (protocol  "HTTP/1.0")
			  (accept "*/*")
			  (close t)
			  content-type
			  body
			  host
			  authorization
			  )
  
  (dformat stream "~a ~a ~a~aAccept: ~a~a"
	  (if* (stringp kind)
	     then kind
	     else (ecase kind
		    (:get "GET")
		    (:post "POST")
		    (:head "HEAD")))
	  url protocol crlf
	  accept crlf)
  (if* content-type
     then (dformat stream "Content-type: ~a~a" content-type crlf))
  
  (if* authorization
     then (dformat stream "Authorization: Basic ~a~a"
		   authorization crlf))
  
  (send-cookies stream url (or host
			       (socket:ipaddr-to-hostname 
				(socket:remote-host stream))))
  (if* body
     then (dformat stream "Content-length: ~d~a"
		   (length body)
		   crlf))
  (if* host
     then (dformat stream "Host: ~a~a" host crlf))
  

  (dwrite-sequence crlf stream)
  (if* body then (dwrite-sequence body stream))
  (force-output stream)
  (if* close then (socket:shutdown stream :direction :output))
  )


(defun post-request (stream url values &key host)
  ;; do a post to the given url of the given values which
  ;; is a list of  (propname . value)  all strings
  (let (res)
    (dolist (val values)
      (if* res
	 then (push "&" res))
      
      (push (urlencode (cdr val)) res)
      (push "=" res)
      (push (car val) res))
    
    (send-request stream
		  :post
		  url
		  :host host
		  :content-type "application/x-www-form-urlencoded"
		  :body (apply #'concatenate 'string res))))


		     
(defun get-response (stream &key string)
  ;; get the http response from the stream and return 
  ;; either the buffer and character count (if string is nil)
  ;; or a newly created string holding the whole response (if string is t)
  ;;
  (let* ((buff (get-response-buffer))
	 (max 0))
    (multiple-value-setq (buff max)
      (read-response stream buff))
    
    
    (if* string
       then ; create  a string for the response
	    (let ((str (make-string max)))
	      (dotimes (i max)
		(setf (schar str i) (schar buff i)))
	      (return-response-buffer buff)
	      str)
       else (values buff max))))

   
(defun simple-get (host url &key authorization)
  (let ((sock (socket:make-socket :remote-host host
				  :remote-port 80)))
    (unwind-protect
	(progn (send-request sock :get url :host host 
			     :authorization authorization)
	       (get-response sock :string t))
      (close sock))))

(defun simple-post (host url values)
  (let ((sock (socket:make-socket :remote-host host
				  :remote-port 80)))
    (unwind-protect
	(progn (post-request sock url values :host host)
	       (get-response sock :string t))
      (close sock))))

(defvar .bufs. (list (make-string 50000)))

(defun get-response-buffer ()
  ;; return the first (largest) response buffer
  (let (buff)
    (mp:without-scheduling
      (if* (setq buff (pop .bufs.))
	 then buff
	 else (setq buff (make-string 5000))))))

(defun return-response-buffer (buff)
  ;; return the given buffer to the list
  ;; keep the list sorted from biggest to smallest
  (mp:without-scheduling
    (if* (null .bufs.)
       then (push buff .bufs.)
       else (let ((size (length buff)))
	      (do ((prev nil cur)
		   (cur  .bufs. (cdr cur)))
		  ((null cur)
		   (setf (cdr prev) (list buff)))
		(if* (>= size (length (car cur)))
		   then ; insert between prev and cur
			(if* (null prev)
			   then ; first one
				(push buff .bufs.)
			   else (setf (cdr prev)
				  (cons buff cur)))
			(return)))))))


(defun read-response (stream buffer)
  ;; read into the buffer starting at max
  ;; get a bigger buffer if necessary
  ;; return the final buffer and the number of characters read
  ;; into the buffer.
  
  (let ((size (length buffer))
	(index 0))
    
    (loop
      (let ((count (read-sequence buffer stream :start index)))
	(if* (eql count index)
	   then ; end of file
		(return (values buffer count))
	 elseif (eql count size)
	   then ; buffer exahusted
		(let ((newbuf (get-response-buffer)))
		  (if* (<= (length newbuf) size)
		     then ; put it back
			  (return-response-buffer newbuf)
			  (setq newbuf (make-string (+ size 10000))))
		  (dotimes (i count)
		    (setf (schar newbuf i) (schar buffer i)))

		  (return-response-buffer buffer)
		  (setq buffer newbuf
			size   (length buffer)
			index  count))
	   else ; read partial buffer
		(setq index count))))))


(defstruct httpr 
  protocol
  response 
  comment
  headers  ; list (header . value)
  body
  )

(defun parse-response (buffer &optional (max (length buffer)))
  ;; scan for headers, returning a http-response structure
  (let ((httpr (make-httpr))
	(i 0))
    
    (with-scan-macros
	(macrolet ((fail ()
		     `(return-from parse-response nil)))
      
	  (setf (httpr-protocol httpr) (collect-to #\space))
	  (incf i)
	  (skip-to-not #\space)
	  (setf (httpr-response httpr) (read-from-string (collect-to #\space)))
	  (incf i)
	  (setf (httpr-comment httpr) (collect-to-eol))
      
	  ; now read the header lines

	  (let ((headers))
	    (loop
	      (incf i)  ; past eol on first line
	      (if* (>= i max) then (return)) ; nothing more left
	      (case (schar buffer i)
		((#\return #\newline)
		  ; blank line, end of headers
		  (setf (httpr-body httpr)
		    (subseq buffer i max))
		  (return))
		((#\space #\tab)
		 ; continuation line of previous header
		 (let ((continue (collect-to-eol)))
		   (setf (cdr (car headers))
		     (concatenate 'string (cdr (car headers))
				  continue))))
		(t (push (cons (collect-to #\: :downcase)
			       (progn (incf i)
				      (skip-to-not #\space)
				      (collect-to-eol)))
			 headers))))
	    (setf (httpr-headers httpr) headers))
		   
	  
	  httpr))))

      
	       
		    
(defun header-value (header httpr &optional (errorp t))
  ;; get the header value and signal an error if header not found 
  ;; unless errorp is nil
  (let ((ent (assoc header (httpr-headers httpr) :test #'equal)))
    (if* (null ent)
       then (if* errorp
	       then (error "header ~s not found in ~s" header httpr)
	       else nil)
       else (cdr ent))))

(defun header-values (header httpr)
  ;; returen a list of all the values for the given name, this is
  ;; used when the header name may be present more than
  ;; once (like in Set-Cookie)
  ;;
  (let (res)
    (dolist (headent (httpr-headers httpr))
      (if* (equal header (car headent))
	 then (push (cdr headent) res)))
    res))
  

(defun split-url (url)
  ;; return values
  ;;  host  url  
  ;;
  (multiple-value-bind (match whole hostx urlx)
      (match-regexp "^http://\\(.*\\)\\(/.*\\)$" url :shortest t)
    (declare (ignore whole))
    (if* match
       then ; start past the http thing
	    (return-from split-url
	      (values hostx urlx))
       else ; all is after host part
	    (values nil url))))
				      
  
		    
(defun hexout (int target)
  (vector-push-extend #\% target)
  (let ((hival (logand #xf (ash int -4)))
	(loval (logand #xf int)))
    (vector-push-extend
     (code-char (+ (if* (> hival 9)
		     then (decf hival 10)
			  #.(char-int #\A)
		     else #.(char-int #\0))
		  hival))
     target)
    (vector-push-extend 
     (code-char (+ (if* (> loval 9)
		     then (decf loval 10)
			  #.(char-int #\A)
		     else #.(char-int #\0))
		  loval)) 
     target)))

(defun needs-hex-escape (ch)
  (member ch '(#\+ #\#	#\; #\/	#\? #\:	#\@ #\=	#\&
	       #\< #\>	#\space	#\%
	       )
	  :test #'eq))

(defun urlencode (string)
  ;;
  ;; convert string to the x-www-form-urlencode form
  ;; returns an adjustable fill pointer string
  ;;
  (let ((target (make-array (+ 10 (length string) )
			    :adjustable t
			    :fill-pointer 0
			    :element-type 'character
			    )))
    (dotimes (i (length string))
      (let ((ch (aref string i)))
	(if* (eq ch #\space)
	   then (vector-push-extend #\+ target)
	 elseif (eq ch #\newline)
	   then (hexout #.(char-int #\return) target)
		(hexout #.(char-int #\linefeed) target)
	 elseif (needs-hex-escape ch)
	   then (hexout (char-int ch) target)
	   else (vector-push-extend ch target))))
    target))

  
(defun parse-multi-value (str)
  ;; parse a header multiple value which looks like  foo=bar; baz=bof
  ;; and return (("foo" . "bar") ("baz" . "bof"))
  ;;
  (let (res)
    (loop
      (multiple-value-bind (match whole key value rest)
	  (match-regexp "[; ]*\\([^ ;=]+\\)=\\([^;]+\\)\\(;.*\\| *\\)$" str
			:shortest t)
	(declare (ignore whole))
	(if* match
	   then (push (cons key value) res)
		(setq str rest)
	   else (return res))))))
    

;; we keep a cookie list to simulate a cookie aware browser
;; form of *cookies* is a list of
;; (hostname (path (var value) ...) ...)
;; 
(defvar *cookies* nil)

(defun save-cookie (host httpr)
  (flet ((all-but-path (vals)
	   ;; return a list of all in the assoc list except the one
	   ;; with the key path , also domain
	   (let (res)
	     (dolist (val vals)
	       (if* (or (equal "path" (car val)) 
			(equal "domain" (car val)))
		  thenret
		  else (push val res)))
	     res)))
	   
    (dolist (val (header-values "set-cookie" httpr))
	  
      (let ((vals)
	    (ent))
	(if* val
	   then (setq vals (parse-multi-value val))
	      
		(if* (setq ent (assoc "domain" vals :test #'equal))
		   then (setq host (list (cdr ent)))) ; indicate a group
		      
		(let ((ent (assoc host *cookies* :test #'equal)))
		  (if* (null ent)
		     then (push (setq ent (list host)) *cookies*))
	      
		  (format t "got cookies ~s for host ~s~%"
			  vals host)
	      
		  ; find applicable path
		  (let ((pathent (assoc "path" vals :test #'equal)))
		    (if* (null pathent)
		       then (warn "no path for cookie")
		       else ;; note: it seems ok to have duplicates
			    ;; for a given path 
			    (push (cons (cdr pathent)
					(all-but-path vals))
				  (cdr ent))))))))))

(defun send-cookies (stream url host)
  ;; send out appropriate cookies for this host and url
  (let ((cookievalue (compute-cookies url host)))
    (if* cookievalue
       then (dformat stream "Cookie: ~a~a" cookievalue crlf))))

(defun compute-cookies (url host)
  (let ((vals (find-applicable-cookies host))
	(cookies))
    (if* vals
       then (dolist (pathvals (cdr vals))
	      (let ((path (car pathvals)))
		(if* (<= (length path) (length url))
		   then ;  is path the initial part of url
			(if* (dotimes (i (length path) t)
			       (if* (not (eq (schar path i) (schar url i)))
				  then (return nil)))
			   then (setq cookies (append (cdr pathvals)
						      cookies)))))))
    (if* cookies
       then ; build a big string holding cookie values
	    (let (res)
	      (dolist (cookie cookies)
		(if* res
		   then (push "; " res))
		
		(push (cdr cookie) res)
		(push "=" res)
		(push (car cookie) res))
	      
	      (apply #'concatenate 'string res)))))

(defun find-applicable-cookies (host)
  ;; look for those cookies that match this host
  (let (res)
    (dolist (cookie *cookies*)
      (let ((chost (car cookie)))
	(if* (stringp chost)
	   then (if* (equal chost host)
		   then (setq res (append res (cdr cookie))))
	 elseif (consp chost)
	   then (setq chost (car chost))
		; test against the tail
		(if* (>= (length host) (length chost))
		   then (do ((ci (1- (length chost)) (1- ci))
			     (hi (1- (length host)) (1- hi)))
			    ((< ci 0) 
			     (setq res (append res (cdr cookie)))
			     (return))
			  (if* (not (eql (schar host hi)
					 (schar chost ci)))
			     then (return nil)))))))
    (if* res 
       then ; make it look like  cookie
	    (cons nil res))))


      
;-----------------------------------------------------------------
;
; desired client interface
;
; (make-http-client-request uri &key protocol keep-alive chunking
;				    cookies basic-authorization
;				    content-length content)
;
;   
;    uri is string or uri object.
;    it must be an http uri
;    open a connection to the given port on the given host and 
;	send the given request
;
;    protocol  - what to put in the request
;	either
;		:http/1.0
;		:http/1.1
;    keep-alive - if true, request that the connection remain open
;		after this request.
;    chunking - if true, say that we'll accept a chunked response
;		(should only do this for http/1.1 requests).
;
;    cookies - list of cookies, from which applicable ones to this
;	       request will be extracted and sent along
;    basic-authorization - if non nil then it should be a cons 
;		of the form ("name" . "password") which we'll send along
;    content-length - the length of the content we will send after 
;		the headers
;    content - the actual content to send, in which case the content-length
;		should not be specified as we'll figure it out.
;
;  This returns a client-request object containing the stream, after
;  the request line and all headers and the crlf after the headers 
;  have been sent.  If the content is given then it will be sent too.
;
;  
; (read-client-response-headers  client-request)
;
;   read the response to the client request and the headers for it, parsing
;  the headers and storing them in the client-request object.
;
; (read-client-request-sequence client-request buffer start end)
;   read the next (end-start) bytes from the body of client request, handling
;   turning on chunking if needed
;   return index after last byte read.
;   return 0 if eof
;
;


(defclass client-request ()
  ((headers ; alist of  ("headername" . "value")
    :initform nil
    :initarg :headers
    :accessor client-request-headers)
   (response-code    ; response code (an integer)
    :initform nil
    :accessor client-request-response-code)
   (socket  ; the socket through which we'll talk to the server
    :initarg :socket
    :accessor client-request-socket)
   (protocol 
    ; the protocol value returned by the web server
    ; note, even if the request is for http/1.0, apache will return
    ; http/1.1.  I'm not sure this is kosher.
    :accessor client-request-protocol)
   (response-comment  ;; comment passed back with the response
    :accessor client-request-response-comment)
   ))


(defmacro with-better-scan-macros (&rest body)
  ;; define the macros for scanning characters in a string
  `(macrolet ((collect-to (ch buffer i max &optional downcasep)
		;; return a string containing up to the given char
		`(let ((start ,i))
		   (loop
		     (if* (>= ,i ,max) then (fail))
		     (if* (eql ,ch (schar ,buffer ,i)) 
			then (return (buf-substr start i ,buffer ,downcasep)))
		     (incf ,i)
		     )))
	      
	      (collect-to-eol (buffer i max)
		;; return a string containing up to the given char
		`(let ((start ,i))
		   (loop
		     (if* (>= ,i ,max) 
			then (return (buf-substr start ,i ,buffer)))
		     (let ((thisch (schar ,buffer ,i)))
		       (if* (eq thisch #\return)
			  then (let ((ans (buf-substr start ,i ,buffer)))
				 (incf ,i)  ; skip to newline
				 (return ans))
			elseif (eq thisch #\newline)
			  then (return (buf-substr start ,i ,buffer))))
		     (incf ,i)
		     )))
	      
	      (skip-to-not (ch buffer i max)
		;; skip to first char not ch
		`(loop
		   (if* (>= ,i ,max) then (fail))
		   (if* (not (eq ,ch (schar buffer ,i)))
		      then (return))
		   (incf ,i)))
	      
	      (buf-substr (from to buffer &optional downcasep)
		;; create a string containing [from to }
		;;
		`(let ((res (make-string (- ,to ,from))))
		   (do ((ii ,from (1+ ii))
			(ind 0 (1+ ind)))
		       ((>= ii ,to))
		     (setf (schar res ind)
		       ,(if* downcasep
			   then `(char-downcase (schar ,buffer ii))
			   else `(schar ,buffer ii))))
		   res)))
     
     ,@body))


(defun make-http-client-request (uri &key 
				     (method  :get)  ; :get, :post, ....
				     (protocol  :http/1.0)
				     keep-alive 
				     (accept "*/*") 
				     cookies 
				     basic-authorization
				     content-length 
				     content)
  ;; start a request 
  
  ; parse the uri we're accessing
  (if* (not (typep uri 'net.uri:uri))
     then (setq uri (net.uri:parse-uri uri)))
  
  ; make sure it's an http uri
  (if* (not (eq :http (or (net.uri:uri-scheme uri) :http)))
     then (error "Can only do client access of http uri's, not ~s" uri))
  
  ; make sure that there's a host
  (let ((host (net.uri:uri-host uri))
	(sock))
    (if* (null host)
       then (error "need a host in the client request: ~s" uri))
    
    (setq sock 
      (socket:make-socket :remote-host host
			  :remote-port (or (net.uri:uri-port uri) 80)))
    
    (format sock "~a ~a ~a~a"
	    (string-upcase (string method))
	    (or (net.uri:uri-path uri) "/")
	    (string-upcase (string protocol))
	    crlf)
    
    ; now the headers
    (if* keep-alive
       then (format sock "Connection: Keep-Alive~a" crlf))

    (if* accept
       then (format sock "Accept: ~a~a" accept crlf))
	    
    ; cookies ?
    

    (write-string crlf sock)  ; final crlf
    
    (force-output sock)
    
    (make-instance 'client-request
      :socket sock)))


    
(defmethod read-client-response-headers ((creq client-response))
  ;; read the response and the headers
  (let ((buff (get-header-line-buffer))
	(buff2 (get-header-line-buffer))
	(pos 0)
	(len)
	(sock (client-request-socket creq))
	(headers)
	protcol
	response
	comment
	saveheader
	saveheader-len
	)
    (with-better-scan-macros
	(if* (null (setq len (read-socket-line creq buff (length buff))))
	   then ; eof getting response
		(error "premature eof from server"))
      (macrolet ((fail ()
		   (let ((i 0))
		     (error "illegal response from web server: ~s"
			    (collect-to-eol buff i len)))))
	(setq protocol (collect-to #\space buff pos len))
	(skip-to-not #\space buff pos len)
	(setq response (collect-to #\space buff pos len))
	(skip-to-not #\space buff pos len)
	(setq comment (collect-to-eol buff pos len)))

      (if* (equalp protocol "HTTP/1.0")
	 then (setq protocol :http/1.0)
       elseif (equalp protocol "HTTP/1.1")
	 else (error "unknown protocol: ~s" protocol))
      
      (setf (client-request-protocol creq) protocol)
      
      (setf (client-request-response-code creq) 
	(quick-convert-to-integer respone))
      
      (setf (client-request-response-comment creq) comment)
      
     
      ; now read the header lines
      (loop
	(if* saveheader
	   then ; buff2 has the saved header we should work on next
		(exch buff buff2)
		(setq len len2
		      saveheader nil)
	 elseif (null (setq len (read-socket-line creq buff (length buff))))
	   then ; eof before header lines
		(error "premature eof in headers")
	 elseif (eql len 0)
	   then ; last header line
		(return))
	  
	; got header line. Must get next one to see if it's a continuation
	(if* (null (setq len2 (read-socket-line creq buff2 (length buff2))))
	   then ; eof before crlf ending the headers
		(error "premature eof in headers")
	 elseif (and (> len2 0)
		     (eq #\space (schar buff2 0)))
	   then ; a continuation line
		(if* (< (length buff) (+ len len2))
		   then (let ((buff3 (make-array (+ len len2 50)
						 :element-type 'character)))
			  (dotimes (i len)
			    (setf (schar buff3 i) (schar buff i))
			    (put-header-line-buffer buff)
			    (setq buff buff3))))
		; can all fit in buff
		(do ((to len (1+ to))
		     (from 0 (1+ from)))
		    ((>= from len2))
		  (setf (schar buff to) (schar buff2 from))
		  )
	   else ; must be a new header line
		(setq saveheader t))
	  
	; parse header
	(let ((pos 0)
	      (headername)
	      (headervalue))
	  (macrolet ((fail ()
		       (let ((i 0))
			 `(error "header line missing a colon:  ~s" 
				 (collect-to-eol buff i len)))))
	    (setq headername (collect-to #\: buff pos len)))
	  
	  (incf pos) ; past colon
	  (skip-to-not #\space buff pos len)
	  (setq headervalue (collect-to-eol buff pos len))
	  
	  (push (cons headername headervalue) headers))))))
		  
	      
(defun quick-convert-to-integer (str)
  ; take the simple string and convert it to an integer
  ; it's assumed to be a positive number
  ; no error checking is done.
  (let ((res 0))
    (dotimes (i (length str))
      (let ((chn (- (char-code (schar str i)) #.(char-code #\0))))
	(if* (<= 0 chn 9)
	   then (setq res (+ (* 10 res) chn)))))
    res))

	
      
;; buffer pool for string buffers of the right size for a header
;; line

(defvar *response-header-buffers* nil)


    
    
    
  
	    
  
		 




		    
  
			
					   
  
			
		      
  


		
		

	    
    
	      
    
    
    
      
    
  
  
