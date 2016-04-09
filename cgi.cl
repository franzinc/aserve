;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; cgi.cl
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
;;   common gateway interface (running external programs)

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(in-package :net.aserve)

(defun run-cgi-program (req ent program
			&key
			path-info
			path-translated
			(script-name (net.uri:uri-path (request-uri req)))
			(query-string nil query-string-p)
			auth-type
			(timeout 200)
			error-output
			env
			terminate
			)
  ;; program is a string naming a external command to run.
  ;; invoke the program after setting all of the environment variables
  ;; according to the cgi specification.
  ;; http://hoohoo.ncsa.uiuc.edu/cgi/interface.html
  ;;
  ;; error-output can be
  ;;   nil - inherit lisp's standard output
  ;;   pathname or string - write to file of a given name
  ;;   :output - mix in the error output with the output
  ;;   function - call function when input's available from the error 
  ;;		stream
  
  (declare (ignorable terminate)) ; not used in Windows
  
  (let ((envs (list '("GATEWAY_INTERFACE" . "CGI/1.1")
		    `("SERVER_SOFTWARE" 
		      . ,(format nil "AllegroServe/~a"
				 *aserve-version-string*))))
	(error-output-arg)
	(error-fcn)
	(body))
    
    ; error check the error argument
    (typecase error-output
      ((or null pathname string) 
       (setq error-output-arg error-output))
      (symbol
       (if* (eq error-output :output)
	  then (setq error-output-arg error-output)
	  else (setq error-output-arg :stream
		     error-fcn error-output)))
      (function
       (setq error-output-arg :stream
	     error-fcn error-output))
      (t (error "illegal value for error-output: ~s" error-output)))
    
    
    (let ((our-ip (socket:local-host (request-socket req))))
      (let ((hostname (socket:ipaddr-to-hostname our-ip)))
	(if* (null hostname) 
	   then (setq hostname (socket:ipaddr-to-dotted our-ip)))
	(push (cons "SERVER_NAME" hostname) envs)))
    
    (push (cons "SERVER_PROTOCOL"
		(string-upcase (string (request-protocol req))))
	  envs)
    
    (push (cons "SERVER_PORT"
		(write-to-string (socket:local-port 
				  (request-socket req))))
	  envs)
    
    (push (cons "REQUEST_METHOD"
		(string-upcase (string (request-method req))))
	  envs)
    
    (if* path-info
       then (push (cons "PATH_INFO" path-info) envs))
    
    (if* path-translated
       then (push (cons "PATH_INFO" path-translated) envs))
    
    (if* script-name
       then (push (cons "SCRIPT_NAME" script-name) envs))
    
    (if* query-string-p
       then (if* query-string
	       then (push (cons "QUERY_STRING" query-string) envs))
       else ; no query string arg given, see if the uri
	    ; for ths command has a query string
	    (let ((query (net.uri:uri-query 
			  (request-uri req))))
	      (if* query
		 then (push (cons "QUERY_STRING" query) envs))))
    
    
    (let ((their-ip (socket:remote-host (request-socket req))))
      (let ((hostname (socket:ipaddr-to-hostname their-ip)))
	(if*  hostname
	   then (push (cons "REMOTE_HOST" hostname) envs)))
      
      (push (cons "REMOTE_ADDR" (socket:ipaddr-to-dotted their-ip))
	    envs))
    
    (if* auth-type
       then (push (cons "AUTH_TYPE" auth-type) envs))
    
    (if* (member (request-method req) '(:put :post))
       then ; there is likely data coming along
	    (setq body (get-request-body req	))
	    (if* (equal body "") then (setq body nil)) ; trivial case
	    (let ((content-type (header-slot-value req :content-type)))
	      (if* content-type 
		 then (push (cons "CONTENT_TYPE"
				  content-type)
			    envs))
	      (push (cons "CONTENT_LENGTH"
			  (princ-to-string
			   (if* body then (length body) else 0)))
		    envs)))
    
    ; now do the rest of the headers.
    
    (dolist (head (listify-parsed-header-block (request-header-block req)))
      (if* (and (not (member (car head) '(:content-type :content-length)
			     :test #'eq))
		(cdr head))
	 then (push (cons (format nil "HTTP_~a"
				  (substitute #\_ #\-
					      (string-upcase 
					       (string (car head)))))
			  (cdr head))
		    envs)))

    (dolist (header env)
      (if* (not (and (consp header)
		     (stringp (car header))
		     (stringp (cdr header))))
	 then (error "bad form for environment value: ~s" header))
      (let ((ent (assoc (car header) envs :test #'equal)))
	(if* ent
	   then ; replace value with user specified value
		(setf (cdr ent) (cdr header))
	   else ; add new value
		(push header envs))))
    
    ;; now to invoke the program
    ;; this requires acl6.1 on unix since this is the first version
    ;; that can set the environment variables for the run-shell-command
    ;; call
    
    (multiple-value-bind
	(to-script-stream
	 from-script-stream
	 from-script-error-stream
	 pid)
	(run-shell-command program
			   :input (if* body then :stream)
			   :output :stream
			   :error-output error-output-arg
			   :separate-streams t
			   :wait nil
			   :environment envs
			   :show-window :hide)
      (declare (ignore ignore-this))
	    
      (unwind-protect
	  ; first send the body to the script
	  ; maybe we should interleave reading and writing
	  ; but that's a lot of work
	  (progn
	    (ignore-errors
	     (if* (and body to-script-stream)
		then (write-sequence body to-script-stream)))
	    
	    (if* to-script-stream
	       then (ignore-errors (close to-script-stream))
		    (setq to-script-stream nil))
	    
	    ; read the output from the script
	    (read-script-data req ent
			      from-script-stream from-script-error-stream
			      error-fcn
			      timeout))
    
    
	;; cleanup forms:
	(if* to-script-stream
	   then (ignore-errors (close to-script-stream)))
	(if* from-script-stream
	   then (ignore-errors (close from-script-stream)))
	(if* from-script-error-stream
	   then (ignore-errors (close from-script-error-stream)))
	(if* pid
	   then ;; wait for process to die
		(if* (null (sys:reap-os-subprocess :pid pid :wait nil))
		   then ; not ready to die yet, but someone 
			; should wait for it to die while we return
			#+unix 
			(if* terminate
			   then ; forceably kill
				(progn (unix-kill pid 15) ; sigterm
				       (sleep 2) ; give it a chance to die
				       (if* (sys:reap-os-subprocess :pid pid :wait nil)
					  then (setq pid nil) ; indicate killed
					  else (unix-kill pid 9) ; kill
					       )))
				       
			(if* pid
			   then ; must have someone wait for the death
				(mp::process-run-function "reaper"
				  #'(lambda () 
				      (dotimes (i 10)
					(sleep (+ 2 (* i 10)))
					(if* (sys:reap-os-subprocess :pid pid
								     :wait nil)
					   then (return))))))))))))


(defun read-script-data (req ent stream error-stream error-fcn timeout)
  ;; read from the stream and the error-stream (if given) 
  ;; do the cgi header processing and start sending output asap
  ;;
  ;; don't close the streams passed, they'll be closed by the caller
  ;;
  (let ((active-streams)
	(buff)
	(start 0))
    
    (labels ((error-stream-handler ()
	       ;; called when data available on error stream.
	       ;; calls user supplied handler function
	       (let ((retcode (funcall error-fcn req ent error-stream)))
		 (if* retcode
		    then ; signal to close off the error stream
			 (setq active-streams
			   (delete error-stream active-streams :key #'car)))))
		 
	     (data-stream-header-read ()
	       ;; called when data available on standard output
	       ;; and we're still reading in search of a full header
	       ;;
	       (if* (>= start (length buff))
		  then ; no more room to read, must be bogus header
		       (failed-script-response req ent)
		       (return-from read-script-data)
		  else (let ((len (read-vector buff stream
						 :start start)))
			 (if* (<= len start)
			    then ; eof, meaning no header
				 (failed-script-response req ent)
				 (return-from read-script-data)
			    else (setq start len)
				 (multiple-value-bind (resp headers bodystart)
				     (parse-cgi-script-data buff start)
				   (if* resp
				      then ; got the header, switch
					   ; to body
					   (data-stream-body-process
					    resp headers bodystart)
					   ; never returns
					   ))))))
		 
	     (data-stream-body-process (resp headers bodystart)
	       ;; called when it's time to start returning the body
	       (with-http-response (req ent :response resp
					:format :binary)
		 (with-http-body (req ent :headers headers)
		   ; write out first block
		   
		   (write-all-vector buff
				     *html-stream*
				     :start bodystart
				     :end start)
		       
		   ; now loop and read rest
		   (setf (cdr (assoc stream active-streams :test #'eq))
		     #'data-stream-body)
		   
		   (loop
		     (if* (null active-streams) 
			then (return))
		     
		     (let ((active
			    (mp:wait-for-input-available 
			     (mapcar #'car active-streams)
			     :timeout timeout)))
		       
		       (if* (null active) 
			  then ; timeout, just shut down streams
			       (setq active-streams nil)
			  else ; run handlers
			       (mapc #'(lambda (x) 
					 (funcall (cdr (assoc x active-streams 
							      :test #'eq))))
				     active)
			       
			       )))))
	       (return-from read-script-data))
	     
	     (data-stream-body ()
	       ;; process data coming back from the body
	       (let ((len (read-vector buff stream)))
		 
		 (if* (<= len 0)
		    then ; end of file, remove this stream
			 (setq active-streams
			   (delete stream active-streams
				   :key #'car))
		    else ; send data to output
			 (write-all-vector buff
					   *html-stream*
					   :start 0
					   :end len)
			 (force-output *html-stream*)))))
			       

      (setq active-streams
	(list (cons stream #'data-stream-header-read)))
      
      (if* error-stream
	 then (push (cons error-stream #'error-stream-handler) 
		    active-streams))
      
      (unwind-protect
	  (progn
	    (setq buff (get-header-block))
	  
						  
			
	    (loop
	      ; this loop is for searching for a valid header
	      
	      (let ((active
		     (mp:wait-for-input-available 
		      (mapcar #'car active-streams) :timeout timeout)))
		
		(if* (null active)
		   then ; must have timed out
			(failed-script-response req ent)
			(return-from read-script-data))
		
		; run the handlers
		(mapc #'(lambda (x) 
			  (funcall (cdr (assoc x active-streams :test #'eq))))
		      active))))
	; cleanup
	(free-header-block buff)))))
    
    
      
    

(defun failed-script-response (req ent)
  ;; send back a generic failed message
  (with-http-response (req ent 
			   :response *response-internal-server-error*
			   :content-type "text/html")
    (with-http-body (req ent)
      (html "The cgi script failed to run"))))



(defun parse-cgi-script-data (buff end)
  ;; if there's a valid header block in the buffer from 0 to end-1
  ;; then return 
  ;; 1. the response object denoting the response value to send back
  ;; 2. a list of headers and values
  ;; 3. the index in the buffer where the data begins after the header
  ;;
  ;; else return nil
  (let* ((loc (search *crlf-crlf-usb8*  buff
			  :end2 (min (length buff) end)))
	     (loclflf (and (null loc)
			   ;; maybe uses bogus lf-lf to end headers
			   (search *lf-lf-usb8*  buff
				   :end2 (min (length buff) end))))
	     (incr 2))
	
	(if* loclflf
	   then (setq loc loclflf
		      incr 1))
	
	(if* (null loc) 
	   then ; hmm.. no headers..bogus return
		;(warn "no headers found")
		(return-from parse-cgi-script-data nil))
	    
        (incf loc incr) ; after last header crlf (lf), before final crlf (lf)
	(let ((headers (parse-and-listify-header-block
			buff
			loc))
	      (resp *response-ok*))

	  
	  (incf loc incr) ; past the final crlf (lf)
	      
	  (if* (assoc :location headers :test #'eq)
	    then (setq resp *response-moved-permanently*))
	      
	      
	  (let ((status (assoc :status headers :test #'eq))
		code
		reason)
		
	    (if* status
	       then (ignore-errors 
		     (setq code (read-from-string (cdr status))))
		    (if* (not (integerp code))
		       then ; bogus status value, just return nil
			    ; eventually we'll get a failed response
			    (logmess 
			     (format nil
				     "cgi script return bogus status value: ~s" 
			     code))
			    (return-from parse-cgi-script-data nil))
		    (let ((space (position #\space (cdr status))))
		      (if* space
			 then (setq reason
				(subseq (cdr status) space))))
		    (setq resp (make-resp code reason))
			
		    (setq headers (delete status headers))))
	  (values resp headers loc))))
		
	  



						    
(defun write-all-vector (sequence stream &key (start 0) 
					      (end (length sequence)))
  ;; write everything in the vector before returning
  (loop
    (if* (< start end)
       then (setq start (write-vector sequence stream 
				      :start start
				      :end end))
       else (return)))
  
  end)

	  
