;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; cgi.cl
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
;; $Id: cgi.cl,v 1.3 2001/09/29 18:03:51 jkf Exp $

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
			script-name
			(query-string nil query-string-p)
			auth-type
			(timeout 200)
			)
  ;; program is a string naming a external command to run.
  ;; invoke the program after setting all of the environment variables
  ;; according to the cgi specification.
  ;; http://hoohoo.ncsa.uiuc.edu/cgi/interface.html
  ;;
  
  (let ((envs (list '("GATEWAY_INTERFACE" . "CGI/1.1")
		    `("SERVER_SOFTWARE" 
		      . ,(format nil "AllegroServe/~a"
				 *aserve-version-string*))))
	(body))
    
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
    
    ;; now to invoke the program
    ;; this requires acl6.1 on unix since this is the first version
    ;; that can set the environment variables for the run-shell-command
    ;; call
    
    (multiple-value-bind
	(to-script-stream
	 from-script-stream
	 ignore-this
	 pid)
	(run-shell-command program
			   :input (if* body then :stream)
			   :output :stream
			   :error-output :output
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
	    (multiple-value-bind (len buffs)
		(read-script-data from-script-stream timeout)
	      
	      (ignore-errors (close from-script-stream))
	      (setq from-script-stream nil)
	      
	      
	      (if* (null len)
		 then (failed-script-response req ent)
		 else (successful-script-response req ent buffs len))))
    
    
	;; cleanup forms:
	(if* to-script-stream
	   then (ignore-errors (close to-script-stream)))
	(if* from-script-stream
	   then (ignore-errors (close from-script-stream)))
	(if* pid
	   then ;; it may be bad to wait here...
		(mp:with-timeout (60)
		  (sys:reap-os-subprocess :pid pid :wait t)))))))

    
    
    
    
(defun read-script-data (stream timeout)
  ;; read all the script data.
  ;; time is how long we should block waiting for a response
  ;; from the script
  ;; return  nil  - error occured, no data to return
  ;; 2 values  count, buffers  - read count bytes into the given buffers
  ;;   the buffers are header-block buffers and must be freed when done.

  (let (buffs (total-len 0))
    (block outer
      (handler-case
	  (loop
	    (let ((buff (get-header-block))
		  (start 0))
	      (push buff buffs)
	      (loop
		(mp:with-timeout (timeout (error "cgi timed out"))
		  (let ((len (read-sequence buff stream
					    :start start)))
		    (if* (<= len start)
		       then ; end of file
			    (incf total-len len)
			    (return-from outer)
		     elseif (eql len (length buff))
		       then ; buffer is full
			    (incf total-len len)
			    (return) ; exit inner loop
		       else (setq start len)))))))
	(error (c)
	  ; something went wrong reading
	  ; give back buffs
	  (warn "error during cgi script read: ~a" c)
	  (dolist (buff buffs)
	    (free-header-block buff))
	
	  (return-from read-script-data nil))))
    
    (values total-len (nreverse buffs))))
    
    
      
			    
		    
	    

    
    

(defun failed-script-response (req ent)
  ;; send back a generic failed message
  (with-http-response (req ent 
			   :response *response-internal-server-error*
			   :content-type "text/html")
    (with-http-body (req ent)
      (html "The cgi script failed to run"))))



(defun successful-script-response (req ent buffs len)
  ;; We've got the response from the script.
  ;; It begins with a mini-header which we have to parse
  ;; which tells us how to respond.


  ; scan for end of headers
  (unwind-protect
      (let* ((loc (search *crlf-crlf-usb8* (car buffs)
			  :end2 (min (length (car buffs)) len)))
	     (loclflf (and (null loc)
			   ;; maybe uses bogus lf-lf to end headers
			   (search *lf-lf-usb8* (car buffs)
				   :end2 (min (length (car buffs)) len))))
	     (incr 2))
	
	(if* loclflf
	   then (setq loc loclflf
		      incr 1))
	
	(if*  (null loc) 
	   then ; hmm.. no headers..bogus return
		;(warn "no headers found")
		(return-from successful-script-response 
		  (failed-script-response req ent)))
	    
        (incf loc incr) ; after last header crlf (lf), before final crlf (lf)
	(let ((headers (parse-and-listify-header-block
			(car buffs)
			loc))
	      (resp *response-ok*))

	  
	  (incf loc incr) ; past the final crlf (lf)
	      
	  (let ((location (assoc :location headers :test #'eq)))
	    (if* location
	       then (setq resp *response-moved-permanently*)))
	      
	      
	  (let ((status (assoc :status headers :test #'eq))
		code
		reason)
		
	    (if* status
	       then (ignore-errors 
		     (setq code (read-from-string (cdr status))))
		    (if* (not (integerp code))
		       then ; bogus status value
			    (return-from successful-script-response 
			      (failed-script-response req ent)))
		    (let ((space (find #\space (cdr status))))
		      (if* space
			 then (setq reason
				(subseq (cdr status) space))))
		    (setq resp (make-resp code reason))
			
		    (setq headers (delete status headers))))
		
	  (if* (null (assoc :content-length headers :test #'eq))
	     then ; must add content length
		  (push (cons :content-length
			      (write-to-string
			       (max 0 (- len loc))))
			headers))
		
	  ; send back response
	  (with-http-response (req ent :response resp)
	    ;; can't allow string output stream here since
	    ;; we're writing binary.  since we speicied content
	    ;; length we shouldn't get a string output stream
	    (with-http-body (req ent :headers headers)
	      ; not write out the buffers
	      (decf len loc) ; len will be byte remaining
		  
	      (dolist (buff buffs)
		(let ((write-this-time 
		       (min len (- (length buff) loc))))
		  (if* (> write-this-time 0)
		     then (write-sequence buff 
					  *html-stream*
					  :start loc
					  :end (+ loc write-this-time)))
		  (decf len write-this-time)
		  (setq loc 0)))))))
    ;; cleanup form
    ;; free buffers
    (dolist (buff buffs)
      (free-header-block buff))
    ))

						    
