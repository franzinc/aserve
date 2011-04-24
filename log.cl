;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; log.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: log.cl,v 1.27 2008/02/04 19:03:59 jkf Exp $

;; Description:
;;   iserve's logging

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)

(defvar *enable-logging* t) ; to turn on/off the standard logging method

(defvar *save-commands* nil) ; if true then a stream to which to write commands

(defun logmess (message &optional (format :long))
  (log-for-wserver *wserver* message format))

(defmethod log-for-wserver ((wserver wserver) message format)
  ;; send log message to the default vhost's error stream 
  (logmess-stream message (vhost-error-stream (wserver-default-vhost wserver)) format))

(defmethod logmess-stream (message stream &optional (format :long))
  ;; send the log message to the given stream which should be a
  ;; stream object and not a stream indicator (like t)
  ;; If the stream has a lock use that.
  (multiple-value-bind (csec cmin chour cday cmonth cyear)
      (decode-universal-time (get-universal-time))
    (let* ((*print-pretty* nil)
	   (str (ecase format
                  (:long
                   (format
                    nil "~a: ~2,'0d/~2,'0d/~2,'0d - ~2,'0d:~2,'0d:~2,'0d - ~a~%"
                    (mp:process-name sys:*current-process*)
                    cmonth cday (mod cyear 100) chour cmin csec
                    message))
                  (:brief
                   (format nil "~2,'0d:~2,'0d:~2,'0d - ~a~%" chour cmin csec
                           message))))
	   (lock (getf (excl::stream-property-list stream) :lock)))
      (if* lock
	 then (mp:with-process-lock (lock)
		(if* (open-stream-p stream)
		   then (write-sequence str stream)
			(finish-output stream)))
	 else (write-sequence str stream)
	      (finish-output stream)))))


(defmethod log-request ((req http-request))
  ;; after the request has been processed, write out log line
  (if* *enable-logging*
     then (let* ((ipaddr (socket:remote-host (request-socket req)))
		 (time   (request-reply-date req))
		 (code   (let ((obj (request-reply-code req)))
			   (if* obj
			      then (response-number obj)
			      else 999)))
		 (length  (or (request-reply-content-length req)
			      #+(and allegro (version>= 6))
			      (excl::socket-bytes-written 
			       (request-socket req))))
	
		 (stream (vhost-log-stream (request-vhost req)))
		
		 (lock (and (streamp stream)
			    (getf (excl::stream-property-list stream) 
				  :lock))))

	    (macrolet ((do-log ()
			 '(progn (format stream
				  "~A~A~a - - [~a] ~s ~s ~s~%"
				  (if* *log-wserver-name* 
				     then (wserver-name *wserver*) 
				     else "")
				  (if* *log-wserver-name* 
				     then " " 
				     else "")
				  (socket:ipaddr-to-dotted ipaddr)
				  (maybe-universal-time-to-date time)
				  (request-raw-request req)
				  code
				  (or length -1))
			   (force-output stream))))
			 
	      (if* lock
		 then (mp:with-process-lock (lock)
			; in case stream switched out while we weren't busy
			; get the stream again
			(setq stream (vhost-log-stream (request-vhost req)))
			(do-log))
		 else (do-log)))))
  
  (if* *save-commands*
     then (multiple-value-bind (ok whole uri-string)
	      (match-re "^[^ ]+\\s+([^ ]+)" (request-raw-request req))
	    (declare (ignore ok whole))
	    (format *save-commands*
		    "((:method . ~s) (:uri . ~s) (:proto . ~s) ~% (:code . ~s)~@[~% (:body . ~s)~]~@[~% (:auth .  ~s)~]~@[~% (:ctype . ~s)~])~%" 
		    (request-method req)
		    uri-string
		    (request-protocol req)
		    (let ((obj (request-reply-code req)))
		      (if* obj
			 then (response-number obj)
			 else 999))
		    (let ((bod (request-request-body req)))
		      (and (not (equal "" bod)) bod))
		    (multiple-value-list (get-basic-authorization req))
		    (header-slot-value req :content-type)
		    ))
	  (force-output *save-commands*))
		  
	  
  )

	    	
    
    
(defun log-proxy (uri level action extra)
  ;; log information from the proxy module
  ;;
  (logmess 
   (format nil "~a ~d ~a ~a~@[ ~s~]"
	   (or (getf (mp:process-property-list mp:*current-process*)
		     'short-name)
	       (mp:process-name mp:*current-process*))
	   level
	   action
	   (if* (stringp uri) 
	      then uri 
	      else (net.uri:render-uri uri nil))
	   extra)
   :brief))

    
  

