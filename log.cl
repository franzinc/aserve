;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; log.cl
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
;;   iserve's logging

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)

(defun log1 (category level message &key (logger *logger*))
  (log1* logger category level message))

(defgeneric log1* (logger category level message)
  (:documentation "This the new, extensible logger interface to which
all others defer. By default, category :access is handled by
log-request* while the rest goes to logmess-stream. Note message is
not necessarily a string: for instance it is a request object
for :access which allows for more flexibility in presentation.")
  (:method (logger category level message)
    (declare (ignore logger))
    (logmess-stream category level message *debug-stream*))
  (:method (logger (category (eql :xmit-server-response-headers)) level message)
    (declare (ignore logger))
    ;; time is :pre or :post depending on whether the headers are
    ;; generated before or after the body
    (destructuring-bind (time string) message
      (logmess-stream category level (format nil "~a ~s" time string)
                      *debug-stream*)))
  (:method (logger (category (eql :access)) level (request http-request))
    (declare (ignore logger level))
    (log-request* request)))

(defvar *enable-logging* t) ; to turn on/off the standard logging method

(defvar *save-commands* nil) ; if true then a stream to which to write commands

(defun logmess (message &optional (format :long))
  (log-for-wserver *wserver* message format))

(defmethod log-for-wserver ((wserver wserver) message format)
  ;; send log message to the default vhost's error stream
  (let ((*debug-stream* (vhost-error-stream (wserver-default-vhost wserver)))
        (*debug-format* format))
    (log1 :aserve :info message)))

(defvar *log-time-zone* 0)

(defmethod logmess-stream (category level message stream
                           &optional (format *debug-format*))
  ;; send the log message to the given stream which should be a
  ;; stream object and not a stream indicator (like t)
  ;; If the stream has a lock use that.
  (declare (ignore level))
  (multiple-value-bind (csec cmin chour cday cmonth cyear)
      (decode-universal-time (get-universal-time) *log-time-zone*)
    (let* ((*print-pretty* nil)
	   (str (ecase format
                  (:long
                   (format
                    nil "[~a] ~a: ~2,'0d/~2,'0d/~2,'0d - ~2,'0d:~2,'0d:~2,'0d - ~a~%"
                    category (mp:process-name sys:*current-process*)
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
     then ;; By default this ends up calling log-request*.
          (log1 :access :info req))
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
		    (header-slot-value req :content-type)))
	  (force-output *save-commands*)))

(defun log-request* (req)
  (let* ((entry (format-access-log-entry req))
         (stream (vhost-log-stream (request-vhost req)))
         (lock (and (streamp stream)
                    (getf (excl::stream-property-list stream) 
                          :lock))))
    (macrolet ((do-log ()
                 '(progn (format stream "~a~%" entry)
                   (force-output stream))))
      (if* lock
         then (mp:with-process-lock (lock)
                ; in case stream switched out while we weren't busy
                ; get the stream again
                (setq stream (vhost-log-stream (request-vhost req)))
                (do-log))
         else (do-log)))))

(defun format-access-log-entry (req)
  (let* ((ipaddr (socket:remote-host (request-socket req)))
         (time   (request-reply-date req))
         (code   (let ((obj (request-reply-code req)))
                   (if* obj
                      then (response-number obj)
                      else 999)))
         (length (or (request-reply-content-length req)
                     #+(and allegro (version>= 6))
                     (excl::socket-bytes-written 
                      (request-socket req)))))
    (format nil "~A~A~a - - [~a] ~s ~s ~s"
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
            (or length -1))))
    
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
