;; -*- mode: common-lisp; package: net.iserve -*-
;;
;; log.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: log.cl,v 1.5.2.1 2000/02/08 19:48:37 jkf Exp $

;; Description:
;;   neo's logging

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.iserve)


(defun logmess (message)
  (multiple-value-bind (csec cmin chour cday cmonth cyear)
      (decode-universal-time (get-universal-time))
    
    (format t "~a: ~2,'0d/~2,'0d/~2,'0d - ~2,'0d:~2,'0d:~2,'0d - ~a~%"
	    (mp:process-name sys:*current-process*)
	    cmonth cday (mod cyear 100)
	    chour cmin csec
	    message)))






(defun log-timed-out-request-read (socket)
  (logmess (format nil "No request read from address ~a" 
		   (socket::ipaddr-to-dotted (socket::remote-host socket)))))



(defmethod log-request ((req http-request))
  ;; after the request has been processed, write out log line
  (let ((ipaddr (socket:remote-host (request-socket req)))
	(time   (request-reply-date req))
	(code   (let ((obj (request-reply-code req)))
		  (if* obj
		     then (response-number obj)
		     else 999)))
	(length  (request-reply-content-length req))
	
	(stream (wserver-log-stream
		 (request-wserver req))))
    
    (format stream
	    "~a - - [~a] ~s ~s ~s~%"
	    (socket:ipaddr-to-dotted ipaddr)
	    (universal-time-to-date time)
	    (request-raw-request req)
	    code
	    (or length -1))))

	    	
    
    
    
    
  
