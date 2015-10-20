;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; chunker.cl
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

;; Description:
;;   aserve's main loop

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)


; stream that reads the input and chunks the data to the output

(def-stream-class chunking-stream (single-channel-simple-stream)
  ((trailers :initform nil :accessor chunking-stream-trailers)
   (eof-sent :initform nil :accessor chunking-stream-eof-sent)))


(defmethod chunking-stream-trailers (stream)
  ;; so this will return nil for non chunkers
  (declare (ignore stream))
  nil)


(defvar *binary-crlf*
    (make-array 2 :element-type '(unsigned-byte 8)
		:initial-contents '(#.(char-code #\return)
				    #.(char-code #\linefeed))))


(defmethod device-open ((p chunking-stream) dummy options)
  (declare (ignore dummy))
  
  (let ((output-handle (getf options :output-handle)))
    
    (setf (slot-value p 'excl::input-handle) nil)
    (setf (slot-value p 'excl::output-handle) output-handle)
    
    (install-single-channel-character-strategy
     p (stream-external-format output-handle)
     nil)
    
    (add-stream-instance-flags p :output :simple)
    
    (setf (slot-value p  'excl::buffer)
      (or (getf options :buffer)
	  (make-array (* 4 1024) :element-type '(unsigned-byte 8))))
    
    (setf (slot-value p 'excl::control-out) excl::*std-control-out-table*)

    ;; this is so that if a deflate stream is sending data to the chunker
    ;; the deflate stream will know to send a chunking eof when the deflate
    ;; stream is closed.  It doesn't want to close the chunking stream since
    ;; due to the logic in publish.cl there may be further attempts to 
    ;; force-output the chunking stream and close it.
    (locally
	(declare (special sys::*deflate-target-stream-close-hook*))
      (setq sys::*deflate-target-stream-close-hook* 'send-chunking-eof))
    t))


(defmethod print-object ((stream chunking-stream) s)
  (print-unreadable-object (stream s :identity *print-escape* :type t)
    (format s "ef ~s, to ~s " 
	    (excl::ef-name (stream-external-format stream))
	    (slot-value stream 'excl::output-handle))))

(defmethod device-write ((p chunking-stream) buffer start end blocking)
  (declare (ignore blocking))
  
  (setf (slot-value p 'excl::oc-state) nil)
  
  (let ((buffer (or buffer (slot-value p 'excl::buffer)))
	(output (slot-value p 'excl::output-handle)))
    
    (setq end (or end (length buffer)))
    
    (if* (> end start)
       then ; write it out with chunking
	    ; chunk header
	    (format output "~x" (- end start))
	    (write-sequence *binary-crlf* output)
	    (write-sequence buffer output :start start :end end)
	    (write-sequence *binary-crlf* output)
	    (force-output output)))
  
  end)


	    

(defmethod device-close ((p chunking-stream) abort)
  ;;
  ;; note: we don't close the inner stream
  ;;
  
  (if* (not abort) 
     then (force-output p)
          (send-chunking-eof p))
  p)
	  
  
  

(defmethod send-chunking-eof ((p chunking-stream))
  (if* (not (chunking-stream-eof-sent p))
     then (let ((inner-stream (slot-value p 'excl::output-handle)))
    

	    ; chunking eof
	    (write-char #\0 inner-stream)
	    (write-sequence *binary-crlf* inner-stream)
	    (dolist (trailer (chunking-stream-trailers p))
	      (write-sequence (string (car trailer)) inner-stream)
	      (write-sequence ": " inner-stream)
	      (if* (cdr trailer) then (write-sequence (cdr trailer) inner-stream))
	      (write-sequence *binary-crlf* inner-stream))
	    (write-sequence *binary-crlf* inner-stream)
	    (force-output inner-stream)
	    
	    (setf (chunking-stream-eof-sent p) t))))

(defmethod send-chunking-eof (p)
  ;; do nothing for other kinds of streams
  p
  )

	  
  

(without-package-locks
(defmethod excl::inner-stream ((p chunking-stream))
  (slot-value p 'excl::output-handle)))


(defmethod set-trailers (p  trailers)
  ;; by default we can't set trailers
  (declare (ignore p trailers))
  ;; do nothing
  nil
  )

(defmethod can-set-trailers-p (p)
  (declare (ignore p))
  nil
  )

#+zlib-deflate
(defmethod set-trailers ((p deflate-stream) trailers)
  (set-trailers (deflate-target-stream p) trailers))

#+zlib-deflate
(defmethod can-set-trailers-p ((p deflate-stream))
  (can-set-trailers-p (deflate-target-stream p)))


(defmethod set-trailers ((p chunking-stream) trailers)
  ;; Set the values only for the trailers we've already declared
  ;;
  (let ((saved-trailers (chunking-stream-trailers p)))
    (if* (consp trailers)
       then (dolist (trailer trailers)
	      (if* (and (consp trailer)
			(or (stringp (car trailer))
			    (symbolp (car trailer)))
			(stringp (cdr trailer)))
		 then (let ((ent (assoc (header-kwdize (car trailer)) saved-trailers
					:test #'eq)))
			(if* ent
			   then (setf (cdr ent) (cdr trailer)))))))))
(defmethod can-set-trailers-p ((p chunking-stream))
  t)





(defmethod set-trailers ((req http-request) trailers)
  (set-trailers (request-reply-stream req) trailers))

(defmethod can-set-trailers-p ((req http-request))
  (can-set-trailers-p (request-reply-stream req)))



		      











;;; un chunking
;;
;; (make-instance 'unchunker-stream  :input-handle stream)
;;
;; creates a unchunker-stream which filters the input-handle stream
;; filters out the meta data about chunk sizes.
;; unchunker stream returns eof when the end of the chunked data
;; is reached however the inner stream is not closed.
;;
(def-stream-class unchunking-stream (single-channel-simple-stream)
  ((state :initform :need-count
	  :accessor unchunking-state)
   (count  :initform 0
	   :accessor unchunking-count)
   (trailers :initform nil
	     :accessor unchunking-trailers)
   ))

(defmethod unchunking-trailers ((stream inflate-stream))
  (unchunking-trailers (slot-value stream 'excl::input-handle)))

(defmethod unchunking-trailers (stream)
  ;; so this returns nil if stream isn't a chunker
  (declare (ignore stream))
  nil
  )

(defmethod device-open ((p unchunking-stream) dummy options)
  (declare (ignore dummy))
  (let ((input-handle (getf options :input-handle)))
    
    (setf (slot-value p 'excl::input-handle) input-handle)
    (setf (slot-value p 'excl::output-handle) nil)
    
    (install-single-channel-character-strategy
     p (stream-external-format input-handle) nil)
    
    (setf (stream-external-format p) (stream-external-format input-handle))
    
    (add-stream-instance-flags p :input :simple)
    
    (setf (slot-value p  'excl::buffer)
      (or (getf options :buffer)
	  (make-array (* 4 1024) :element-type '(unsigned-byte 8))))

    (setf (slot-value p 'excl::co-state) nil)
    (setf (slot-value p 'excl::oc-state) nil)
    
    
    
    t))

(defmethod print-object ((stream unchunking-stream) s)
  (print-unreadable-object (stream s :identity *print-escape* :type t)
    (format s "ef ~s, from ~s " 
	    (excl::ef-name (stream-external-format stream))
	    (slot-value stream 'excl::input-handle))))

(defmethod device-read ((p unchunking-stream) buffer start end blocking)

  (declare (ignore blocking))
  
  (setq buffer (or buffer (slot-value p 'excl::buffer)))
  (setq end    (or end (length buffer)))

  (loop
    (let ((state (unchunking-state p))
	  (ins (slot-value p 'excl::input-handle)))
      (case state
	(:eof
	 (return-from device-read -1))
	(:need-count 
	 (let ((count 0)
	       (seen nil))
	 
	   ; read and parse
	   (loop
	     (let ((ch (read-byte ins nil nil)))
	       (if* (null ch)
		  then (error "premature eof before chunking data"))
	       (if* (<= #.(char-int #\0) ch #.(char-int #\9))
		  then (setq count (+ (ash count 4) (- ch #.(char-int #\0)))
			     seen  t)
		elseif (<= #.(char-int #\a) ch #.(char-int #\f))
		  then (setq count (+ (ash count 4)  10 (- ch #.(char-int #\a)))
			     seen t)
		elseif (<= #.(char-int #\A) ch #.(char-int #\F))
		  then (setq count (+ (ash count 4) (+ 10 (- ch #.(char-int #\A))))
			     seen t)
		elseif seen
		  then (return) ; valid count
		  else ; non hex char
		       (error "bad chunking count"))))
	   
	   ; now skip to newline
	   (loop 
	     (let ((ch (read-byte ins nil nil)))
	       (if* (null ch) then (error "premature eof before chunking data"))
	       (if* (eq ch #.(char-int #\newline))
		  then (return))))

	   (if* (zerop count)
	      then ; chunking eof, read trailers and trailing crlf
 
		   ; read any trailers
		   (setf (unchunking-trailers p) (read-trailers ins))
		   ; return signal of eof
		   (setf (unchunking-state p) :eof)
		   (return-from device-read -1)
	      else ; now ready to ready data
		   (setf (unchunking-state p) :read-data
			 (unchunking-count p) count)
		 
		 
		   )))
	
	(:read-data
	 ;; we'll read all we can from this chunk
	 (let ((count (unchunking-count p)))
	   (if* (zerop count)
	      then ; read crlf
		   
		   (setf (unchunking-state p) :need-count)
	      else (do ((bytes-read 0)
                        (i start (+ i bytes-read))
                        (count count (- count bytes-read)))
                       ((or (zerop count)
                            (>= i end))

                        (if* (zerop count)
                           then (loop (let ((ch (read-byte ins)))
                                        (if* (eq ch #.(char-int #\newline))
                                           then (return))))
                                (setf (unchunking-state p) :need-count))
                        (setf (unchunking-count p) count)
                        (return-from device-read (- i start)))
                     (let ((res (read-vector buffer ins :start i :end (min (+ i count) end))))
                       (if* (= res i)
                          then (return-from device-read -1))
                       (setf bytes-read (- res i)))))))))))

(defun read-trailers (ins)
  ;; read the trailers from the unchunking stream
  (let ((trailers)
	(header)
	(value))
    (loop
      (let ((ch (read-byte ins nil nil)))
	(if* (null ch) then (error "premature eof before chunking data"))
	(if* (eq ch #.(char-int #\newline))
	   then ; see if we've seen a header value
		(if* header
		   then (setq header (make-array (length header)
						 :element-type 'character
						 :initial-contents (nreverse header))
			      value  (if* value
					then (let ((value (nreverse value)))
					       ; eliminate leading blanks
					       (loop
						 (if* (eq #\space (car value))
						    then (pop value)
						    else (return)))
					       (setq value
						 (make-array (length value)
							     :element-type 'character
							     :initial-contents 
							     value)))
					else (setq value "")))
			(push (cons header value) trailers)
			(setq header nil  value nil)
		   else ; blank line, end of trailers
			(return trailers))

	 elseif (eq ch #.(char-int #\return))
	   then nil ;ignore
	   else (if* (eq ch #.(char-int #\:))
		   then (if* value
			   then (push (code-char ch) value)
			 elseif header
			   then (push  #\space value)
			   else ; colon beginning a line.. bogus
				; so create a header
				(setq header (list #\x #\x #\x)
				      value (list #\space)))
		 elseif value
		   then (push (code-char ch) value)
		   else (push (code-char ch) header)))))))
			
					
				
  


(defmethod device-close ((p unchunking-stream) abort)
  (declare (ignore abort))
  
  p)

(without-package-locks
(defmethod excl::inner-stream ((p unchunking-stream))
  (slot-value p 'excl::input-handle)))

(without-package-locks
 
(defmethod excl::record-stream-advance-to-eof ((p unchunking-stream))
   ;; if the stream is composed of records ending in a pseudo eof
   ;; then read up to an eof
   (loop 
     ;; advance to eof
     (if* (null (read-byte p nil nil)) then (return)))
   p))




		     
	 
	     
		     
		     
		     
		     
  
  
