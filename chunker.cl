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
  ())

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
  (let ((inner-stream (slot-value p 'excl::output-handle)))
    
    (if* (not abort) then (force-output p))

    ; chunking eof
    (write-char #\0 inner-stream)
    (write-sequence *binary-crlf* inner-stream)
    (write-sequence *binary-crlf* inner-stream)
    (force-output inner-stream)
    p))

(without-package-locks
(defmethod excl::inner-stream ((p chunking-stream))
  (slot-value p 'excl::output-handle)))


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
	   :accessor unchunking-count)))


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
		   (let ((seen-stuff-on-line nil))
		     (loop 
		       (let ((ch (read-byte ins nil nil)))
			 (if* (null ch) then (error "premature eof before chunking data"))
			 (if* (eq ch #.(char-int #\newline))
			    then (if* (not seen-stuff-on-line)
				    then (return)
				    else (setq seen-stuff-on-line nil) ; reset
					 )
			  elseif (eq ch #.(char-int #\return))
			    then nil ;ignore
			    else (setq seen-stuff-on-line t)))))
		 
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


	 
		     
	 
	     
		     
		     
		     
		     
  
  
