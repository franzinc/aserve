;; -*- mode: common-lisp; package: net.aserve.client -*-
;;
;; client.cl
;;
;; See the file LICENSE for the full license governing this code.
;;
;;

;; Description:
;;   stream for reading  from a creq object

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


;; this will evolve into the http client code but for now it's
;; just some simple stuff to allow us to test aserve
;;

(in-package :net.aserve.client)

;; for creating a stream
(eval-when (compile eval)
  (require :iodefs)
  (require :process))


(defclass creq-stream (single-channel-simple-stream)
  ((creq :initarg :creq :accessor creq-stream-creq)
   (at-eof :initform nil :accessor creq-stream-at-eof)
   ))

(defmethod print-object ((stream creq-stream) s)
  (print-unreadable-object (stream s :identity *print-escape* :type t)
    (format s "reading ~s" 
            (net.uri:render-uri (client-request-uri (creq-stream-creq stream)) nil))))


(defmethod device-open ((s creq-stream) slot-names initargs)
  (declare (ignore slot-names))
  (setf (creq-stream-creq s) (getf initargs :creq))
  
  (with-stream-class (creq-stream s)
    (setf (sm excl::buffer s) 
      (make-array (* 6 1024 1024) :element-type '(unsigned-byte 8)))
  
    (setf (sm excl::buffer-ptr s) 0)
    (setf (sm excl::buffpos s) 0)
  
    (add-stream-instance-flags s :input :simple)
  
    (excl::set-sc-stream-external-format-body s :utf-8 :access 'excl::buffer)
  
    t))

(defmethod device-read ((s creq-stream) buffer start end blocking)
  (declare (ignore blocking))
  
  (with-stream-class (creq-stream s)
    (if* (creq-stream-at-eof s)
       then 0 ; still at eof
       else (let ((use-buffer (or buffer (slot-value s 'excl::buffer))))
              (let ((bytes-read (client-request-read-sequence use-buffer (creq-stream-creq s) 
                                                              :start (or start 0)
                                                              :end   
                                                              (if* (or (null end)
                                                                       (eql start end))
                                                                 then (length use-buffer)
                                                                 else end))))
                (if* (null buffer)
                   then (setf (sm excl::buffer-ptr s) bytes-read)
                        (setf (sm excl::buffpos s) 0))
      
                (if* (zerop bytes-read)
                   then (client-request-close (creq-stream-creq s))
                        (setf (creq-stream-at-eof s) t))
              
                bytes-read)))))
                

(defmethod close :before ((s creq-stream) &key abort)
  (declare (ignore abort))
  (if* (not (creq-stream-at-eof s))
     then (client-request-close (creq-stream-creq s))
          (setf (creq-stream-at-eof s) t)))
  
  

(defmethod device-file-position ((s creq-stream))
  nil)
          
  
