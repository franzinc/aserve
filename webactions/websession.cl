;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; websession.cl
;; session support for webactions
;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

(defclass websession-master ()
  ;; describes how a set of sessions is managed
  ((prefix :initarg :prefix
	   ;; string that preceeds all keys
	   :initform ""
	   :accessor sm-prefix)

   (suffix  :initarg :suffix
	    ;; number against which the counter will be xored
	    :initform ""
	    :accessor sm-suffix)
   
   (counter :initarg :counter
	    :initform nil
	    :accessor sm-counter)
   
   ;; how long a session will last if no reference made to it
   (lifetime :initarg :lifetime
	    :accessor sm-lifetime
	    :initform #.(* 5 60 60) ; five hours
	    )
   
   (reap-hook-function  :initarg :reap-hook-function
			:accessor sm-reap-hook-function
			:initform nil)
   
   (cookie-name :initarg :cookie-name
		:initform "webaction"
		:reader sm-cookie-name)
   
   (websessions  :initform (make-hash-table :test #'equal)
		 :reader sm-websessions)))

;; Mention class in make-instance after class def to avoid bug24329.
(defun make-instance-websession-master+cookie-name+reap-hook-function  (name reap-hook-function)
  (make-instance 'websession-master :cookie-name name :reap-hook-function reap-hook-function))



(defclass websession ()
  ;; individual sessions
  (
   (key  :initarg :key
	 ;; the session key
	 :reader  websession-key)
   
   (lastref  :initform (excl::cl-internal-real-time)
	     :accessor websession-lastref)
   
   ;; how we pass the session information
   ;; :try-cookie - send via cookie and url rewiting
   ;; :do-cookie - send via cookie, never do url
   ;; :cookie - cookie received, so no need to send it any more
   ;; :url  - send session id in url	
   ;;
   ;; normally the method changes from
   ;;  :try-cookie -> :cookie
   ;; or
   ;;  :try-cookie -> :url
   ;; but if :session-cookie-only is set then it's
   ;;  :do-cookie -> :cookie  (stay at :do-cookie if cookie not returned)
   (method :initarg :method
	   :initform nil
	   :accessor websession-method)
   
   ; a place for users to hang information onto the session object.
   (data   :initarg :data
	   :initform nil
	   :accessor websession-data)
   
   (variables :initform nil
	      :accessor websession-variables)))

;; Mention class in make-instance after class def to avoid bug24329.
(defun make-instance-websession+key+method (key method)
  (make-instance 'websession :key key :method method))



(defmethod initialize-websession-master ((sm websession-master))
  ;; we no longer do this here.. we wait until we start to use
  ;; the keys that way a saved image will get new info when
  ;; it starts
  nil
  
  )

(defun compute-prefix-suffix (sm)
  ;; compute the prefix string and suffix value

  ;; randomize the random number generator
  (dotimes (i (logand (get-universal-time) #xfff)) (random 256))
  
  (let ((val 1))
    (dotimes (i 4)
      (setq val (+ (ash val 8) (random 255))))
    (setf (sm-prefix sm) (format nil "~x" val))

    (setq val 0)
    (dotimes (i 4)
      (setq val (+ (ash val 8) (random 255))))
    (setf (sm-suffix sm) val))
)



(defvar *websession-counter-lock* (mp:make-process-lock))

(defmethod next-websession-id ((sm websession-master))
  (mp:with-process-lock (*websession-counter-lock*)
    
    (let ((counterval (sm-counter sm)))
      
      (if* (null counterval)
	 then (compute-prefix-suffix sm)
	      (setq counterval (random 255)))
      
      (setf (sm-counter sm) (1+ counterval))
		  
      (concatenate 'string (sm-prefix sm)
		   (format nil "~x" (random #xfffffff))
		   (format nil "~x" (logxor (sm-suffix sm) counterval))))))

    
  

(defvar *verify-reaper-started* 0)

(defmethod note-websession-referenced ((sess websession))
  (setf (websession-lastref sess) (excl::cl-internal-real-time))
  
  ; make sure we've got the reaper process running, but don't
  ; check too often since it's not necessary
  (if* (< (decf *verify-reaper-started*) 0)
     then (setq *verify-reaper-started* 30)
	  (ensure-webaction-cleanup-process)))



(defun websession-variable (websession name)
  (and websession
       (cdr (assoc name (websession-variables websession) :test #'equal))))

(defsetf  websession-variable .inv-websession-variable)

(defun .inv-websession-variable (websession name newvalue)
  (if* (null websession)
     then ; do nothing since there is no session
	  newvalue
     else 
	  (let ((ent (assoc name (websession-variables websession) 
			    :test #'equal)))
	    (if* ent
	       then (setf (cdr ent) newvalue)
	       else (setq ent (cons name newvalue))
		    (push ent (websession-variables websession)))
	    newvalue)))


      
(defun reap-unused-sessions (sm)
  (let ((now (excl::cl-internal-real-time))
	(lifetime (sm-lifetime sm))
	(reap-fcn (sm-reap-hook-function sm))
	(toreap))
    (maphash #'(lambda (id websession)
		 (declare (ignore id))
		 (if* (> now
			 (+ (websession-lastref websession) lifetime))
		    then (if* (and reap-fcn
				   (funcall reap-fcn websession))
			    then ; keep around this session longer
				 (setf (websession-lastref websession) now)
			    else (push websession toreap))))
	     (sm-websessions sm))
  
    (dolist (websession toreap)
      (debug-format :info " flush session ~s" (websession-key websession))
      (force-output)
      (remhash (websession-key websession) (sm-websessions sm)))))

  
  
  
  
(defun kill-websession (req ent)
  ;; remove the session associated with this req/ent
  ;; (untested)
  (let* ((wa (webaction-from-ent ent))
	 (sm (and wa (webaction-websession-master wa)))
	 (sessions (and sm (sm-websessions sm)))
	 (session (websession-from-req req)))
    
    (if* (and sessions session)
       then (remhash (websession-key session) sessions))))


	 
	 
    
	
    
    
   
