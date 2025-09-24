;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; macs.cl
;;
;; See the file LICENSE for the full license governing this code.
;;

;;

;; Description:
;;   useful internal macros

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



;; macros used by iserve


(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

;; add features based on the capabilities of the host lisp
#+(version>= 6 1) (pushnew :io-timeout *features*) ; support i/o timeouts

;; Note for people using this code in non-Allegro lisps.
;;
;; The if* macro used in this code can be found at:
;;
;; http://www.franz.com/~jkf/ifstar.txt
;;


;; macros to speed up some common character operations
(defmacro find-it (ch buff start end)
  ;; return position of ch in buff from [start end}
  ;;
  (let ((pos (gensym)))
    `(do ((,pos ,start (1+ ,pos)))
	 ((>= ,pos ,end))
       (if* (eq (schar ,buff ,pos) ,ch)
	  then (return ,pos)))))

(defmacro find-it-rev (ch buff start end)
  ;; return position of ch in buff from [start end}
  ;; searching backwards
  ;;
  (let ((pos (gensym)))
    `(do ((,pos (1- ,end) (1- ,pos)))
	 ((< ,pos ,start))
       (if* (eq (schar ,buff ,pos) ,ch)
	  then (return ,pos)))))

(defmacro buffer-substr (buff start end)
  ;; return a string holding the chars in buff from [start end }
  ;;
  (let ((res (gensym))
	(i (gensym))
	(pos (gensym)))
    `(let ((,res (make-string (- ,end ,start))))
       (do ((,i 0 (1+ ,i))
	    (,pos ,start (1+ ,pos)))
	   ((>= ,pos ,end))
	 (setf (schar ,res ,i) (schar ,buff ,pos)))
       ,res)))

(defmacro buffer-match (buff start str)
  ;; return t if the buffer buff contains the same string as str
  (let ((pos (gensym))
	(i (gensym))
	(len (gensym)))
	   
    `(do ((,pos ,start (1+ ,pos))
	  (,i 0 (1+ ,i))
	  (,len (length ,str)))
	 ((>= ,i ,len) t)
       (if* (not (eq (schar ,buff ,pos) (schar ,str ,i)))
	  then (return nil)))))

(defmacro buffer-match-ci (buff start str)
  ;; return t if the buffer buff contains the same string as str
  ;; case insensitive version where str contains each char doubled
  (let ((pos (gensym))
	(i (gensym))
	(len (gensym))
	(xchar (gensym)))
	   
    `(do ((,pos ,start (1+ ,pos))
	  (,i 0 (+ 2 ,i))
	  (,len (length ,str)))
	 ((>= ,i ,len) t)
       (let ((,xchar (schar ,buff ,pos)))
	 (if* (not (or (eq ,xchar (schar ,str ,i))
		       (eq ,xchar (schar ,str (1+ ,i)))))
	    then (return nil))))))


(defmacro rational-read-sequence (&rest args)
  ;; acl's read-sequence was changed to conform to the
  ;; bogus ansi definition where the whole buffer was filled up.
  ;; even for socket stream. 
  ;; rational-read-sequence does read-sequence the right way.
  #-(version>= 6 0 pre-final 9) 
  `(read-sequence ,@args)
  #+(version>= 6 0 pre-final 9) 
  `(read-sequence ,@args :partial-fill t))




;;;; response macros


;----  unsigned byte 8 array macros:

(defmacro ausb8 (vec index)
  ; like aref but it declares the type
  `(aref (the (simple-array (unsigned-byte 8) 1) ,vec) ,index))

(defmacro copy-usb8 (from-vector from-start
		     to-vector   to-start
		     count)
  ;; copy count bytes from from-vector[start] to to-vector[start].
  ;; vectors are usb8
  (let ((from (gensym))
	(to   (gensym))
	(i (gensym)))


    `(do ((,from ,from-start (1+ ,from))
	  (,to   ,to-start   (1+ ,to))
	  (,i ,count (1- ,i)))
	 ((<= ,i 0))
       (setf (ausb8 ,to-vector ,to)
	 (ausb8 ,from-vector ,from)))))


(defmacro copy-usb8-up (from-vector from-start
			to-vector   to-start
			count)
  ;; copy count bytes from from-vector[start] to to-vector[start],
  ;; going from the top down.  this is designed to be used if we are
  ;; copying upward in place so we have to copy from the top down
  ;;
  ;; vectors are usb8
  (let ((from (gensym))
	(to   (gensym))
	(i    (gensym)))


    `(do* ((,i ,count (1- ,i))
	   (,from (+ ,from-start ,i -1) (1- ,from))
	   (,to   (+ ,to-start   ,i -1) (1- ,to)))
	 ((<= ,i 0))
       (setf (ausb8 ,to-vector ,to)
	 (ausb8 ,from-vector ,from)))))


;-------------




(defmacro dlogmess (&rest args)
  ;; for now we just disable completely the log messages.
  ;; in the future we'll turn on and off the log messages 
  ;; at runtime with a switch.
  (declare (ignore args))
  nil)


; with-timeout-local: use with-timeout if that all we've got
; else use read-write timeouts
; 
(defmacro with-timeout-local ((time &rest actions) &body body)
  (declare (ignore time))
  (let ((g-blocktag (gensym)))
    `(block ,g-blocktag
       (handler-bind ((socket-error 
		       #'(lambda (c)
			   (if* (member (stream-error-identifier c) 
					'(:read-timeout :write-timeout)
					:test #'eq)
			      then ; must handle this
				   (return-from ,g-blocktag
				     (progn ,@actions))))))
	 ,@body))))

;;;;;;;;;;;;;; SMP-aware macros
;;
;; We cater to three slightly different states:
;;
;;  #+smp
;;     (smp-case (t form) ...)
;;     The compile and execute environments support SMP;
;;     9.0 with smp.
;;
;;  #+(and smp-macros (not smp))
;;     (smp-case (:macros form) ...)
;;     Compile environment recognizes the SMP macros, but compiles
;;     for non-SMP lisp;
;;     >=8.2 but not smp
;;     8.1 with smp patches.
;;
;;  #-smp-macros  ;; never done anymore
;;     (smp-case (nil form) ...)
;;     Compile environment does not recognize SMP macros;
;;     8.1 without smp patches.
;;
;;  In 12.0, a new with-style-case macro is introduced (and has been
;; back-ported to 10.1 and 11.0).  Furthermore, smp-macros is always
;; loaded in (since 10.0) so the nil case need never be considered,
;; unless the :macros case is not also in the same clause as the t case.

(defmacro smp-case (&rest clauses)
  (let* (t-clause nil-clause macros-clause)	 
    (dolist (c clauses)
      (unless (and (consp c)
		   (consp (cdr c))
		   (null (cddr c)))
	(error "smp-case clause ~s is badly formed" c))
      (let ((c-key (car c)))
	(when (or (eq c-key t)
		  (and (consp c-key) (member t c-key)))
	  (setq t-clause c))
	(when (or (eq c-key :macros)
		  (and (consp c-key) (member :macros c-key)))
	  (setq macros-clause c))
	(when (or (null c-key)
		  (and (consp c-key) (member nil c-key)))
	  (setq nil-clause c))))
    (if* (eq t-clause macros-clause)
       then ;; Only one clause needed
	    (second t-clause)
       else `(with-style-case :mp
	       (:vmp ,(or (second macros-clause) (second nil-clause)))
	       (:smp ,(second t-clause)))))) 

;; In 12.0, fasl files compiled by smp and non-smp lisps are
;; completely compatible if they have appropriate with-style-case
;; forms where differences matter.  Since smp-macros are always
;; loaded now, a (smp-case ((t :macros) <form1>) (nil <form2>))
;; will never select form2, so there need not even be any with-style-case
;; macro - the t/:macros clause can be the only expansion without
;; a style macro.
;;
;; Note that check-smp-consistency is not even defined in 12.0 but is
;; still needed in pre-12.0 lisps because the fasl files are not fat.
#-(version>= 12 0)
(defmacro check-smp-consistency ()
  (smp-case
   (nil (if* (featurep :smp)
	   then (error "This file was compiled to run in a non-smp acl")))
   (:macros (if* (featurep :smp)
	       then (error "This file was compiled to run in a non-smp acl")
	     elseif (not (featurep :smp-macros))
	       then (error "This file requires the smp-macros patch")))
   (t (if* (not (featurep :smp))
	 then (error "This file was compiled to run in an smp acl")))))


(defmacro atomic-incf (var)
  `(with-style-case :mp
     (:smp (incf-atomic ,var))
     (:vmp (with-locked-object (nil :non-smp :without-scheduling) (incf ,var)))))

(defmacro atomic-decf (var)
  `(with-style-case :mp
     (:smp (decf-atomic ,var))
     (:vmp (with-locked-object (nil :non-smp :without-scheduling) (decf ,var)))))

(defmacro with-locked-server ((s) &body body)
  `(with-locked-object (,s :non-smp :without-scheduling) ,@body))

(defmacro defvar-mp (v &rest rest)
  `(defvar-nonbindable ,v ,@rest))

(defmacro atomically-fast (&body body)
  `(excl::.atomically (excl::fast ,@body)))


(defmacro atomic-setf-max (place val)
  (let ((newvar (gensym)) (oldvar (gensym)))
    `(let ((,newvar ,val) ,oldvar)
       (loop
	 (setq ,oldvar  ,place)
	 (cond ((not (< ,oldvar ,newvar)) (return nil))
	       ((atomic-conditional-setf ,place ,newvar ,oldvar)
		(return t)))))))


;;;;;; end of smp-aware macro definitions
			 

(define-condition allegroserve-error (error)
  (;; what was being attempted with the error occured
   (action :initarg :action :reader allegroserve-error-action
	   :initform "unspecified")
   
   ;; describing the result that is considered an error
   (result :initarg :result :reader allegroserve-error-result
	   :initform "unspecified")
   
   ; a keyword unique to each error
   (identifier :initarg :identifier :reader allegroserve-error-identifier
	       :initform nil))
  (:report
   (lambda (con stream)
     (with-slots (action result identifier) con
       (format stream
	       "~@<~a resulted in error ~s :  ~a.~:@>"
	       action
	       identifier
	       result)))))
   
