;; -*- mode: common-lisp; package: net.iserve -*-
;;
;; authorize.cl
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
;; $Id: authorize.cl,v 1.1.2.1 2000/03/14 23:13:23 jkf Exp $

;; Description:
;;   classes and functions for authorizing access to entities

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.iserve)

(defclass authorizer ()
  ;; denotes information on authorizing access to an entity
  ;; this is meant to be subclassed with the appropriate slots
  ;; for the type of authorization to be done
  ())



;; - password authorization.
;;
(defclass password-authorizer (authorizer)
  ((allowed :accessor password-authorizer-allowed
	    ;; list of conses (name . password)
	    ;; which are valid name, password pairs for this entity
	    :initarg :allowed
	    :initform nil)
   (realm  :accessor password-authorizer-realm
	   :initarg :realm
	   :initform "Allegro iServe")
   ))


(defmethod authorize ((auth password-authorizer) 
		  (req http-request)
		  (ent entity))
  ;; check if this is valid request, return t if ok
  ;; and :done if we've sent a request for a  new name and password
  ;;
  (multiple-value-bind (name password) (get-basic-authorization req)
    
    (if*  name
       then (dolist (pair (password-authorizer-allowed auth))
	      (if* (and (equal (car pair) name)
			(equal (cdr pair) password))
		 then (return-from authorize t))))

    ;; valid name/password not given, ask for it 
    (with-http-response (req ent :response 
			     *response-unauthorized*)
      (set-basic-authorization req
			       (password-authorizer-realm auth))
      (with-http-body (req ent)))
    :done))
	    
	    
  


;; location authorization
;; we allow access based on where the request is made from.
;; the pattern list is a list of items to match against the
;; ip address of the request.  When the first match is made the
;; request is either accepted or denied.
;;
;; the possible items in the list of patterns
;;      :accept   	accept immediately
;;	:deny		deny immediately
;;	(:accept ipaddress [bits])   accept if left 'bits' of the
;;			ipaddress match
;;	(:deny ipaddress [bits])     deny if the left 'bits' of the 
;;			ipaddress match
;;
;;	bits defaults to 32
;;	the ipaddress can be an
;;		integer -  the 32 bit ip address
;;		string
;;		  "127.0.0.1"  - the dotted notation for an ip address
;;		  "foo.bar.com" - the name of a machine
;;	 when the ipaddress is a string it is converted to an integer
;;	 the first time it is examined.
;;	 When the string is a machine name then the conversion may or
;;	 may not work due to the need to access a nameserver to do
;;	 the lookup.
;;
;;
;;


(defclass location-authorizer (authorizer)
  ((patterns :accessor location-authorizer-patterns
	     ;; list of patterns to match
	     :initarg :patterns
	     :initform nil)))




(defmethod authorize ((auth location-authorizer)
		      (req http-request)
		      (ent entity))
  (let ((request-ipaddress (socket:remote-host (request-socket req))))
    (dolist (pattern (location-authorizer-patterns auth))
      (if* (atom pattern)
	 then (case pattern
		(:accept (return-from authorize t))
		(:deny   (return-from authorize nil))
		(t (warn "bogus authorization pattern: ~s" pattern)
		   (return-from authorize nil)))
	 else (let ((decision (car pattern))
		    (ipaddress (cadr pattern))
		    (bits (if* (cddr pattern)
			     then (caddr pattern)
			     else 32)))
		(if* (not (member decision '(:accept :deny)))
		   then (warn "bogus authorization pattern: ~s" pattern)
			(return-from authorize nil))
		
		(if* (stringp ipaddress)
		   then ; check for dotted ip address first
			(let ((newaddr (socket:dotted-to-ipaddr ipaddress
								:errorp nil)))
			  (if* (null newaddr)
			     then ; success!
				  (ignore-errors
				   (setq newaddr (socket:lookup-hostname ipaddress))))
			  
			  (if* newaddr
			     then (setf (cadr pattern) 
				    (setq ipaddress newaddr))
			     else ; can't compute the address
				  ; so we'll not accept and we will deny
				  ; just to be safe
				  (warn "can't resolve host name ~s" ipaddress)
				  (return-from authorize nil))))
		
		
		(if* (not (and (integerp bits) (<= 1 bits 32)))
		   then (warn "bogus authorization pattern: ~s" pattern)
			(return-from authorize nil))
		
		; now we're finally ready to test things
		(let ((mask (if* (eql bits 32) 
			       then -1
			       else (ash -1 (- 32 bits)))))
		  (if* (eql (logand request-ipaddress mask)
			    (logand ipaddress mask))
		     then ; matched, 
			  (case decision
			    (:accept (return-from authorize t))
			    (:deny   (return-from authorize nil))))))))
    
    t ; the default is to accept
    ))

		
	      
			  
			  
			  
			  
				  
		
		
			
			
		
			 
