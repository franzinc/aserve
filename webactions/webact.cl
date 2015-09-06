;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; webaction.cl
;; framework for building dynamic web sites
;;
;; copyright (c) 2003-2015 Franz Inc, Oakland, CA - All rights reserved.
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

(defpackage :net.aserve
  (:export 
   #:initialize-websession-master
   #:locate-action-path
   #:webaction
   #:webaction-entity
   #:webaction-from-ent
   #:webaction-project
   #:websession
   #:websession-data
   #:websession-key
   #:websession-from-req
   #:websession-master
   #:websession-variable
   ))


(in-package :net.aserve)

(defclass webaction-entity (computed-entity access-file-mixin)
  ((webaction ;; holds webaction object
    :initarg :webaction
    :initform nil
    :accessor webaction-webaction)))


(defclass webaction ()
  ;; a request handled as a action
  ((name   :initarg name
	   :initform "unnamed"
	   :accessor webaction-name
	   )
   (project-prefix :initarg :project-prefix
		   :initform ""
		   :accessor webaction-project-prefix
		   )
   
   
   ; prefix of where regular files are found
   (destination :initarg :destination
		:initform ""
		:accessor webaction-destination)
   
   (clp-suffixes :initarg  :clp-suffixes
		 :initform '("clp")
		 :accessor webaction-clp-suffixes)
	       
   (map  :initarg :map
	 :initform nil
	 :accessor webaction-map)
   
   (hash :initform (make-hash-table :test #'equal)
	 :reader webaction-hash)

   ; list of actions triggered by a prefix
   (prefixes :initform nil
	     :accessor webaction-prefixes)
   
   (websession-master :initarg :websession-master
		      :initform nil
		      :accessor webaction-websession-master)
   
   (external-format :initarg :external-format
		    :accessor webaction-external-format)

   ; content-type for clp files
   (clp-content-type :accessor webaction-clp-content-type
		     :initform nil)
   
   ;; default actions when there is no map match
   (default-actions :initform nil
     :accessor webaction-default-actions)
   ;; use http-only cookies
   (use-http-only-cookies
    :initform nil :accessor webaction-use-http-only-cookies)
   ))

(defparameter *webactions-version* "1.14")
	      
(defvar *name-to-webaction* (make-hash-table :test #'equal))

(defparameter *session-reap-interval* (* 5 60)) ; 5 minutes

(defun webaction-project (name &key (project-prefix "/")
				    (clp-suffixes '("clp"))
				    map
				    (destination "")
				    index 
				    (server *wserver*)
				    host
				    session-lifetime
				    (sessions t)
				    reap-interval
				    reap-hook-function
				    access-file
				    authorizer
				    clp-content-type
				    (external-format
				     *default-aserve-external-format*)
				    (default-actions nil)
				    (use-http-only-cookies nil)
				    (timeout #+io-timeout #.(* 100 24 60 60)
					     #-io-timeout nil)
				    )
  ;; create a webaction project
  ;; and publish all prefixes
  ;;
  
  (if* (not (and (stringp project-prefix)
		 (> (length project-prefix) 0)
		 (eql #\/ (aref project-prefix (1- (length project-prefix))))))
     then (error "project-prefix should be a string ending in /, not ~s"
		 project-prefix))
  
  
  ; publish the webactions
  (let ((ent (publish-prefix :prefix project-prefix
			     :function 'webaction-entity
			     :class 'webaction-entity
			     :server server
			     :host host
			     :timeout timeout
			     :authorizer authorizer
			     ))
	(wa (or (gethash name *name-to-webaction*)
		(make-instance 'webaction))))

    (setf (directory-entity-access-file ent) access-file)
    
    (setf (webaction-name wa) name)
    (setf (webaction-project-prefix wa) project-prefix)
    (setf (webaction-map wa) map)
    (setf (webaction-clp-suffixes wa) clp-suffixes)
    (setf (webaction-destination wa) destination)
    (setf (webaction-external-format wa) external-format)
    (setf (webaction-clp-content-type wa) clp-content-type)
    (setf (webaction-default-actions wa) default-actions)
    (setf (webaction-use-http-only-cookies wa) use-http-only-cookies)
    
    (if* (and reap-interval (integerp reap-interval) (> reap-interval 0))
       then (setq *session-reap-interval* reap-interval))
    

    ; put stuff in the table
    (clrhash (webaction-hash wa))
    (let ((hash (webaction-hash wa)))
      (dolist (ent map)
	(let ((lastval (car (last ent))))
	  (if* (and (consp lastval) (getf lastval :prefix))
	     then ; this is a prefix entry, not a fixed entry
		  (push ent (webaction-prefixes wa))
	     else (setf (gethash (car ent) hash) (cdr ent))))))
    
    (setf (webaction-webaction ent) wa)
    
    ; store the webaction object here too so that
    ; webaction-from-req will work in action functions too
    (setf (getf (entity-plist ent) 'webaction) wa)
    
    (if* (and (null (webaction-websession-master wa))
	      sessions)
       then (initialize-websession-master
	     (setf (webaction-websession-master wa)
	       (make-instance 'websession-master
		 :cookie-name name
		 :reap-hook-function reap-hook-function
		 ))))
    
    (if* (null sessions)
       then ; no sessions for this project
	    (setf (webaction-websession-master wa) nil))
    
    (if* (and session-lifetime (webaction-websession-master wa))
       then (setf (sm-lifetime (webaction-websession-master wa))
	      session-lifetime))
		
    
    (setf (gethash name *name-to-webaction*) wa)

    (remove-old-webaction-pages wa server)

    ;; if we have an index page for the site, then redirect
    ;; the project-prefix to it
    (if* index
       then (publish :path project-prefix
		     :function #'(lambda (req ent)
				   (redirect-to req ent 
						(concatenate 
						    'string project-prefix
						    index)))
		     :authorizer authorizer
		     :server server
		     :host host)
	    (if* (> (length project-prefix) 1)
	       then ; also do it with the slash missing at the end
		    (publish :path (subseq project-prefix 0
					   (1- (length project-prefix)))
			     :function #'(lambda (req ent)
					   (redirect-to req ent 
							(concatenate 
							    'string project-prefix
							    index)))
			     :authorizer authorizer
			     :server server
			     :host host)))
	    
    
    ent))


(defun remove-old-webaction-pages (wa server)
  ;; unpublish all pages from the previous time this webaction-project
  ;; was defined
  
  (map-entities #'(lambda (ent)
		    
		    (if* (eq (getf (entity-plist ent) 'webaction) wa)
		       then :remove))
		    
		(find-locator :exact server)))

(defun redirect-to (req ent dest)
  ;; the http v1.1 spec says that 307 (temporary redirects) should only be done
  ;; silently for get's and head's.  For all else (e.g. post) 
  ;; the browser should ask if the user wants to do the redirect.
  ;; Thus we use the permantent redirect in that case
  (with-http-response (req ent
			   :response 
			   (if* (member (request-method req) '(:get :head))
			      then *response-temporary-redirect*
			      else *response-moved-permanently*))
    (setf (reply-header-slot-value req :location) dest)
    (with-http-body (req ent))))
  
;; the current websession is placed in the req object by
;; the action code that first gets the request.

(defun websession-from-req (req)
  (getf (request-reply-plist req) 'websession))

(defsetf websession-from-req .inv-websession-from-req)

(defun .inv-websession-from-req (req websession)
  (setf (getf (request-reply-plist req) 'websession) websession))



(defun webaction-from-ent (ent)
  (getf (entity-plist ent) 'webaction))



(defun webaction-entity (req ent
			 ;; use-actions is used in an internal recursive call
			 ;; when there are no action matches and we turn to
			 ;; the webaction's default-actions.
			 &key (use-actions nil))
  ;; handle a request in the uri-space of this project
  
  ; determine if it's in the action space, if so, find the action
  ; the map, run it, and then handle what it returns
  (let ((path (uri-path (request-uri req)))
	(wa (webaction-webaction ent))
	(newfollowing)
	(websession (websession-from-req req))
	(failed-following)
	(final-flags)
	(sm))
    
    
    ; look for session info based on cookie
    ; and remember it on the request

    
    (let (csessid)
      (if* (and (null websession)
		(setq sm (webaction-websession-master wa))
		(setq csessid
		  (cdr (assoc (sm-cookie-name sm)
			      (get-cookie-values req)
			      :test #'equal))))
	 then 
	      (if* (setq websession
		     (gethash csessid (sm-websessions sm)))
		 then (if* (eq :try-cookie (websession-method websession))
			 then (setf (websession-method websession) :cookie))
	       elseif (> (length csessid) 10) 
		 then ; no session found, but this session id looks good
		      ; and was probably created by another web server.
		      ; so create a session object
		      (setq websession (make-instance 'websession
					 :key csessid
					 :method :cookie))
		      (setf (gethash csessid (sm-websessions sm)) websession)))
      (if* websession 
	 then  (setf (websession-from-req req) websession)))
    
    

    #+ignore
    (if* websession
       then (format t "in action key  ~s data ~s~%"
		    (websession-key websession)
		    (websession-data websession)))
			 
    ;; strip sessionid off either of the possible prefixes
    (let* ((prefix (webaction-project-prefix wa))
	   (following (match-prefix
		       prefix
		       path)))
      
      (if* (and following (setq newfollowing
			    (strip-websessionid 
			     req wa following websession)))
	 then ; found session id
	      (modify-request-path req 
				   prefix
				   newfollowing)
	      (return-from webaction-entity 
		(handle-request req))))

      
    (if* (and (null websession) 
	      (or sm (setq sm (webaction-websession-master wa))))
				 
       then ; we haven't got a session yet for this session.
	    ; create one, and remeber it for this requst
	    (let ((key (next-websession-id sm)))
		
	      (setf (websession-from-req req)
		(setf (gethash key (sm-websessions sm))
		  (setq websession (make-instance 'websession
				     :key key
				     :method :try-cookie))))))
		
	      

    (if* websession then (note-websession-referenced websession))
    
    (let* ((following (match-prefix (webaction-project-prefix wa)
				    path))
	   (initial-following following))
      (if* following
	 then ; this is a call on a webaction and no session id
	      ; was found in the url
	      ; try to locate the session via a cookie
	      
	      (let* ((actions (or use-actions
				  (locate-actions req ent wa following)))
		     (redirect))
		
		; there may be a list of flags at the end of
		; the map entry
		(setq final-flags (let ((last (last actions)))
				    (if* (consp (car last))
				       then (car last))))
		
		
		(if* (and actions
			  (not (listp (car actions))))
		   then ; this isn't the case of an entry followed
			; right by flags
			(setq redirect (getf final-flags :redirect))
			  
			(loop
			  (if* (stringp (car actions))
			     then (if* redirect
				     then  ; redir so client will send
					  ; new request meaning we have to
					  ; pass the cookie value in
					  (modify-request-path 
					   req 
					   (webaction-project-prefix wa)
					   (locate-action-path
					    wa (car actions) websession))
					   
				     else (modify-request-path 
					   req 
					   (webaction-project-prefix wa)
					   (car actions)))
				  (return)
				  
			   elseif (symbolp (car actions))
			     then
				  (setq following (funcall (car actions) 
							   req ent)) 
				  #+ignore
				  (format t "following is ~s, actions is ~s~%" 
					  following actions)
				  (if* (null following)
				     then ; must have done html output
					  (return-from webaction-entity nil)
				   elseif (eq following :continue)
				     then (if* (null (cdr actions))
					     then (logmess (format 
							    nil
							    "action ~s return nil with no subsequent actions"
							    (car actions)))
						  (return-from webaction-entity
						    nil))
					  
					  (pop actions)
				   elseif (stringp following)
				     then (modify-request-path 
					   req (webaction-project-prefix wa)
					   (locate-action-path
					    wa following websession))
					  (return)
					  
				     else ; bogus ret from action fcn
					  (logmess (format nil "action function ~s returns illegal value: ~s"
							   (car actions)
							   following))
					  (return-from webaction-entity nil))
			     else (logmess (format nil
						   "reached end of map entries for ~s" 
						   initial-following))
				  (return-from webaction-entity nil)))
			
			; out of the procesing loop.  the request
			; has been modified and may now refer to
			; an already published file, so start through
			; the handle-request logic again to find
			; an existing entity before creating one.
			(return-from webaction-entity
			  (if* redirect
			     then (redirect-to req ent
					       (net.uri:uri-path
						(request-uri req)))
			     else (handle-request req)))
		   else (setq failed-following following)))))
    
    ; must be a file then..
    (multiple-value-bind (realname postfix)
	(compute-symname-as-filename req ent wa)
      (let ((info)
	    (forbidden))
      
	; this is like what publish-directory does now
	(if* (null realname)
	   then ; contains ../ or ..\  
		; ok, it could be valid, like foo../, but that's unlikely
		; Also on Windows don't allow \ since that's a directory sep
		; and user should be using / in http paths for that.
		(return-from webaction-entity
		  (failed-request req)))
      
	      
	(multiple-value-setq (info forbidden)
	  (read-access-files ent realname postfix))		
		
	(if* forbidden
	   then ; give up right away.
		(return-from webaction-entity (failed-request req)))
	
	(let ((type (excl::filesys-type realname)))
	  (if* (not (eq :file type))
	     then ;; No regular map entry for webaction.  If it has a default
		  ;; action, we try it here.
		  (if* use-actions
		     then ;; I don't think we can actually get here, but
			  ;; just in case...
			  (if* failed-following
			     then (logmess (format nil "~
no map for webaction with default-actions ~s"
						   failed-following)))
			  (return-from webaction-entity (failed-request req))
		     else (let ((default-actions
				    (webaction-default-actions wa)))
			    (if* default-actions
			       then ;; try again specifying actions
				    (return-from webaction-entity
				      (webaction-entity
				       req ent
				       :use-actions default-actions))
			       else (if* failed-following
				       then (logmess
					     (format 
					      nil "~a: no map for webaction ~s"
					      (socket:ipaddr-to-dotted
					       (socket:remote-host
						(request-socket req)))
					      failed-following)))
				    (return-from webaction-entity
				      (failed-request req))))))

	  (let ((new-ent (clp-directory-entity-publisher
			  req ent realname info
			  (webaction-clp-suffixes wa)
			  (webaction-external-format wa)
			  (or (getf final-flags :content-type)
			      (webaction-clp-content-type wa))
			  )))
	    ; put the webaction in the entity so it can be used
	    ; when the clp file (if this is clp entity) is used
	    (setf (getf (entity-plist new-ent) 'webaction)
	      (webaction-webaction ent))
	    (authorize-and-process req new-ent)))))))

(defun compute-symname-as-filename (req ent wa)
  ;; compute the filename that the symbolic name denotes.
  ;; return nil if the filename is illegal (since it contains
  ;; upward directory movement characters).
  (let* ((postfix (subseq (request-decoded-uri-path req) (length (prefix ent))))
	 (realname (concatenate 'string (webaction-destination wa) postfix)))
    (if* (or #+mswindows (position #\\ postfix) ; don't allow windows dir sep
	     (match-regexp "\\.\\.[\\/]" postfix))
       then ; contains ../ or ..\  
	    ; ok, it could be valid, like foo../, but that's unlikely
	    ; Also on Windows don't allow \ since that's a directory sep
	    ; and user should be using / in http paths for that.
	    (return-from compute-symname-as-filename nil))
    
    (if* sys:*tilde-expand-namestrings*
       then (setq realname (excl::tilde-expand-unix-namestring realname)))
    
    (values realname postfix)))




(defun strip-websessionid (req wa following websession)
  ;; strip leading session id if any
  ;; setup the current session on the request object
  ;; /prefix/~24234344234234242342342234~/realname/whatever
  ;;
  ;; return what follows the session id.  If no session id
  ;; as found, return nil
  ;;
  ;; we assume that before this function is called we check for
  ;; a cookie indicated session and if that found something then
  ;; websession is non-nil.
  ;;
  (let (pos sessid sm)
    (if* (and (> (length following) 0)
	      (eq #\~ (aref following 0))
	      (setq pos (position #\~ following :start 1))
	      (> (length following) (1+ pos))
	      (eql #\/ (aref following (1+ pos)))
	      )
       then (setq sessid (subseq following 1 pos)
		  following (subseq following (+ 2 pos)))
	    
	    (if* (null websession)
	       then ; cookie didn't work to locate a websession
		    ; it could be that it wasn't even tried though...
		    (setq sm (webaction-websession-master wa)
			  websession
			  (and sm (gethash sessid (sm-websessions sm))))
	  
		    ; possibilities
		    ;   session found 
		    ;     check mode.  if we're in try-cookie mode then
		    ;		     check to see if the cookie was passed
	  
		    (if* websession
		       then (setf (websession-from-req req) websession)
			    (case (websession-method websession)
			      (:try-cookie
			       ;; cookie didn't work so use url for now on
			       (setf (websession-method websession) 
				 :url)))
		     elseif sm
		       then ; add new session
			    (setf (websession-from-req req)
			      (setf (gethash sessid (sm-websessions sm))
				(make-instance 'websession 
				  :key sessid
				  :method :try-cookie)))
			    
			    ))
	  
	    following)))
  



(defun locate-actions (req ent wa action-name)
  ;; retrieve a list of actions for the symbolic page name
  ;;
  (or (gethash action-name (webaction-hash wa))
      ; only do prefixes if there's no existing filename
      ; mentioned
      (let ((realname (compute-symname-as-filename req ent wa)))
	(if* (and realname 
		  (probe-file realname))
	   then nil 	; means no actions
	   else ; look for prefixes
      
		(dolist (pp (webaction-prefixes wa))
		  (if* (and (consp pp)
			    (stringp (car pp))
			    (match-prefix (car pp) action-name))
		     then (return (cdr pp))))))))


(defun locate-action-path (wa action-name websession)
  ;; return the full uri path for what action points to
  ;; if the action points to a page, otherwise return a pointer
  ;; to the action itself
  ;;** change -- always return a pointer to the action since
  ;;  that will allow the project to be redefined and not have
  ;;  the clp files reparsed.
  (let* ((relative-path action-name)
	 (prefix (webaction-project-prefix wa)))
    
    (relative-to-absolute-path 
     (if* (and websession (member (websession-method websession) 
				  '(:try-cookie :url)))
	then ; add session id to url
	     (concatenate 'string
	       prefix
	       "~"
	       (websession-key websession)
	       "~/")
	else prefix)
     relative-path)))


(defun relative-to-absolute-path (prefix relative-path)
  ;; add on the project prefix so that the resulting path
  ;; is reachable via a browser
  (if* (or (zerop (length relative-path))
	   (not (eq #\/ (aref relative-path 0))))
     then ; relative path
	  (concatenate 'string prefix relative-path)
     else relative-path))


(defun match-prefix (prefix full)
  ;; if prefix is a  prefix of full then return what follows
  ;; prefix
  (and (<= (length prefix) (length full))
       (dotimes (i (length prefix)
		  (subseq full (length prefix)))
	 (if* (not (eq (aref prefix i) (aref full i))) then (return nil)))))
	     
  
  
(defun modify-request-path (req prefix newpath)
  ;; modify the http request with the new path
  ;; the new path can be relative or absolute
  (setq newpath 
    (relative-to-absolute-path prefix
			       newpath))
  
  (setf (request-decoded-uri-path req) newpath)
  (setf (request-uri req)
    (net.uri:copy-uri (request-uri req)
		      :path newpath))
  
  (setf (request-raw-uri req)
    (net.uri:copy-uri (request-raw-uri req)
		      :path newpath)))



(defun webaction-cleanup-process ()
  ;; clean up all old sessions in all active webactions
  (loop 
    ;;(format t "~%Reap Check~%")(force-output)
    (maphash #'(lambda (name webaction)
		 (declare (ignore name))
	       (let ((websession-master (webaction-websession-master
					 webaction)))
		 (if* websession-master
		    then (reap-unused-sessions websession-master))))
	     *name-to-webaction*)
    
    (sleep *session-reap-interval*)))

(defvar *webaction-cleanup-lock* (mp:make-process-lock))

(defvar *webaction-cleanup-process* nil)

(defun ensure-webaction-cleanup-process ()
  (mp:with-process-lock (*webaction-cleanup-lock*)
    (if* (not (and *webaction-cleanup-process*
		   (mp:process-active-p *webaction-cleanup-process*)))
       then ; must restart it
	    (setq *webaction-cleanup-process*
	      (mp:process-run-function "session reaper" 
		'webaction-cleanup-process))
	    (setf (mp:process-keeps-lisp-alive-p *webaction-cleanup-process*) nil)
	    
	    *webaction-cleanup-process*
	    )))


  

	       
	       
