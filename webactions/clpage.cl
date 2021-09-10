;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; clpage.cl
;; common lisp server pages
;;
;; See the file LICENSE for the full license governing this code.

#+(version= 10 1)
(sys:defpatch "webactions" 4
  "v4: 1.21 clp_include can take arguments
v3: 1.20 specify http return code, more clp transforms
v2: 1.18: can use cookies only for sessions; 
v1: 1.17: don't create session for non-existant urls, get session from ent;"
  :type :system
  :post-loadable t)

#+(version= 10 0)
(sys:defpatch "webactions" 5
  "v5: 1.18: can use cookies only for sessions; 
v4: 1.17: don't create session for non-existant urls, get session from ent;
v3: 1.16: fix misspelled initarg to websession-master;
v2: 1.15: update parsed time after parsing is finished;
v1: 1.13: cosmetic: bump version #; code same as 10.0 initial release."
  :type :system
  :post-loadable t)





(eval-when (compile load eval) (require :aserve))

(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

(defpackage :net.aserve
  (:export #:clp-directory-entity-processor
	   #:clp-entity
	   #:def-clp-function
	   #:emit-clp-entity
	   #:find-clp-module
	   #:find-clp-module-function
	   #:publish-clp
	   ))


(defclass clp-entity (entity)
  ;; a clp file
  ((file :initarg :file :accessor file)
   (file-write-date :initarg :file-write-date
		    :accessor clp-entity-file-write-date
		    :initform nil)
   (objects  :initarg :objects
	     :accessor clp-entity-objects)
   
   ; list of (filename . last-write-date) for included files
   (dependencies :initform nil
		 :accessor clp-entity-dependencies)
   
   (external-format  :initform nil
		     :initarg :external-format
		     :accessor clp-entity-external-format)
   
   ;; if non-nil user has specified the response code for 
   ;; this clp file with <clp_response code="433"/>
   ;; response code can be an integer meaning that exact value
   ;; or a string meaning the value of that session variable
   (response-code    :initform nil
                     :accessor clp-entity-response-code)
   ))



;; returned by a parse when the tag is :clp
(defstruct (pobj (:type list))
  tag
  module
  name
  args
  body)

(defvar *include-args* nil)


(defun publish-clp (&key (host nil host-p) port path class
			 (server *wserver*)
			 locator
			 remove
			 authorizer
			 filename
			 (timeout #+io-timeout #.(* 100 24 60 60)
				  #-io-timeout nil)
			 (content-type "text/html")
			 plist
			 external-format
			 )
  
  ;; publish a templated page, one that is created by a combination
  ;; of an html file and a lisp file
  
  (if* (null locator) 
     then (setq locator (find-locator :exact server)))
  
  (if* remove
     then (unpublish-entity locator path
			    host
			    host-p)
	  (return-from publish-clp nil))
  

  (let* ((hval (convert-to-vhosts (if* (and host (atom host))
				     then (list host)
				     else host)
				  server))
	 (ent (make-instance (or class 'clp-entity)
		:host hval
		:port port
		:path path
		:authorizer authorizer
		:file filename
		:content-type content-type
		:timeout timeout
		:plist plist
		:external-format external-format
		)))
    (publish-entity ent locator path hval)
    
    ent)
  
  )


(defun clp-directory-entity-publisher (req ent realname info suffixes
				       external-format clp-content-type)
  ;; publish all files ending in .clp as clp files
  ;; otherwise do the standard-directory-entity-publisher thing
  ;;
  (multiple-value-bind (root tail name type)
      (split-namestring realname)
    (declare (ignore root tail name))
    (if* (member type suffixes :test #'equalp)
       then 

	    (multiple-value-bind (content-type local-authorizer)
		(standard-access-file-reader realname info)

	      (publish-clp :path (request-decoded-uri-path req)
			   :host (host ent)
			   :filename realname
			   :authorizer (or local-authorizer
					   (entity-authorizer ent))
			   :content-type (or content-type 
					     clp-content-type
					     "text/html")
			   :timeout (entity-timeout ent)
			   :plist (list :parent ent) ; who spawned us
			   :external-format external-format
			   ))
       else
	    (standard-directory-entity-publisher req ent realname info))))
	    
    

	     





(defmethod process-entity ((req http-request) (ent clp-entity))
  ;; emit a clp file
  ;;
  (let ((websession (getf (request-reply-plist req) 'websession))
	(wa)
	(sm))
    
    (if* (and (null websession) 
	      (setq wa (getf (entity-plist ent) 'webaction))
	      (setq sm (webaction-websession-master wa)))
       then ; try to find session via cookie and if that fails,
            ; make up a session
	    (let ((csessid (cdr (assoc (sm-cookie-name sm)
				       
				       (get-cookie-values req)
				       :test #'equal))))
	      (if* (setq websession
		     (gethash csessid (sm-websessions sm)))
		 then ; cookie worked
		      (if* (eq :try-cookie (websession-method websession))
			 then (setf (websession-method websession)
				:cookie))
		 else ; no session found via cookie
		      (setq websession (make-instance-websession+key+method (next-websession-id sm) :cookie))
		      (setf (gethash (websession-key websession)
				     (sm-websessions sm))
			websession)))
	    
	    (if* websession
	       then 
		    (setf (getf (request-reply-plist req) 'websession)
		      websession)))
	    

    (if* websession then (note-websession-referenced websession))
	      
    (if* (or (null (clp-entity-file-write-date ent))
	     (not (eql (clp-entity-file-write-date ent)
		       (file-write-date (file ent))))
	     (dolist (dep (clp-entity-dependencies ent))
	       (if* (not (eql (cdr dep)
			      (file-write-date (car dep))))
		  then (return t)))
		   
		   
	     )
       then (parse-clp-file ent))
  
    (with-http-response (req ent 
                         :content-type (content-type ent)
                         :response (if* (clp-entity-response-code ent)
                                      then (code-to-response 
                                            (convert-response-code
                                             websession
                                             (clp-entity-response-code ent)))
                                      else *response-ok*))
      
      (insert-cookie req ent sm wa websession)
      
      (setf (reply-header-slot-value req :cache-control) "no-cache")
      (setf (reply-header-slot-value req :pragma) "no-cache")
      
      (with-http-body (req ent
                       :external-format 
                       (or (clp-entity-external-format ent)
                           *default-aserve-external-format*)
                       )
	(emit-clp-entity req ent (clp-entity-objects ent))))
    t))

(defun insert-cookie (req ent sm wa websession)
  ;; set our cookie if needed
  (if* (and websession
            (member (websession-method websession) '(:try-cookie :do-cookie))
            (or sm  ; sm already known, otherwise compute it
                (and (setq wa (getf (entity-plist ent) 'webaction))
                     (setq sm (webaction-websession-master wa)))))
     then (set-cookie-header
           req 
           :name (sm-cookie-name sm)
           :value (websession-key websession)
           :path (webaction-project-prefix wa)
           :http-only (webaction-use-http-only-cookies wa))))

(defun convert-response-code (websession code)
  ;; if code is a string lookup the value of that
  ;; string as a websession variable, else return the code
  ;; 
  ;; code can be 
  ;;  an integer  123
  ;;  an integer written to a string   "123"
  ;;  a variable name "foo"
  ;;    the value of a variable can be   123 or "123"  
  ;;
  (flet ((string-to-integer (string)
           ;; convert a string to an integer if possible
           ;; if not just return the string
           (or (ignore-errors (parse-integer string))
               string)))
    (if* (stringp code)
       then (if* (fixnump (setq code (string-to-integer code)))
               thenret ; we have the answer
               else (setq code 
                      (block nil
                        (let ((var-value (websession-variable websession code)))
                          (if* (stringp var-value)
                             then (if* (fixnump
                                        (setq var-value
                                          (string-to-integer var-value)))
                                     then (return var-value))
                           elseif (fixnump var-value)
                             then (return var-value))

                          ;; fall through, can't calcuate a fixnum value
                          (error "clp_response code ~s has variable value ~s that is not an integer"
                                 code var-value)))))
     elseif (fixnump code)
       thenret
       else (error "clp_response code is ~s which is an illegal value"
                   code)))
  code)
                            
                            
                            
                            
                                    
                            
                            
  

(defun parse-clp-file (ent)
  ;; parse the clp file
  ;;

  (let (*include-args*)
    (handler-case 
        (with-open-file (p (file ent)
                         :direction :input
                         :external-format (clp-entity-external-format ent))
	
          (multiple-value-bind (objects response-code) 
              (parse-clp-guts p (file ent))
            (let* (
                   (dependencies (expand-clp-includes objects (file ent)
                                                      (clp-entity-external-format ent))))
              (if* dependencies 
                 then (setf (clp-entity-dependencies ent)
                        (mapcar #'(lambda (filename)
                                    (cons filename (file-write-date filename)))
                                dependencies)))
              (setf (clp-entity-objects ent) objects)
	  
              (setf (clp-entity-file-write-date ent)
                (file-write-date (file ent)))
            
              (setf (clp-entity-response-code ent) response-code)
	  
              objects
              )))
      
      (error (c)
        (logmess (format nil "processing clp file ~s got error ~a"
                         (file ent)
                         c))))))



(defun parse-clp-filename (filename external-format)
  ;; parse the clp file
  ;;
    
  (handler-case 
      (with-open-file (p filename 
		       :direction :input
		       :external-format external-format)
	(parse-clp-guts p filename))
      
    (error (c)
      (logmess (format nil "processing clp file ~s got error ~a"
		       filename
		       c)))))


(defun expand-clp-includes (objects filename 
			    &optional (external-format *default-aserve-external-format*))
  ;; expand all the includes in the file
  ;; destructively modify the objects list to replace the 
  ;; include directives with what they include
  (do ((oo objects (cdr oo))
       (deps)
       (obj)
       (fname)
       )
      ((null oo)
       deps)
    
    (setq obj (car oo))
    
    (if* (and (consp obj)
	      (eq :clp (pobj-tag obj))
	      (equal "clp" (pobj-module obj))
	      (equal "include" (pobj-name obj))
	      (setq fname (cdr (assoc "name" (pobj-args obj) :test #'equal))))
       then (let ((newname (merge-pathnames fname filename)))
	      (pushnew newname deps :test #'equal)
	      
	      (let* ((*include-args* (append (pobj-args obj) *include-args*))
                     (newobjs (parse-clp-filename newname external-format))
		     (newdeps (expand-clp-includes newobjs newname external-format)))
		(dolist (dep newdeps)
		  (pushnew dep deps :test #'equal))
		  
		(if* newobjs
		   then ; splice it in in place of the clp
			(setf (cdr (last newobjs)) (cdr oo)
			      (cdr oo) (cdr newobjs)
			      (car oo) (car newobjs))))))
    
    ; expand the body (if any)
    (dolist (subdep (expand-clp-includes (pobj-body obj) filename external-format))
      (pushnew subdep deps :test #'equal))))

			
			
		
		  


(defun tparse (filename)
  (with-open-file (p filename)
    (let ((res (parse-clp-guts p filename)))
      (expand-clp-includes res filename)
      res)))





(defun parse-clp-guts (p filename)
  ;; p is a stream we'll read and parse
  ;; filename is the name of the file we're reading.
  ;; 
  ;; return two values:
  ;;   list of objects found in the file.  
  ;;     objects are (:text "string") or (:clp ..) for clp directives
  ;;   response-code-specified by <clp_response code="423">
  (let ((result)
        response-code
	(pos-start 0)
	(chstart 0)
	(chcount 0)
	(ch)
	(res)
	(lasttag)
	(backbuffer (make-array 10 :element-type 'character
				:initial-element #\space))
	(backindex 0))

    (flet ((savestring (p pos chars)
	     (if* (> chars 0)
		then (let ((ans (make-array chars
					    :element-type 'character))
			   (savepos (file-position p)))
		       (file-position p pos)
		       (dotimes (i chars)
			 (setf (aref ans i) (read-char p)))
		       (file-position p savepos)
		       
		       (push `(:text ,ans) result))))
	   (wa-cvt (dest)
             ;; convert dest to the appropriate wa_link command
             ;;
	     (let ((xpos (min (or (position #\? dest) most-positive-fixnum)
			      (or (position #\# dest) most-positive-fixnum)))
		   (extra))
	       (if* (< xpos most-positive-fixnum)
		  then ; tag as ? or # in it, remove that and make it extra
		       (setq extra (subseq dest xpos)
			     dest  (subseq dest 0 xpos)))
	       
	       `(:clp "wa" "link"
		      (("name" . ,dest)
                       ;; added by cac 29jun16 -- pass to wa_link for
                       ;; relative-path / webaction-destination fixup.
		       ("filename" . ,filename)
		       ,@(if* extra then 
				 `(("extra" . ,extra))))
		      nil))))
      (block outer
	(loop
	  (if* (null (setq ch (read-char p nil nil)))
	     then (savestring p pos-start (- chcount chstart))
		  (return))
	  (incf chcount)
	  (if* (eq ch #\<)
	     then 
		  (multiple-value-setq  (res lasttag)
		    (parse-clp-tag p filename))
		  ;(format t "res is ~s~%" res)
		  (if* res
		     then (savestring p pos-start 
				      (- chcount chstart 1))
                          ;; handle <clp_response ..> here
                          (let ((code (clp-response-check res)))
                            (if* code
                               then (setq response-code code)
                               else (push res result)))
			  (setq pos-start (file-position p)
				chstart chcount))
	   elseif (eq ch #\")
	     then (if* (or (match-buffer backbuffer backindex "=ferh")
                           ;; check for action= within a form only since
                           ;; backbase use b:action=
                           ;; cac 2aug07
 			   (and (equalp lasttag "form")
 				(match-buffer backbuffer backindex "=noitca"))
			   (and (member lasttag '("frame" "script" "img")
                                        :test #'equalp)
				(match-buffer backbuffer backindex "=crs"))
			   )
		     then (savestring p pos-start (- chcount chstart))
                          ; scan for tag name
			  (let ((savepos (file-position p)))
			    (setq chstart chcount)
			    (loop
			      (let ((lastpos (file-position p))
				    (ch (read-char p nil nil)))
				(if* (null ch)
				   then ; no " seen to end tag
					(savestring p 
						    pos-start 
						    (- chcount chstart))
					(return-from outer nil))
				(incf chcount)
				(if* (eq ch #\")
				   then  ; end of tag, collect
					(savestring p savepos
						    (- chcount chstart 1))
					(let ((res (car result)))
					  (if* (and (> (length (cadr res)) 0)
						    (or (member 
							 (aref (cadr res) 0)
							 '(#\/ #\# #\?))
							(match-regexp
							 "^[A-Za-z]+:" ;eg: http: 
							 (cadr res)))
						    )
					     thenret ; absolute pathname, ok
					     else (pop result)
						  (push (wa-cvt (cadr res))
							result)))
					(setq pos-start lastpos
					      chstart (1- chcount))
					(return))))))
	     else (setq backindex (mod (1+ backindex) (length backbuffer)))
		  (setf (aref backbuffer backindex) ch)))))
      
    ;; return
    (values (nreverse result) response-code)))

(defun clp-response-check (form)
  ;; test if we've encountered something like:
  ;;  (:clp "clp" "response" (("code" . "233"))) 
  ;; if so return  233
  ;; or
  ;;   (:clp "clp" "response" (("code" . "foo")))
  ;; then return "foo"
  ;;
  (if* (consp form)
     then 
          (destructuring-bind (&optional tag module function args &rest ignore)
              form

            (declare (ignore ignore))
            (if* (and (eq :clp tag)
                      (equal "clp" module)
                      (equal "response" function))
               then (if* (and (consp args)
                              (consp (car args))
                              (equal "code" (car (car args))))
                       then ;; if it's an integer return that
                            ;; else return the value of code
                            ;; which will be a string naming
                            ;; a session variable
                            (or (ignore-errors 
                                 (parse-integer (cdr (car args))))
                                (cdr (car args))))))))
                                  
           

(defun match-buffer (buffer index string)
  ;; see if string matches what's in the buffer, case insensitive
  (dotimes (i (length string) t)
    (if* (not (char-equal (schar string i)
			  (schar buffer index)))
       then (return nil))
    (if* (< (decf index) 0) then (setq index (1- (length buffer))))))
  

(eval-when (compile load eval)
  (defparameter *clp-white-space* '(#\space #\tab #\return #\linefeed))
  (defparameter *clp-end-tagname* '(#\space #\tab #\return #\linefeed
				    #\/ #\>))
)

(defun parse-clp-tag  (p filename)
  (multiple-value-bind (form lasttag) (parse-clp-tag-1 p filename)
    (if* (and (consp form) (eq :clp (pobj-tag form)))
       then (values (postprocess-clp-tag form)
                    lasttag)
       else (values form lasttag))))
            
(defun parse-clp-tag-1 (p filename)
  ;; just read a <.. now see if there's a clp tag to read  ;;
  (macrolet ((no-tag-found (tag)
	       `(progn (file-position p start-pos) ; restore position
		       (return-from parse-clp-tag-1 
			 (values nil ,tag)))))
    
    (let ((start-pos (file-position p))
	  (chars))
      (loop
	(let ((ch (read-char p nil nil)))
	  (if* (null ch)
	     then (no-tag-found nil))
	
	  (if* (member ch *clp-end-tagname*)
	     then ; seen end of tag
		  (unread-char ch p)
		  (let ((tag (make-array (length chars)
					 :element-type 'character
					 :initial-contents (nreverse chars))))
		    ;(format t "tag is ~s~%" tag)
		    (if* (equal tag "!--")
		       then (return-from parse-clp-tag-1
			      (collect-comment p)))
		
		    (let ((pos (position #\_ tag)))
		      (if* pos
			 then ; possible clp tag
			      (let ((module (subseq tag 0 pos))
				    (fcn    (subseq tag (1+ pos))))
				(if* (or (equal module "clp") ; built-in
					 (find-clp-module module))
				   then ; ok a valid module
					(let ((clptag (process-clp-tag
						       p 
						       module
						       fcn
						       filename)))
					  (if* clptag
					     then 
						  (return-from parse-clp-tag-1
						    clptag))))))
		      (no-tag-found tag)))
	     else (push ch chars)))))))
			
	

(defun process-clp-tag (p module fcn filename)
  ;; we've found a valid clp tag.
  ;; now parse the arguments and the body
  ;; and recusively parse the guts
  (let (arg-values namechars valuechars seendq)
    (flet ((finish-attribute ()
             ;; build the attribute name,values
	     (let ((name (make-array (length namechars)
				     :element-type 'character
				     :initial-contents (nreverse
							namechars)))
		   (value (and valuechars
			       (make-array (length valuechars)
					   :element-type 'character
					   :initial-contents 
					   (nreverse valuechars)))))
	       (push (cons name
			   (or value ""))
		     arg-values)
	       )))
      (let ((state 0))
      
	(loop
	  (let ((ch (read-char p nil nil)))
	    (if* (null ch)
	       then ; eof before end of tag seen, give up
		    (return-from process-clp-tag nil))
	    (case state
	      (0  ; looking for next interesting thing
	       (case ch
		 (#\/ (setq state 1))
		 (#\> (setq state 2)) ; end of tag, look for body
		 (#.*clp-white-space*
		  nil  ; do nothing
		  )
		 (t (setq namechars (list ch)
			  valuechars nil)
		    (setq state 3))))
	    
              ; seen /, expect > next
	      (1 (if* (eq ch #\>)
		    then ; body-free clp tag
			 (return-from process-clp-tag
                           (make-pobj
                            :tag :clp 
                            :module module 
                            :name fcn 
                            :args (nreverse arg-values) 
                            :body nil))
		    else ; not a valid tag
			 (return-from process-clp-tag nil)))
	    
              ; seen end of tag, scan until </tag> seen
	      (2 (unread-char ch p)
		 (let ((body-start (file-position p)))
		   (let ((length (scan-for-end-tag p module fcn)))
		     (if* length
			then ; found end tag, now parse the 
                             ; body
			     
			     (let ((body (make-array length
						     :element-type
						     'character))
				   (curpos (file-position p)))
			       (file-position p body-start)
			       (read-sequence body p)
			       (file-position p curpos)
			     
			       (return-from process-clp-tag
				 (make-pobj 
                                  :tag :clp 
                                  :module module 
                                  :name fcn 
                                  :args (nreverse arg-values)
                                  :body (parse-clp-guts 
                                         (make-string-input-stream body)
                                         filename
                                         ))))
			else ; no end tag, bogus!
			     (return-from process-clp-tag nil)))))
	    
              ; collecting the attribute name
	      (3 (case ch
		   (#\= ; end of name, time for value
		    (setq valuechars nil)
		    (setq state 4))
		   (#.*clp-white-space*
		    (finish-attribute)
		    (setq state 0))
		   (#\/ 
		    (finish-attribute)
		    (setq state 1))
		   (#\> 
		    (finish-attribute)
		    (setq state 2))
		   (t (push ch namechars))))
	    
              ; start collecting the attribute value
	      
	      (4 (case ch
		   (#.*clp-white-space*
		    (finish-attribute)
		    (setq state 0))
		   (#\/ (finish-attribute)
                    (setq state 1))
		   (#\> (finish-attribute)
                    (setq state 2))
		   (#\" (setq seendq t
			      state 5))
		   (t (push ch valuechars)
		      (setq state 5)
		      )))
	      
              ; in the middle of collecting an attribute value
	      (5 
	       (if* seendq
		  then (if* (eq ch #\")
			  then (finish-attribute)
			       (setq state 0)
			  else (push ch valuechars))
		  else (case ch
			 (#.*clp-white-space*
			  (finish-attribute)
			  (setq state 0))
			 (#\/ (finish-attribute)
                          (setq state 1))
			 (#\> (finish-attribute)
                          (setq state 2))
			 (t (push ch valuechars))))))))))))
			     
	
	
    



(defun scan-for-end-tag (p module fcn)
  ;; look for </module_fcn>
  ;; leave the file position after the tag
  ;;
  ;; return the number of characters read not including
  ;; the end tag
  ;; 
  ;; return  nil if the end tag wasn't found
  ;;
  
  ;; we define a search obj as a cons holding how far we've
  ;; matched so far and the string we're matching
  (macrolet ((create-search-obj (string)
	       `(cons 0 ,string))
	     
	     (init-search-obj (obj)
	       ;; set back to initial state
	       `(setf (car ,obj) 0))
	     
	     (end-of-search-p (obj)
	       ;; see if we've matched all characters
	       `(equal (car ,obj) (length (cdr ,obj))))
	     
	     (search-string (obj)
	       `(cdr ,obj))
	     
	     (search-counter (obj)
	       `(car ,obj))
	     
	     (match-search-string (obj ch)
	       `(if* (eql ,ch (schar (search-string ,obj) 
				     (search-counter ,obj)))
		   then (incf (search-counter ,obj))
		   else (init-search-obj ,obj))))
	     
    (let ((end-tag   (create-search-obj (format nil "</~a_~a>" module fcn)))
	  (start-tag (create-search-obj (format nil "<~a_~a" module fcn)))
	  (nest-level 0)
	  (ch)
	  (chcount 0))

    
      (loop

	;(format t "nest: ~s  start ~s . end ~s~%" nest-level start-tag end-tag)
	(if* (end-of-search-p end-tag)
	   then (if* (> nest-level 0)
		   then (decf nest-level)
			(init-search-obj end-tag)
		   else (return (- chcount (length (search-string end-tag))))))
	
	
	    

    
	; get next character ...
	(if* (null (setq ch (read-char p nil nil)))
	   then ; no end tag found
		(return nil))
	
	; start tag can end with > or a space character preceeding
	; attributes
	(if* (end-of-search-p start-tag)
	   then (if* (member ch '(#\> #\space #\tab #\newline))
		   then (incf nest-level))
		(init-search-obj start-tag))
	
	(incf chcount)

	;; and look for matches
	(match-search-string end-tag ch)
	(match-search-string start-tag ch)))))
	    

(defun collect-comment (p)
  ;; return a text object holding a whole comment
  (let ((state 0) 
	(start-pos (file-position p))
	(chcount 0))
    (flet ((makestr ()
	     (let ((retstr (make-array (+ 5 chcount)
				       :element-type 'character)))
	       (setf (aref retstr 0) #\<)
	       (setf (aref retstr 1) #\!)
	       (setf (aref retstr 2) #\-)
	       (setf (aref retstr 3) #\-)
	       (setf (aref retstr 4) #\space)
	       (file-position p start-pos)
	       (dotimes (i chcount)
		 (setf (aref retstr (+ i 5)) (read-char p)))
	       `(:text ,retstr))))
	       
      (loop
	(let ((ch (read-char p nil nil)))
	  (if* (null ch) then (return (makestr)))
	  (incf chcount)
	  (case state
	    (0 (if* (eql #\- ch) then (setq state 1)))
	    (1 (if* (eql #\- ch) 
		  then (setq state 2)
		  else (setq state 0)))
	    (2 (case ch
		 (#\> (return (makestr)))
		 (#\- nil) ; still in state 2
		 (t (setq state 0))))))))))





(defun emit-clp-entity (req ent objects)
  ;; send objects in the clp to the output stream
  (dolist (obj objects)
    ;(format t "process ~s~%" obj)
    (if* (consp obj)
       then (case (car obj)
	      (:text (write-sequence (cadr obj) *html-stream*))
	      (:clp
	       (destructuring-bind (mod fcn args body) (cdr obj)
		   (let ((func (find-clp-module-function mod fcn)))
		     (if* func
			then (funcall func req ent args body)))))))))


;; code for clp tags that need to be evaluated at parse time

(defun postprocess-clp-tag (pobj)
  ;; cases handled
  ;;   <clp_value name=foo include>
  ;; will look at the args supplied with enclosing clp_includes and retrieve
  ;; the value of that arg
  (if* (and (equal  (pobj-module pobj) "clp")
            (equal  (pobj-name pobj) "value")
            (assoc "include" (pobj-args pobj) :test #'equal))
     then ;; retrieve value of 
          (let* ((name (cdr (assoc "name" (pobj-args pobj) :test #'equal)))
                 (arg-value
                  (and name (cdr (assoc name *include-args* :test #'equal)))))
            `(:text ,(or arg-value "")))
     else pobj))

            
            
          
          
          
         
		   


(defvar *clp-modules* (make-hash-table :test #'equal))

(defun find-clp-module (modname &key create)
  (let ((mod (gethash modname *clp-modules*)))
    (or mod
	(if* create
	   then (setf (gethash modname *clp-modules*)
		  (make-hash-table :test #'equal))))))

(defun find-clp-module-function (module function)
  ;; get the specified function in the specified module
  (let ((mod (find-clp-module module)))
    (and mod (gethash function mod))))

	


  
;; define a clp handler
; args to lambda are   req ent args body
;;
(defmacro def-clp-function (fcn args &rest body)
  (let ((name (if* (symbolp fcn)
		 then (string-downcase (symbol-name fcn))
	       elseif (stringp fcn)
		 then fcn
		 else (error "The first argument to def-clp-function should be a string or a symbol, not ~s" fcn)))
	(module)
	(function)
	(pos))
    
    (setq pos (position #\_ name))
    (if* (null pos)
       then (error "def-clp-function names must have an underscore in them ~
to separate the module part from the function name part, ~s doesn't" name))
    (setq module (subseq name 0 pos)
	  function (subseq name (1+ pos)))
    
    `(setf (gethash ,function
		    (find-clp-module ,module :create t))
       (named-function (:clp ,name) (lambda ,args ,@body)))))






(provide :webactions)
