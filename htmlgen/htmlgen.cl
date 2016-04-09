;; -*- mode: common-lisp; package: net.html.generator -*-
;;
;; htmlgen.cl
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
;;   html generator

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



(defpackage :net.html.generator
  (:use :common-lisp :excl)
  (:export #:html 
	   #:html-print
	   #:html-print-subst
	   #:html-print-list
	   #:html-print-list-subst
	   #:html-stream 
	   #:*html-stream*
	   
	   
	   ;; should export with with-html-xxx things too I suppose
	   ))

(in-package :net.html.generator)

;; html generation

(defvar *html-stream*)	; all output sent here

(defstruct (html-process (:type list) (:constructor
				       make-html-process (key has-inverse
							      macro special
							      print
							      name-attr
							      )))
  key		; keyword naming this tag
  has-inverse	; t if the / form is used
  macro  	; the macro to define this
  special       ; if true then call this to process the keyword and return
                ; the macroexpansion
  print         ; function used to handle this in html-print
  name-attr     ; attribute symbols which can name this object for subst purposes
  )


(defparameter *html-process-table* 
    (make-hash-table :test #'equal) ; #'eq is accurate but want to avoid rehashes
  )

(defvar *html-stream* nil) ; where the output goes

(defmacro html (&rest forms &environment env)
  ;; just emit html to the curfent stream
  (process-html-forms forms env))

(defmacro html-out-stream-check (stream)
  ;; ensure that a real stream is passed to this function
  `(let ((.str. ,stream))
     (if* (not (streamp .str.))
	then (error "html-stream must be passed a stream object, not ~s"
		    .str.))
     .str.))


(defmacro html-stream (stream &rest forms)
  ;; set output stream and emit html
  `(let ((*html-stream* (html-out-stream-check ,stream))) (html ,@forms)))



(defun process-html-forms (forms env)
  (let (res)
    (flet ((do-ent (ent args argsp body)
	     ;; ent is an html-process object associated with the 
	     ;;	    html tag we're processing
	     ;; args is the list of values after the tag in the form
	     ;;     ((:tag &rest args) ....)
	     ;; argsp is true if this isn't a singleton tag  (i.e. it has
	     ;;     a body) .. (:tag ...) or ((:tag ...) ...)
	     ;; body is the body if any of the form
	     ;; 
	     (let (spec)
	       (if* (setq spec (html-process-special ent))
		  then ; do something different
		       (push (funcall spec ent args argsp body) res)
		elseif (null argsp)
		  then ; singleton tag, just do the set
		       (push `(,(html-process-macro ent) :set) res)
		       nil
		  else (if* (equal args '(:unset))
			  then ; ((:tag :unset)) is a special case.
			       ; that allows us to close off singleton tags
			       ; printed earlier.
			       (push `(,(html-process-macro ent) :unset) res)
			       nil
			  else ; some args
			       (push `(,(html-process-macro ent) ,args
								 ,(process-html-forms body env))
				     res)
			       nil)))))
				 
		    

      (do* ((xforms forms (cdr xforms))
	    (form (car xforms) (car xforms)))
	  ((null xforms))

	(setq form (macroexpand form env  #+(version>= 7 0) t))
	
	(if* (atom form)
	   then (if* (keywordp form)
		   then (let ((ent (gethash form *html-process-table*)))
			  (if* (null ent)
			     then (error "unknown html keyword ~s"
					 form)
			     else (do-ent ent nil nil nil)))
		 elseif (stringp form)
		   then ; turn into a print of it
			(push `(write-string ,form *html-stream*) res)
		   else (push form res))
	   else (let ((first (car form)))
		  (if* (keywordp first)
		     then ; (:xxx . body) form
			  (let ((ent (gethash first
					    *html-process-table*)))
			    (if* (null ent)
			       then (error "unknown html keyword ~s"
					   form)
			       else (do-ent ent nil t (cdr form))))
		   elseif (and (consp first) (keywordp (car first)))
		     then ; ((:xxx args ) . body)
			  (let ((ent (gethash (car first)
					    *html-process-table*)))
			    (if* (null ent)
			       then (error "unknown html keyword ~s"
					   form)
			       else (do-ent ent (cdr first) t (cdr form))))
		     else (push form res))))))
    `(progn ,@(nreverse res))))


(defun html-atom-check (args open close body)
  (if* (and args (atom args))
     then (let ((ans (case args
		       (:set `(write-string  ,open *html-stream*))
		       (:unset `(write-string  ,close *html-stream*))
		       (t (error "illegal arg ~s to ~s" args open)))))
	    (if* (and ans body)
	       then (error "can't have a body form with this arg: ~s"
			   args)
	       else ans))))

(defun html-body-form (open close body)
  ;; used when args don't matter
  `(progn (write-string  ,open *html-stream*)
	  ,@body
	  (write-string  ,close *html-stream*)))


(defun html-body-key-form (string-code has-inv args body)
  ;; do what's needed to handle given keywords in the args
  ;; then do the body
  (if* (and args (atom args))
     then ; single arg 
	  (return-from html-body-key-form
	    (case args
	      (:set `(write-string  ,(format nil "<~a>" string-code)
				    *html-stream*))
	      (:unset (if* has-inv
			 then `(write-string  ,(format nil "</~a>" string-code)
					      *html-stream*)))
	      (t (error "illegal arg ~s to ~s" args string-code)))))
  
  (if* (not (evenp (length args)))
     then (warn "arg list ~s isn't even" args))
  
  
  (if* args
     then `(progn (write-string ,(format nil "<~a" string-code)
				*html-stream*)
		  ,@(do ((xx args (cddr xx))
			 (res))
			((null xx)
			 (nreverse res))
		      (if* (eq :if* (car xx))
			 then ; insert following conditionally
			      (push `(if* ,(cadr xx)
					then (write-string 
					      ,(format nil " ~a" (caddr xx))
					      *html-stream*)
					     (prin1-safe-http-string ,(cadddr xx)))
				    res)
			      (pop xx) (pop xx)
			 else 
					     
			      (push `(write-string 
				      ,(format nil " ~a" (car xx))
				      *html-stream*)
				    res)
			      (push `(prin1-safe-http-string ,(cadr xx)) res)))
						    
		      
		  (write-string ">" *html-stream*)
		  ,@body
		  ,(if* (and body has-inv)
		      then `(write-string ,(format nil "</~a>" string-code)
					  *html-stream*)))
     else `(progn (write-string ,(format nil "<~a>" string-code)
				*html-stream*)
		  ,@body
		  ,(if* (and body has-inv)
		      then `(write-string ,(format nil "</~a>" string-code)
					  *html-stream*)))))
			     
		 

(defun princ-http (val)
  ;; print the given value to the http stream using ~a
  (format *html-stream* "~a" val))

(defun prin1-http (val)
  ;; print the given value to the http stream using ~s
  (format *html-stream* "~s" val))


(defun princ-safe-http (val)
  (emit-safe *html-stream* (format nil "~a" val)))

(defun prin1-safe-http (val)
  (emit-safe *html-stream* (format nil "~s" val)))


(defun prin1-safe-http-string (val)
  ;; used only in a parameter value situation
  ;;
  ;; if the parameter value is the symbol with the empty print name
  ;; then turn this into a singleton object.  Thus || is differnent
  ;; than "".
  ;;
  ;; print the contents inside a string double quotes (which should
  ;; not be turned into &quot;'s
  ;; symbols are turned into their name
  (if* (and (symbolp val)
	    (equal "" (symbol-name val)))
     thenret ; do nothing
     else (write-char #\= *html-stream*)
	  (if* (or (stringp val)
		   (and (symbolp val) 
			(setq val (symbol-name val))))
	     then (write-char #\" *html-stream*)
		  (emit-safe *html-stream* val)
		  (write-char #\" *html-stream*)
	     else (prin1-safe-http val))))



(defun emit-safe (stream string)
  ;; send the string to the http response stream watching out for
  ;; special html characters and encoding them appropriately
  (do* ((i 0 (1+ i))
	(start i)
	(end (length string)))
      ((>= i end)
       (if* (< start i)
	  then  (write-sequence string
				stream
				:start start
				:end i)))
	 
      
    (let ((ch (char string i))
	  (cvt ))
      (if* (eql ch #\<)
	 then (setq cvt "&lt;")
       elseif (eq ch #\>)
	 then (setq cvt "&gt;")
       elseif (eq ch #\&)
	 then (setq cvt "&amp;")
       elseif (eq ch #\")
	 then (setq cvt "&quot;"))
      (if* cvt
	 then ; must do a conversion, emit previous chars first
		
	      (if* (< start i)
		 then  (write-sequence string
				       stream
				       :start start
				       :end i))
	      (write-string cvt stream)
		
	      (setq start (1+ i))))))
	
		

(defun html-print-list (list-of-forms stream &key unknown)
  ;; html print a list of forms
  (dolist (x list-of-forms)
    (html-print-subst x nil stream unknown)))


(defun html-print-list-subst (list-of-forms subst stream &key unknown)
  ;; html print a list of forms
  (dolist (x list-of-forms)
    (html-print-subst x subst stream unknown)))


(defun html-print (form stream &key unknown)
  (html-print-subst form nil stream unknown))


(defun html-print-subst (form subst stream unknown)
  ;; Print the given lhtml form to the given stream
  (assert (streamp stream))
    
	       
  (let* ((attrs)
	 (attr-name)
	 (name)
	 (possible-kwd (if* (atom form)
			  then form
			elseif (consp (car form))
			  then (setq attrs (cdar form))
			       (caar form)
			  else (car form)))
	 print-handler
	 ent)
    (if* (keywordp possible-kwd)
       then (if* (null (setq ent (gethash possible-kwd *html-process-table*)))
	       then (if* unknown
		       then (return-from html-print-subst
			      (funcall unknown form stream))
		       else (error "unknown html tag: ~s" possible-kwd))
	       else ; see if we should subst
		    (if* (and subst 
			      attrs 
			      (setq attr-name (html-process-name-attr ent))
			      (setq name (getf attrs attr-name))
			      (setq attrs (html-find-value name subst)))
		       then
			    (return-from html-print-subst
			      (if* (functionp (cdr attrs))
				 then 
				      (funcall (cdr attrs) stream)
				 else (html-print-subst
				       (cdr attrs)
				       subst
				       stream
				       unknown)))))
				     
	    (setq print-handler
	      (html-process-print ent)))
    (if* (atom form)
       then (if* (keywordp form)
	       then (funcall print-handler ent :set nil nil nil nil stream)
	     elseif (stringp form)
	       then (write-string form stream)
	       else (princ form stream))
     elseif ent
       then (funcall print-handler 
		     ent
		     :full
		     (if* (consp (car form)) then (cdr (car form)))
		     form 
		     subst
		     unknown
		     stream)
       else (error "Illegal form: ~s" form))))

  
(defun html-find-value (key subst)
  ; find the (key . value) object in the subst list.
  ; A subst list is an assoc list ((key . value) ....)
  ; but instead of a (key . value) cons you may have an assoc list
  ;
  (let ((to-process nil)
	(alist subst))
    (loop
      (do* ((entlist alist (cdr entlist))
	    (ent (car entlist) (car entlist)))
	  ((null entlist) (setq alist nil))
	(if* (consp (car ent))
	   then ; this is another alist
		(if* (cdr entlist)
		   then (push (cdr entlist) to-process))
		(setq alist ent)
		(return) ; exit do*
	 elseif (equal key (car ent))
	   then (return-from html-find-value ent)))
	       
      (if* (null alist)
	 then ; we need to find a new alist to process
	     
	      (if* to-process
		 then (setq alist (pop to-process))
		 else (return))))))

(defun html-standard-print (ent cmd args form subst unknown stream)
  ;; the print handler for the normal html operators
  (ecase cmd
    (:set ; just turn it on
     (format stream "<~a>" (html-process-key ent)))
    (:full ; set, do body and then unset
     (let (iter)
       (if* args
	  then (if* (and (setq iter (getf args :iter))
			 (setq iter (html-find-value iter subst)))
		  then ; remove the iter and pre
		       (setq args (copy-list args))
		       (remf args :iter)
		       (funcall (cdr iter)
				(cons (cons (caar form)
					    args)
				      (cdr form))
				subst
				stream)
		       (return-from html-standard-print)
		  else
		       (format stream "<~a" (html-process-key ent))
		       (do ((xx args (cddr xx)))
			   ((null xx))
			 ; assume that the arg is already escaped 
			 ; since we read it
			 ; from the parser
			 (format stream " ~a=\"~a\"" (car xx) (cadr xx)))
		       (format stream ">"))
	  else (format stream "<~a>" (html-process-key ent)))
       (dolist (ff (cdr form))
	 (html-print-subst ff subst stream unknown)))
     (if* (html-process-has-inverse ent)
	then ; end the form
	     (format stream "</~a>" (html-process-key ent))))))
     
  
  
		  
		    
  
					 
		      
;; --  defining how html tags are handled. --
;;
;; most tags are handled in a standard way and the def-std-html
;; macro is used to define such tags
;;
;; Some tags need special treatment and def-special-html defines
;; how these are handled.  The tags requiring special treatment
;; are the pseudo tags we added to control operations
;; in the html generator.
;; 
;;
;; tags can be found in three ways:
;;  :br	    		- singleton, no attributes, no body
;;  (:b "foo")          - no attributes but with a body
;;  ((:a href="foo") "balh")  - attributes and body
;;
  
  

(defmacro def-special-html (kwd fcn print-fcn)
  ;; kwd - the tag we're defining behavior for.
  ;; fcn - function to compute the macroexpansion of a use of this
  ;;       tag. args to fcn are: 
  ;;		ent - html-process object holding info on this tag
  ;;		args - list of attribute-values following tag
  ;;		argsp - true if there is a body in this use of the tag
  ;;		body - list of body forms.
  ;; print-fcn - function to print an lhtml form with this tag 
  ;;	    args to fcn are:
  ;;		ent - html-process object holding info on this tag
  ;;		cmd - one of :set, :unset, :full
  ;;		args - list of attribute-value pairs
  ;;		subst - subsitution list
  ;;		unknown - function to call for unknown tags
  ;;		stream - stream to write to
  ;;		
  `(setf (gethash ,kwd *html-process-table*) 
     (make-html-process ,kwd nil nil ,fcn ,print-fcn nil)))


(def-special-html :newline 
    (excl:named-function html-newline-function
      (lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	(if* body
	   then (error "can't have a body with :newline -- body is ~s" body))
			       
	`(terpri *html-stream*)))
  
  (excl:named-function html-newline-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (if* (eq cmd :set)
	 then (terpri stream)
	 else (error ":newline in an illegal place: ~s" form)))))

(def-special-html :princ
    (excl:named-function html-princ-function
      (lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(princ-http ,bod))
			  body))))
  
  (excl:named-function html-princ-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if* (eq cmd :full)
	 then (format stream "~a" (cadr form))
	 else (error ":princ must be given an argument")))))

(def-special-html :princ-safe 
    (excl:named-function html-princ-safe-function
      (lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(princ-safe-http ,bod))
			  body))))
  (excl:named-function html-princ-safe-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if* (eq cmd :full)
	 then (emit-safe stream (format nil "~a" (cadr form)))
	 else (error ":princ-safe must be given an argument")))))

(def-special-html :prin1
    (excl:named-function html-prin1-function
      (lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(prin1-http ,bod))
			  body))))
  (excl:named-function html-prin1-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent args unknown subst))
      (assert (eql 2 (length form)))
      (if* (eq cmd :full)
	 then (format stream "~s" (cadr form))
	 else (error ":prin1 must be given an argument")))))

(def-special-html :prin1-safe
    (excl:named-function html-prin1-safe-function
      (lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(prin1-safe-http ,bod))
			  body))))
  (excl:named-function html-prin1-safe-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent subst unknown))
      (assert (eql 2 (length form)))
      (if* (eq cmd :full)
	 then (emit-safe stream (format nil "~s" (cadr form)))
	 else (error ":prin1-safe must be given an argument")))))

(def-special-html :comment
    (excl:named-function html-comment-function
      (lambda (ent args argsp body)
	;; must use <!--   --> syntax
	(declare (ignore ent args argsp))
	`(progn (write-string "<!--" *html-stream*)
		(html ,@body)
		(write-string "-->" *html-stream*))))
  (excl:named-function html-comment-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent cmd args subst unknown))
      (format stream "<!--~a-->" (cadr form)))))



(defmacro def-std-html (kwd has-inverse name-attrs)
  (let ((mac-name (intern (format nil "~a-~a" :with-html kwd)))
	(string-code (string-downcase (string kwd))))
    `(progn (setf (gethash ,kwd *html-process-table*)
	      (make-html-process ,kwd ,has-inverse
				     ',mac-name
				     nil
				     #'html-standard-print
				     ',name-attrs))
	    (defmacro ,mac-name (args &rest body)
	      (html-body-key-form ,string-code ,has-inverse args body)))))

    

(def-std-html :a        t nil)
(def-std-html :abbr     t nil)
(def-std-html :acronym  t nil)
(def-std-html :address  t nil)
(def-std-html :applet   t nil)
(def-std-html :area    nil nil)

(def-std-html :b        t nil)
(def-std-html :base     nil nil)
(def-std-html :basefont nil nil)
(def-std-html :bdo      t nil)
(def-std-html :bgsound  nil nil)
(def-std-html :big      t nil)
(def-std-html :blink    t nil)
(def-std-html :blockquote  t nil)
(def-std-html :body      t nil)
(def-std-html :br       nil nil)
(def-std-html :button   t nil)

(def-std-html :caption  t nil)
(def-std-html :center   t nil)
(def-std-html :cite     t nil)
(def-std-html :code     t nil)
(def-std-html :col      nil nil)
(def-std-html :colgroup nil nil)

(def-std-html :dd        t nil)
(def-std-html :del       t nil)
(def-std-html :dfn       t nil)
(def-std-html :dir       t nil)
(def-std-html :div       t nil)
(def-std-html :dl        t nil)
(def-std-html :dt        t nil)

(def-std-html :em        t nil)

(def-std-html :fieldset        t nil)
(def-std-html :font        t nil)
(def-std-html :form        t :name)
(def-std-html :frame        t nil)
(def-std-html :frameset        t nil)

(def-std-html :h1        t nil)
(def-std-html :h2        t nil)
(def-std-html :h3        t nil)
(def-std-html :h4        t nil)
(def-std-html :h5        t nil)
(def-std-html :h6        t nil)
(def-std-html :head        t nil)
(def-std-html :hr        nil nil)
(def-std-html :html        t nil)

(def-std-html :i     t nil)
(def-std-html :iframe     t nil)
(def-std-html :ilayer     t nil)
(def-std-html :img     nil :id)
(def-std-html :input     nil nil)
(def-std-html :ins     t nil)
(def-std-html :isindex    nil nil)

(def-std-html :kbd  	t nil)

(def-std-html :label  	t nil)
(def-std-html :layer  	t nil)
(def-std-html :legend  	t nil)
(def-std-html :li  	t nil)
(def-std-html :link  	nil nil)
(def-std-html :listing  t nil)

(def-std-html :map  	t nil)
(def-std-html :marquee  t nil)
(def-std-html :menu  	t nil)
(def-std-html :meta  	nil nil)
(def-std-html :multicol t nil)

(def-std-html :nobr  	t nil)
(def-std-html :noembed  t nil)
(def-std-html :noframes t nil)
(def-std-html :noscript t nil)

(def-std-html :object  	t nil)
(def-std-html :ol  	t nil)
(def-std-html :optgroup t nil)
(def-std-html :option  	t nil)

(def-std-html :p  	t nil)
(def-std-html :param  	t nil)
(def-std-html :plaintext  nil nil)
(def-std-html :pre  	t nil)

(def-std-html :q  	t nil)

(def-std-html :s  	t nil)
(def-std-html :samp  	t nil)
(def-std-html :script  	t nil)
(def-std-html :select  	t nil)
(def-std-html :server  	t nil)
(def-std-html :small  	t nil)
(def-std-html :spacer  	nil nil)
(def-std-html :span  	t :id)
(def-std-html :strike  	t nil)
(def-std-html :strong  	t nil)
(def-std-html :style    t nil)  
(def-std-html :sub  	t nil)
(def-std-html :sup  	t nil)

(def-std-html :table  	t :name)
(def-std-html :tbody  	t nil)
(def-std-html :td  	t nil)
(def-std-html :textarea  t nil)
(def-std-html :tfoot  	t nil)
(def-std-html :th  	t nil)
(def-std-html :thead  	t nil)
(def-std-html :title  	t nil)
(def-std-html :tr  	t nil)
(def-std-html :tt  	t nil)

(def-std-html :u 	t nil)
(def-std-html :ul 	t nil)

(def-std-html :var 	t nil)

(def-std-html :xmp 	t nil)

;; html 5

(def-std-html :section		t nil)
(def-std-html :article		t nil)
(def-std-html :main		t nil)
(def-std-html :aside		t nil)
(def-std-html :hgroup		t nil)
(def-std-html :header		t nil)
(def-std-html :footer		t nil)
(def-std-html :nav		t nil)
(def-std-html :figure		t nil)
(def-std-html :figcaption 	t nil)

(def-std-html :video	 	t nil)
(def-std-html :audio	 	t nil)
(def-std-html :source	 	t nil)
(def-std-html :track	 	t nil)
(def-std-html :embed	 	t nil)
(def-std-html :mark	 	t nil)
(def-std-html :progress	 	t nil)
(def-std-html :meter	 	t nil)
(def-std-html :time	 	t nil)
(def-std-html :data	 	t nil)
(def-std-html :dialog	 	t nil)
(def-std-html :ruby	 	t nil)
(def-std-html :rt	 	t nil)
(def-std-html :rp	 	t nil)
(def-std-html :bdi	 	t nil)
(def-std-html :wbr		nil nil)
(def-std-html :canvas	 	t nil)
(def-std-html :menuitem	 	t nil)
(def-std-html :details	 	t nil)
(def-std-html :datalist	 	t nil)
(def-std-html :keygen	 	nil nil)
(def-std-html :output	 	t nil)
