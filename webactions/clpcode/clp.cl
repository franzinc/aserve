;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; clp.cl
;; clp functions named clp_xxx
;;
;; copyright (c) 2003 Franz Inc, Oakland CA  - All rights reserved.
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
;; $Id: clp.cl,v 1.1.2.1 2003/10/22 21:12:33 layer Exp $


(in-package :net.aserve)

(def-clp-function clp_base (req ent args body)
  ;; put out a base tag for this page.
  ;; use this in the head section so that relative links to images
  ;; and such are properly handled
  ;;
  (declare (ignore args body))
  (write-string "<base href=\"" *html-stream*)
  (render-uri (copy-uri (request-uri req) :path (path ent))
	      *html-stream*)
  (write-string "\">" *html-stream*))



(defun locate-any-value (req args name)
  ;; find the value with the given name looking in one of three
  ;; places:
  ;;  the request object's list of variable   [the default]
  ;;  the query
  ;;  the session
  ;;
  (let ((location :request))
    (if* (assoc "query" args :test #'equal)
       then (setq location :query)
     elseif (assoc "session" args :test #'equal)
       then (setq location :session))
    
    (case location
      (:request
       (request-variable-value req name))
      (:query
       (request-query-value name req))
      (:session
       (websession-variable (websession-from-req req) name)))))

(defsetf locate-any-value .inv-locate-any-value)

(defun .inv-locate-any-value (req args name value)
  (let ((location :request))
    (if* (assoc "query" args :test #'equal)
       then (setq location :query)
     elseif (assoc "session" args :test #'equal)
       then (setq location :session))
    
    (case location
      (:request
       (setf (request-variable-value req name) value))
      (:query
       (setf (request-query-value name req) value))
      (:session
       (setf (websession-variable (websession-from-req req) name) value)))))
  
(defun cvt-to-integer (value)
  ;; convert value to an integer if possible
  (if* (integerp value)
     then value
   elseif (stringp value)
     then (parse-integer value :junk-allowed t)))

(def-clp-function clp_value (req ent args body)
  ;; name=xxxx
  ;; print the value of the variable
  (declare (ignore ent body))
  (let* ((name (cdr (assoc "name" args :test #'equal)))
	 
	 (value (and name
		     (locate-any-value req args name)))
	 (safe (assoc "safe" args :test #'equal)))
    (if* value 
       then (if* safe
	       then (html (:princ-safe value))
	       else (html (:princ value))))))


(def-clp-function clp_set (req ent args body)
  ;; name=xxxx
  ;; value=yyyy
  ;; set the value of var xxxx to yyyy
  (declare (ignore ent body))
  (let* ((name (cdr (assoc "name" args :test #'equal)))
	 (value (cdr (assoc "value" args :test #'equal))))
    (if* name 
       then (setf (locate-any-value req args name) value))
    value))


(def-clp-function clp_ifgt (req ent args body)
  ;; name=varname
  ;; value=val
  ;;
  ;; compare the value of varname against the value.  If it's
  ;; greater than then process the body.
  ;;
  ;; if name or value cannot be turned into an integer value then
  ;; it's assume to not be greater than.
  ;;
  (let ((name  (cdr (assoc "name" args  :test #'equal)))
	(value (cdr (assoc "value" args :test #'equal))))
    (setq name (if* name
		  then (cvt-to-integer
			(locate-any-value req args name)))
	  value (cvt-to-integer value))
    ;(format   t "name ~s ... value ~s~%" name value)
    (if* (and name value
	      (> name value))
       then ; process the body
	    (emit-clp-entity req ent body))))

(def-clp-function clp_iflt (req ent args body)
  ;; name=varname
  ;; value=val
  ;;
  ;; compare the value of varname against the value.  If it's
  ;; greater than then process the body.
  ;;
  ;; if name or value cannot be turned into an integer value then
  ;; it's assume to not be greater than.
  ;;
  (let ((name  (cdr (assoc "name" args  :test #'equal)))
	(value (cdr (assoc "value" args :test #'equal))))
    (setq name (if* name
		  then (cvt-to-integer
			(locate-any-value req args name)))
	  value (cvt-to-integer value))
    ;(format   t "name ~s ... value ~s~%" name value)
    (if* (and name value
	      (< name value))
       then ; process the body
	    (emit-clp-entity req ent body))))

(def-clp-function clp_ifeq (req ent args body)
  ;; name=varname
  ;; value=val
  ;;
  ;; compare the value of varname against the value.  If it's
  ;; greater than then process the body.
  ;;
  ;; if name or value cannot be turned into an integer value then
  ;; it's assume to not be greater than.
  ;;
  (let ((name  (cdr (assoc "name" args  :test #'equal)))
	(value (cdr (assoc "value" args :test #'equal))))
    (setq name (if* name
		  then (cvt-to-integer
			(locate-any-value req args name)))
	  value (cvt-to-integer value))
    ;(format   t "name ~s ... value ~s~%" name value)
    (if* (and name value
	      (eql name value))
       then ; process the body
	    (emit-clp-entity req ent body))))



(def-clp-function clp_ifdef (req ent args body)
  ;; name=varname
  ;;
  ;; if name has a non-nil value then emit body
  ;;
  (let ((name (cdr (assoc "name" args :test #'equal))))
    (if* (and name (locate-any-value req args name))
       then ; process the body
	    (net.aserve::emit-clp-entity req ent body))))

(def-clp-function clp_ifndef (req ent args body)
  ;; name=varname
  ;;
  ;; if name is not defined or has nil value then emit body
  ;;
  (let ((name (cdr (assoc "name" args :test #'equal))))
    (if* (not (and name (locate-any-value req args name)))
       then ; process the body
	    (net.aserve::emit-clp-entity req ent body))))



(def-clp-function clp_ifequal (req ent args body)
  ;; name=varname
  ;; value=val
  ;;
  ;; compare the value of varname against the value, which
  ;; are both strings
  ;;
  ;;
  (let ((name  (cdr (assoc "name" args  :test #'equal)))
	(value (cdr (assoc "value" args :test #'equal))))
    (setq name (if* name
		  then (locate-any-value req args name)))
    ;(format   t "name ~s ... value ~s~%" name value)
    (if* (equal name value)
       then ; process the body
	    (emit-clp-entity req ent body))))


(def-clp-function clp_options (req ent args body)
  ;; if body contains a zero or more text elements then convert to
  ;; a list of option strings and change to (:options ..)
  ;; args contains a "name" tag which says which request query
  ;; value contains the default value
  (declare (ignore ent))
  (let ((val (locate-any-value 
	      req args (or (cdr (assoc "name" args :test #'equal)) "")))
	(firstselect)
	(options))
    (if* (dolist (form body t)
	   (if* (not (eq :text (car form)))
	      then (return nil)))
       then ; all :text forms
	    (let (res (s (make-string-input-stream
			  (apply #'concatenate 'string 
				 (mapcar #'second body)))))
	      (loop
		(let ((ent (read s nil s)))
		  (if* (eq s ent) then (return))
		  (push ent res)))
	      (setf (car body) `(:options ,(nreverse res)))))
    (if* (zerop (length val)) then (setq firstselect t))
  
    (setq options (if* (and (consp body)
			    (consp (car body))
			    (eq :options (caar body)))
		     then (cadr (car body))))
    (dolist (opt options)
      (if* (or firstselect
	       (equal val opt))
	 then (format *html-stream*
		      "<option selected>~a</option>~%" opt)
	      (setq firstselect nil)
	    
	 else (format *html-stream*
		      "<option>~a</option>~%" opt)))))

(def-clp-function clp_select (req ent args body)
  ;; this just does a <select> ... </select>
  ;; but is useful in cases where clp_options is used and
  ;; you're using an html editor that gets confused by
  ;; <select><clp_options ...> </select>
  ;;
  (format *html-stream*
	  "<select")
  (dolist (arg args)
    (format *html-stream* " ~a=~s" (car arg) (cdr arg)))
  (write-char #\> *html-stream*)
  (emit-clp-entity req ent body)

  (write-string "</select>" *html-stream*))
  
  
