;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; wa.cl
;; clp functions named wa_xxx
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
;; $Id: wa.cl,v 1.1.2.1 2003/10/22 21:12:33 layer Exp $


(in-package :net.aserve)


(def-clp-function wa_link (req ent args body)
  (declare (ignore body))
  (let ((wa (getf (entity-plist ent) 'webaction))
	(session (getf (request-reply-plist req) 'websession)))
    (and wa
	 (let ((name (locate-action-path
		      wa 
		      (cdr (assoc "name" args :test #'equal))
		      session)))
	   (html (:princ name))
	   (let ((extra (cdr (assoc "extra" args :test #'equal))))
	     (if* extra
		then (html (:princ extra))))
			    
	   ))))



	      
	      
(def-clp-function wa_showerrors (req ent args body)
  (declare (ignore ent body))
  (let* ((name (cdr (assoc "name" args :test #'equal)))
	 (clear (cdr (assoc "clear" args :test #'equal)))
	 (errs (locate-any-value req args (or name ""))))
    
    (if* clear 
       then (setf (locate-any-value req args (or name ""))
	      nil))
    
    (if* errs
       then (if* (atom errs)
	       then (setq errs (list errs)))
	    (html ((:font :color "red")
		   :br
		   (dolist (err errs)
		     (html (:princ-safe err) :br))
		   :br
		   )))
    ))

    




    

  
  
      

