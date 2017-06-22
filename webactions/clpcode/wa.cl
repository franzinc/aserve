;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; wa.cl
;; clp functions named wa_xxx
;;
;; See the file LICENSE for the full license governing this code.
;;
;;


(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

(def-clp-function wa_link (req ent args body)
  (declare (ignore body))
  (let ((wa (getf (entity-plist ent) 'webaction))
	(session (getf (request-reply-plist req) 'websession)))
    (and wa
	 (let ((name (locate-action-path
		      wa 
		      (cdr (assoc "name" args :test #'equal))
		      session
		      ;; pass through any filename from clp parser for relative
		      ;; url fixup.  cac 29jun16
		      :filename (cdr (assoc "filename" args :test #'equal)))))
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

    




    

  
  
      

