;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; wa.cl
;; clp functions named wa_xxx
;;
;; copyright (c) 2003-2013 Franz Inc, Oakland, CA - All rights reserved.
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

    




    

  
  
      

