;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; http.cl
;; clp functions named http_xxx
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
;; $Id: http.cl,v 1.1 2003/10/22 18:37:28 jkf Exp $

(in-package :net.aserve)


(def-clp-function http_header-value (req ent args body)
  (declare (ignore ent body))
  (let ((header-name (cdr (assoc "name" args :test #'equal))))
    (if* header-name
       then (let ((value 
		   (header-slot-value req
				     (read-from-string
				      (format nil ":~a"
					      (string-downcase header-name))))))
	      (html (:princ-safe value))))))


