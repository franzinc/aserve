;; -*- mode: common-lisp; package: net.iserve -*-
;;
;; macs.cl
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
;; $Id: macs.cl,v 1.5.2.1 2000/02/08 19:48:37 jkf Exp $

;; Description:
;;   useful internal macros

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(eval-when (compile load eval)
 (require :uri))

;; macros used by neo
(defpackage :net.iserve
  (:use :common-lisp :excl :net.html.generator :net.uri))

(in-package :net.iserve)



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







;;;; response macros

