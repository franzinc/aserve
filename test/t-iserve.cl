;; -*- mode: common-lisp; package: net.iserve.test -*-
;;
;; t-iserve.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation; 
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
;; $Id: t-iserve.cl,v 1.1 2000/03/21 05:56:50 jkf Exp $

;; Description:
;;   test iserve

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(eval-when (compile load eval)
  (require :test))

(defpackage :net.iserve.test
  (:use :common-lisp :excl :net.html.generator :net.iserve 
	:net.iserve.client
	:test)
  )

(in-package :net.iserve.test)


(test-set iserve
  (let ((port (start-iserve-running)))
    (unwind-protect 
	(progn)
      (stop-iserve-running))))


(defun start-iserve-running ()
  ;; start iserve, return the port on which we've started iserve
  (let ((wserver (start :port nil)))	; let the system pick a port
    (unpublish :all t) ; flush anything published
    (socket::local-port (net.iserve::wserver-socket wserver))
    ))

(defun stop-iserver-running ()
  (shutdown))



  

