;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; time.cl
;; clp functions named time_xxx
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
;; $Id: time.cl,v 1.1.2.1 2003/10/22 21:12:33 layer Exp $

(in-package :net.aserve)


(net.aserve:def-clp-function time_universal-time
    (req ent args body)
  (declare (ignore req ent args body))
  (net.html.generator:html (:princ-safe (get-universal-time))))



  
