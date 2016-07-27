;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; time.cl
;; clp functions named time_xxx
;;
;; See the file LICENSE for the full license governing this code.
;;
;;

(in-package :net.aserve)


(net.aserve:def-clp-function time_universal-time
    (req ent args body)
  (declare (ignore req ent args body))
  (net.html.generator:html (:princ-safe (get-universal-time))))



  
