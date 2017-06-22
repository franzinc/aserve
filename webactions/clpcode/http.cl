;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; http.cl
;; clp functions named http_xxx
;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

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


