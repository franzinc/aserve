(sys:defpatch "aserve" 2
  "v1: aserve version 1.2.25, various fixes from 1.2.24 released with 6.2;
htmlgen fix :comment and :prin1-safe when html-print'ing;
v2: aserve version 1.2.27, various fixes from 1.2.25."
  :type :system
  :post-loadable t)

;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; packages.cl
;;
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA 
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
;; $Id: packages.cl,v 1.1.4.5 2003/07/08 22:22:27 layer Exp $

;; Description:
;;   packages and exports for AllegroServe
;;
;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


; note: net.html.generator is not defined here since that's a
;  standalone package
;
(in-package :user)

(defpackage :net.aserve
  (:use :common-lisp :excl :net.html.generator :net.uri)
  (:export
   #:authorize
   #:authorizer
   #:base64-decode
   #:base64-encode
   #:compute-strategy
   #:computed-entity
   ;; don't export, these should be private
   ; #:debug-off		
   ; #:debug-on			
   #:denied-request
   #:enable-proxy
   #:ensure-stream-lock
   #:entity-plist
   #:failed-request
   #:form-urlencoded-to-query
   #:function-authorizer ; class
   #:function-authorizer-function
   #:get-basic-authorization
   #:get-cookie-values
   #:get-all-multipart-data
   #:get-multipart-header
   #:get-multipart-sequence
   #:get-request-body
   #:handle-request
   #:handle-uri		; add-on component..
   #:header-slot-value
   #:http-request  	; class
   #:locator		; class
   #:location-authorizer  ; class
   #:location-authorizer-patterns
   #:map-entities
   #:parse-multipart-header
   #:password-authorizer  ; class
   #:process-entity
   #:publish
   #:publish-file
   #:publish-directory
   #:publish-multi
   #:publish-prefix
   #:query-to-form-urlencoded
   #:reply-header-slot-value 
   #:run-cgi-program
   #:set-basic-authorization
   #:standard-locator
   #:unpublish-locator
   #:vhost
   #:vhost-log-stream
   #:vhost-error-stream
   #:vhost-names
   #:vhost-plist

   #:request-method
   #:request-protocol

   #:request-protocol-string
   #:request-query
   #:request-query-value
   #:request-raw-request
   #:request-raw-uri
   #:request-socket
   #:request-uri
   #:request-wserver
   
   #:request-reply-code
   #:request-reply-date
   #:request-reply-content-length
   #:request-reply-content-type
   #:request-reply-plist
   #:request-reply-protocol-string
   #:request-reply-strategy
   #:request-reply-stream
   
   #:set-cookie-header
   #:shutdown
   #:split-into-words
   #:start
   #:uridecode-string
   #:uriencode-string
   #:unpublish
   #:url-argument
   #:url-argument-alist
   #:with-http-response
   #:with-http-body
   
   #:wserver
   #:wserver-default-vhost
   #:wserver-enable-chunking
   #:wserver-enable-keep-alive
   #:wserver-external-format
   #:wserver-filters
   #:wserver-locators
   #:wserer-io-timeout
   #:wserver-log-function
   #:wserver-log-stream
   #:wserver-response-timeout
   #:wserver-socket
   #:wserver-vhosts

   #:*aserve-version*
   #:*default-aserve-external-format*
   #:*http-io-timeout*
   #:*http-response-timeout*
   #:*mime-types*
   #:*response-accepted*
   #:*response-bad-request*
   #:*response-continue*
   #:*response-created*
   #:*response-found*
   #:*response-internal-server-error*
   #:*response-not-found*
   #:*response-not-modified*
   #:*response-ok*
   #:*response-moved-permanently*
   #:*response-see-other*
   #:*response-temporary-redirect*
   #:*response-unauthorized*
   #:*wserver*))


(defpackage :net.aserve.client 
  (:use :net.aserve :excl :common-lisp)
  (:export 
   #:client-request  ; class
   #:client-request-close
   #:client-request-cookies
   #:client-request-headers
   #:client-request-protocol
   #:client-request-read-sequence
   #:client-request-response-code
   #:client-request-response-comment
   #:client-request-socket
   #:client-request-uri
   #:client-response-header-value
   #:cookie-item
   #:cookie-item-expires
   #:cookie-item-name
   #:cookie-item-path
   #:cookie-item-secure
   #:cookie-item-value
   #:cookie-jar     ; class
   #:do-http-request
   #:make-http-client-request
   #:read-client-response-headers
   ))

;; required so old versions of the patch can be loaded.
(when (fboundp 'get-request-body)
  (makfunbound 'get-request-body))
