  ;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; packages.cl
;;
;; See the file LICENSE for the full license governing this code.

;; 11.0.beta RC4 needs this patch
#+(version= 11 0 beta 25)
(sys:defpatch "aserve" 2
  "v2: 1.3.87: handle redirects before calling read-body-hook;
v1: 1.3.86: support colon in a password."
  :type :system
  :post-loadable t)

#+(version= 10 1)
(sys:defpatch "aserve" 33
  "v33: 1.3.87: handle redirects before calling read-body-hook;
v32: 1.3.86: support colon in a password;
v31: 1.3.85: support http on an ssl port;
v30: 1.3.84: add seize-request function;
v29: 1.3.81: do-http-request can be passed restartable-function-input-stream;
v28: 1.3.80: add exported function client-connection-mode;
v27: 1.3.79: add client SSL handshake lock;
v26: 1.3.78: increase max header size to 16k;
v25: 1.3.77: add switch to enable TCP keepalive for sockets
v24: 1.3.76: do-http-request can return a stream to read the body
v23: 1.3.75: add support for PATCH http verb;
v22: 1.3.74: handle input from stream for MAKE-HTTP-CLIENT-REQUEST;
v21: 1.3.73: allow free worker wait timeout configuration;
v20: 1.3.71: cache reuses previous accept header
v19: 1.3.70: caching of redirects
v18: 1.3.69: automatic caching in the client
v17: 1.3.68: computed-content for do-http-request
v16: 1.3.67: improve redirection for SSL, caching for do-http-request;
v15: 1.3.65: device-read fix for truncated-stream; remove dup auth header;
v14: 1.3.64: proxing https through a tunnel
v13: 1.3.63: do request timing in microseconds
v12: 1.3.62: fix x-www-form-encoded decoding
v11: 1.3.61: Make keep-alive timeout configurable at startup.
v10: 1.3.60: use SNI if available in make-ssl-client-stream;
v9: 1.3.57: fix setting response trailers when :xmit-server-response-body debug option enabled;
v8: 1.3.56: force-output of a prepend-stream supported;
v7: 1.3.55: Add and export a more full set of http response codes;
v6: 1.3.54: Use Allegro's built-in base64 routines when available;
v5: 1.3.53: Add :no-keep-alive strategy;
v4: 1.3.52: optimize compilation for speed;
v3: 1.3.51 add get-request-body-incremental;
v2: 1.3.50: define deflate-stream methods all the time;
v1: 1.3.49: speed up read-sock-line."
  :type :system
  :post-loadable t)

#+(version= 10 0)
(sys:defpatch "aserve" 26
  "v26: 1.3.70: caching of redirects
v25: 1.3.68: computed-content for do-http-request
v24: 1.3.67: improve redirection for SSL, caching for do-http-request;
v23: 1.3.64: proxing https through a tunnel 
v22: 1.3.63: do request timing in microseconds
v21: 1.3.62: fix x-www-form-encoded decoding
v20: 1.3.57: fix setting response trailers when :xmit-server-response-body debug option enabled;
v19: 1.3.56: force-output of a prepend-stream supported;
v18: 1.3.55: Add and export a more full set of http response codes;
v17: 1.3.54: Use Allegro's built-in base64 routines when available;
v16: 1.3.53: Add :no-keep-alive strategy;
v15: 1.3.52: optimize compilation for speed;
v14: 1.3.51 add get-request-body-incremental;
v13: 1.3.50: define deflate-stream methods all the time;
v12: 1.3.49: speed up read-sock-line;
v11: 1.3.45 - avoid races in constructor initialization;
v10: no version change, fix defpatch;
v9: 1.3.44: add :test-ssl argument to start function;
v8: 1.3.43: don't log when client closes connection early;
v7: 1.3.42: internal improvements to server body access;
v6: 1.3.41: fix multi-directory clp file rewriting;
v5: 1.3.40: add methods for socket-bytes-read|written;
v4: 1.3.39: workaround for a bug in decoding chunked request bodies over ssl.
v3: 1.3.38: call make-ssl-client-stream with :method instead of :ssl-method;
v2: 1.3.37: add trailer support
v1: 1.3.36: cosmetic: bump version #; code same as 10.0 initial release."
  :type :system
  :post-loadable t)

#+(version= 9 0)
(sys:defpatch "aserve" 24
  "v24: 1.3.67: improve redirection for SSL, caching for do-http-request;
v23: 1.3.62: proxing https through a tunnel
v22: 1.3.52: optimize compilation for speed;
v21: 1.3.50: define deflate-stream methods all the time;
v20: 1.3.38: call make-ssl-client-stream with :method instead of :ssl-method;
v19: 1.3.37: add trailer support
v18: 1.3.36: add http-only cookies;
v17: 1.3.35: add max-listeners arg to net.aserve:start. Increase max header size to 8192;
v16: 1.3.33: speed up serving of files;
v15: 1.3.32: add no-proxy argument to do-http-request. Fix buggy argument checking for ssl arguments ;
v14: 1.3.30: For https, use defaults of the underlying ssl module;
v13: 1.3.29: proxy now returns content-length;
v12: 1.3.28: Fix bug in retry-on-timeout code in do-http-request;
v11: 1.3.28: Have server send a 408 Request Timeout response on timeout instead of closing connection. Allow client to auto-retry;
v10: 1.3.27: Make clients reading a chunked response detect an unexpected eof instead of busy looping;
v9: 1.3.26: Make do-http-request merge the query part of the uri of requests with the query argument;
v8: 1.3.25: fix keep-alive timeout header: use wserver-header-read-timeout instead of wserver-read-request-timeout;
v7: 1.3.24: Move 100-continue expectation handling until after authorization and an entity has been found. Allow disabling of auto handling per entity;
v6: 1.3.23: fixes socket leak in client when the the writing of the initial headers and body fails;
v5: 1.3.21: new proxy control;
v4: 1.3.20: handle connection reset and aborted errors properly in the client;
v3: add timeout for reading request header;
v2: 1.3.18: introduce allegroserve-error condition object, fix compression with logical pathnames;
v1: 1.3.16: fix freeing freed buffer."
  :type :system
  :post-loadable t)

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

(eval-when (compile) (declaim (optimize (speed 3))))

(eval-when (compile load eval)
  (require :osi)
  (require :autozoom)
  (require :uri)
  #-(and allegro (version>= 6))
  (require :streamc)
  (require :inflate))

(eval-when (compile load eval)
  (defvar sys::*user-warned-about-deflate* nil)
  (handler-case (require :deflate)
    (error (c)
      (if* (null sys::*user-warned-about-deflate*)
	 then (format t "~&NOTE: ~@<the deflate module could not be loaded, so ~
server compression is disabled.  AllegroServe is completely functional ~
without compression.  Original error loading deflate was:~%~a~%~:@>" c)
	      (setq sys::*user-warned-about-deflate* t)))))

(defpackage :net.aserve
  (:use :common-lisp :excl :net.html.generator :net.uri :util.zip)
  (:intern #:wrap-enable-keepalive)
  (:export
   #:allegroserve-error
   #:allegroserve-error-action
   #:allegroserve-error-result
   #:allegroserve-error-identifier
   #:authorize
   #:authorize-proxy-request
   #:authorizer
   #:base64-decode
   #:base64-encode
   #:can-set-trailers-p
   #:chunking-stream-trailers
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
   #:get-request-body-incremental
   #:handle-request
   #:handle-uri		; add-on component..
   #:header-slot-value
   #:http-request  	; class
   #:http-stream
   #:locator		; class
   #:location-authorizer  ; class
   #:location-authorizer-patterns
   #:map-entities
   #:parse-multipart-header
   #:password-authorizer  ; class
   #:process-entity
   #:proxy-control	; class
   #:proxy-control-location
   #:proxy-control-destinations
   #:publish
   #:publish-file
   #:publish-directory
   #:publish-multi
   #:publish-prefix
   #:query-to-form-urlencoded
   #:reply-header-slot-value 
   #:run-cgi-program
   #:seize-request
   #:seize-request-finish
   #:set-basic-authorization
   #:set-trailers
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
   #:request-seized
   #:request-socket
   #:request-uri
   #:request-variable-value
   #:request-wserver
   
   #:request-request-microtime
   
   #:request-reply-code
   #:request-reply-date
   #:request-reply-content-length
   #:request-reply-content-type
   #:request-reply-microtime
   #:request-reply-plist
   #:request-reply-protocol-string
   #:request-reply-strategy
   #:request-reply-stream
   #:request-has-continue-expectation
   
   #:send-100-continue
   
   #:set-cookie-header
   #:shutdown
   #:split-into-words
   #:start
   #:unchunking-trailers
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
   #:wserver-header-read-timeout
   #:wserver-locators
   #:wserver-io-timeout
   #:wserver-log-function
   #:wserver-log-stream
   #:wserver-max-content-length
   #:wserver-response-timeout
   #:wserver-free-worker-timeout
   #:wserver-socket
   #:wserver-vhosts
   #:log-for-wserver

   #:*aserve-version*
   #:*default-aserve-external-format*
   #:*enable-keepalive*
   #:*http-header-read-timeout*
   #:*http-io-timeout*
   #:*http-response-timeout*
   #:*http-free-worker-timeout*
   #:*mime-types*
   #:*response-continue*
   #:*response-switching-protocols*
   #:*response-ok*
   #:*response-created*
   #:*response-accepted*
   #:*response-non-authoritative-information*
   #:*response-no-content*
   #:*response-partial-content*
   #:*response-multiple-choices*
   #:*response-moved-permanently*
   #:*response-found*
   #:*response-see-other*
   #:*response-not-modified*
   #:*response-temporary-redirect*
   #:*response-bad-request*
   #:*response-unauthorized*
   #:*response-forbidden*
   #:*response-not-found*
   #:*response-method-not-allowed*
   #:*response-not-acceptable*
   #:*response-proxy-unauthorized*
   #:*response-request-timeout*
   #:*response-conflict*
   #:*response-gone*
   #:*response-precondition-failed*
   #:*response-uri-too-long*
   #:*response-unsupported-media-type*
   #:*response-requested-range-not-satisfiable*
   #:*response-expectation-failed*
   #:*response-upgrade-required*
   #:*response-internal-server-error*
   #:*response-not-implemented*
   #:*response-service-unavailable*
   #:*wserver*))


(defpackage :net.aserve.client 
  (:use :net.aserve :excl :common-lisp)
  (:import-from :net.aserve #:wrap-enable-keepalive)
  (:export 
   #:client-cache   ; class
   #:client-cache-max-cache-entry-size
   #:client-cache-max-cache-size
   #:client-cache-cache-size
   #:client-cache-lookups
   #:client-cache-alive
   #:client-cache-revalidate
   #:client-cache-validated
   #:client-connection-mode
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
   #:read-response-body
   #:compute-digest-authorization
   #:cookie-item
   #:cookie-item-expires
   #:cookie-item-http-only
   #:cookie-item-name
   #:cookie-item-path
   #:cookie-item-secure
   #:cookie-item-value
   #:cookie-jar     ; class
   #:digest-authorization
   #:digest-password
   #:digest-realm
   #:digest-username
   #:do-http-request
   #:flush-client-cache
   #:http-copy-file
   #:make-http-client-request
   #:read-client-response-headers
   
   #:*cache-size-slop*   ;; variable
   
   ;; computed content exports:
   #:computed-content
   #:get-content-length
   #:get-content-headers
   #:write-content
   #:file-computed-content
   #:stream-computed-content
   ))

;; These functions must be undefined in case new aserve is loaded on
;;   top of older aserve in 8.1. [bug23328] 
#+(version= 8 1)
(eval-when (compile load eval)
  (fmakunbound 'net.aserve::logmess)
  (fmakunbound 'net.aserve::logmess-stream)
  (fmakunbound 'net.aserve.client::read-client-response-headers)
  )

