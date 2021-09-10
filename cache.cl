;; -*- mode: common-lisp; package: net.aserve.client -*-
;;
;; cache.cl
;;
;; See the file LICENSE for the full license governing this code.
;;
;;

;; Description:
;;   cache for http client code

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve.client)


(defclass cache-entry ()
  (;; url in string form
   (uri :initarg :url :reader centry-uri)
   
   ;; the result of doing the full http call. 
   ;; This may be updated when validation occurs.
   ;;
   ;; the date string from the response
   (date   :initarg :date    :accessor centry-date)
   (last-modified :initarg :last-modified :accessor centry-last-modified)
   
   (body   :initarg :body    :initform nil :accessor centry-body)
   (code   :initarg :code    :initform 0   :accessor centry-code)
   (headers :initarg :headers :initform nil :accessor centry-headers)
   
   ;; ut when this entry expires and must be revalidated
   (expires :initarg :expires :accessor centry-expires
            :initform 0)
   
   (etag    :initarg :etag  :accessor centry-etag
            :initform nil)
   
   ;; cookies sent with request
   (cookie-string :initarg :cookie-string 
                  :accessor centry-cookie-string
                  :initform nil)
   
   ;; accept header value when request made
   (accept        :initarg :accept
                  :reader  centry-accept
                  :initform nil)
   
   ;; if true then we always validate this and don't use
   ;; the expiration date
   (must-validate :initarg :must-validate
                  :accessor centry-must-validate
                  :initform nil)
   
   ;; last time this centry was consulted
   (last-used  :initform (get-universal-time)
               :accessor centry-last-used)
   
   ;; lock for this entry
   (lock :initform (mp:make-process-lock) :reader centry-lock)
   
   ))

(defmacro with-centry-lock ((cache-entry) &body body)
  `(mp:with-process-lock ((centry-lock ,cache-entry))
     ,@body))

(defclass client-cache ()
  ;; A cache over calls to do-http-request
  ((lock :initform (mp:make-process-lock)
         :reader client-cache-lock)
   
   ;; a table mapping url (string) to a list of cache-entries
   ;; which have the same centry-uri but different centry-cookies
   (table :initform (make-hash-table :test #'equal)
          :reader client-cache-table)
   
   ;; parameters
   (max-cache-entry-size :initform 1000000 ; one million bytes
                          :initarg :max-cache-entry-size
                          :accessor client-cache-max-cache-entry-size)
   (max-cache-size :initform 10000000 ; 10mb
                   :initarg :max-cache-size
                   :accessor client-cache-max-cache-size)
   
   
   (auto-cache-codes :initform nil
                     :initarg :auto-cache-codes
                     :accessor client-cache-auto-cache-codes)
   
   (auto-cache-seconds :initform nil
                       :initarg :auto-cache-seconds
                       :accessor client-cache-auto-cache-seconds)
  
   ;; statistics
   ;; number of body bytes cached
   (cache-size   :initform 0
                 :accessor client-cache-cache-size)
   
   ;; check to see if response is in the cache
   (lookups :initform 0
            :accessor client-cache-lookups)
   
   ;; cached value is alive and returned immediately
   (alive   :initform 0
            :accessor client-cache-alive)
   
   ;; response needs to be revalidated
   (revalidate :initform 0
               :accessor client-cache-revalidate)
   
   ;; validation done and value is still valid
   (validated  :initform 0
               :accessor client-cache-validated)
   
   ))

(defmacro with-client-cache-lock ((client-cache) &rest body)
  `(mp:with-process-lock ((client-cache-lock ,client-cache))
     ,@body))


;; we'll allow the cache to grow to this many bytes beyond
;; the desired size (client-cache-max-size) and when it grows beyond
;; that we'll reduce the cache to this many bytes below the 
;; desired size.  This will prevent us from having to remove
;; items in the cache every time we add an entry once the
;; client-cache-max-size is reached.
(defparameter *cache-size-slop* 100000) ; roughly 100k


(defun lookup-in-client-cache (cache uri cookies accept method)
  ;; locate cached value and possibily 
  ;; validate to verify it's up to date.
  ;; 
  ;; If there was a cache entry for this uri and that entry
  ;; is either valid now or can be revalidated then return
  ;; the cache-entry with the correct value for the HTTP request.
  ;;
  ;; If there was no cache entry or the entry cannot be revalidated
  ;; then return nil.
  ;;

  (let ((cookie-string (and cookies (compute-cookie-string uri cookies)))
        (centry))
    
    (if* (typep uri 'net.uri:uri)
       then (setq uri (net.uri:render-uri uri nil)))

    (with-client-cache-lock (cache)
      (incf (client-cache-lookups cache))
      (dolist (this-centry (gethash uri (client-cache-table cache)))
        (if* (and (equal cookie-string (centry-cookie-string this-centry))
                  (equal accept        (centry-accept this-centry)))
           then (return (setq centry this-centry)))))
  
    (if* (null centry)
       then ;; fail
            (return-from lookup-in-client-cache nil))

    (with-centry-lock (centry)
      ;; check if entry is still valid in which case we can use it
      (let ((now 
             (setf (centry-last-used centry) 
               (get-universal-time))))
        (if* (and (not (centry-must-validate centry))
                  (>= (centry-expires centry) now))
           then ; expires in the future, we can use this
                (with-client-cache-lock (cache)
                  (incf (client-cache-alive cache)))
                (return-from lookup-in-client-cache centry)))
  
      (let ((new-headers))
        (if* (centry-etag centry)
           then (push `("if-none-match" . ,(centry-etag centry)) new-headers)
           else (push `("if-modified-since" . 
                                            ,(or (centry-last-modified centry)
                                                 (centry-date centry)))
                      new-headers))

        (with-client-cache-lock (cache)
          (incf (client-cache-revalidate cache)))
        
        (multiple-value-bind (body code headers)
            (do-http-request uri
              :method method
              :accept (or (centry-accept centry) "*/*")
              :headers new-headers)
          (setq centry 
            (update-cache-entry-from-response cache centry body code headers))))

      centry)))


(defparameter *cacheable-codes*
    '(#.(net.aserve::response-number *response-ok*)
      #.(net.aserve::response-number *response-non-authoritative-information*)
      #.(net.aserve::response-number *response-multiple-choices*)
      #.(net.aserve::response-number *response-found*) ;; aka moved-temporarily
      #.(net.aserve::response-number *response-moved-permanently*)
      #.(net.aserve::response-number *response-gone*)))

(defun update-cache-entry-from-response (cache centry body code headers)
  ;; After issuing an HTTP request for validating the cache-entry for
  ;; a uri, we process the response appropriately.
  ;; 
  ;; Return the cache-entry given to us after modifying it appropriately.
  ;; In the unusual circumstance where we were given a non-cacheable response
  ;; to a uri which previously responded with a cacheable response we
  ;; eliminate the cache entry and return nil.
  (if* (eql code #.(net.aserve::response-number *response-not-modified*))
     then ;; update expiration date
          (with-client-cache-lock (cache)
            (incf (client-cache-validated cache)))
          (update-expiration-time cache centry headers)
          centry
   elseif (member code *cacheable-codes*)
     then (update-in-place-centry cache centry body code headers)
          centry
     else ; eliminate this cache entry
          (eliminate-cache-entry cache centry)
          nil))

(defun update-expiration-time (cache centry headers)
  ;; compute new expiration date
  (let* ((date (header-value headers :date))
         (cache-control (get-cache-control-values headers))
         (last-modified (header-value headers :last-modified))
         (expires  (header-value headers :expires))
         (max-age (and cache-control
                       (let ((max (cdr (assoc "max-age"
                                              cache-control
                                              :test #'equal))))
                         (if* max
                            then (or (ignore-errors (parse-integer max))
                                     ;; the only safe default is 0 here
                                     0))))))
          
    (setf (centry-last-modified centry) (or last-modified date))
    (setf (centry-date centry) date)
    (if* (and (null max-age)
              (null expires)
              (member (centry-code centry) (client-cache-auto-cache-codes cache)))
       then (setq max-age (client-cache-auto-cache-seconds cache)))
    (if* max-age
       then (setf (centry-expires centry)
              (+ (get-universal-time) max-age))
     elseif expires
       then (setf (centry-expires centry)
              (net.aserve::date-to-universal-time expires)))))
           

(defun get-cache-control-values (headers)
  (let ((ent (header-value headers :cache-control)))
    (if* ent 
       then (net.aserve::parse-header-line-equals ent))))

(defun eliminate-cache-entry (cache centry)
  (with-client-cache-lock (cache)
    (let ((ents (gethash (centry-uri centry) (client-cache-table cache))))
      (if* ents
         then (setf (gethash (centry-uri centry) (client-cache-table cache))
                (remove-cache-entry-from-list cache centry ents))))))

(defun remove-cache-entry-from-list (cache centry ents)
  ;; remove the cache entry centry from the list ents and account for
  ;; the reduction in the amount of data in the cache.
  ;; Return the updated list.
  (decf (client-cache-cache-size cache) (length (centry-body centry)))
  (remove centry ents :test #'eq))
  
(defun update-in-place-centry (cache centry body code headers)
  ;; we got a whole new response while validating our cache
  ;; entry
  (let ((cache-control (get-cache-control-values headers))
        (must-validate))
    
    (multiple-value-setq (headers must-validate)
      (remove-no-cache-headers cache-control headers))

    (with-client-cache-lock (cache)
      (incf (client-cache-cache-size cache) 
            (- (length body) (length (centry-body centry)))))
    (setf (centry-body centry) body)
    (setf (centry-code centry) code)
    (setf (centry-headers centry) headers)
    (setf (centry-must-validate centry) must-validate)
    (update-expiration-time cache centry headers)
    (ensure-cache-size-ok cache :preserve (list centry))
    ))


(defun remove-no-cache-headers (cache-control headers)
  ;; remove any headers marked as no-cache and return the new
  ;; headers list
  ;; return second value of t iff there's a no-cache spec with no value meaning
  ;; that this cache entry must always be revalidated
  (let (must-validate)
    (dolist (keyval cache-control)
      (destructuring-bind (key . value) keyval
        (if* (equalp key "no-cache") 
           then (if* value
                   then (setq headers
                          (remove (net.aserve::header-keywordify value) 
                                  headers :key #'car))
                   else (setq must-validate t)))))
    (values headers must-validate)))
  
(defun insert-into-cache (cache creq body)
  ;; create a cache entry for this response if 
  ;; permitted
  
  (let* ((uri (client-request-uri creq))
         (headers (client-request-headers creq))
         (code  (client-request-response-code creq))
         (cache-control (get-cache-control-values headers))
         (cookie-string (client-request-cookie-string creq)))
    
    (if* (assoc "no-store" cache-control :test #'equalp)
       then ;; we're not allowed to put this in the cache
            (return-from insert-into-cache nil))
    
    (if* (typep uri 'net.uri:uri)
       then (setq uri (net.uri:render-uri uri nil)))
    
  
    (let ((centry (make-instance 'cache-entry
                    :url uri
                    :etag (strip-weak-indicator (header-value headers :etag))
                    :accept (client-request-accept creq)
                    :cookie-string cookie-string)))
      (update-in-place-centry cache
                              centry
                              body
                              code
                              headers)

      ;; insert this cache entry into the cache
      ;; it's highly unlikely that we'll find it already there
      ;; because someone added it between the time we didn't 
      ;; find it in the lookup and now want to insert it,but 
      ;; if we do we'll remove the existing one to make way for
      ;; our new one
      (with-client-cache-lock (cache)
        (let ((ents (gethash uri (client-cache-table cache)))
              (to-remove))
          ;; find all matching entries
          (dolist (centry ents)
            (if* (and (equal cookie-string (centry-cookie-string centry))
                      (equal (client-request-accept creq) (centry-accept centry)))
               then (push centry to-remove)))
          ;; remove all matching entries, it will be very
          ;; rare for there to be any to remove
          (if* to-remove
             then (dolist (centry to-remove)
                    (setq ents (remove-cache-entry-from-list cache centry ents))))
          ;; add our new entry, size already added
          (setf (gethash uri (client-cache-table cache))
            (cons centry ents)))))))

(defun strip-weak-indicator (string)
  ;; An entity tag can be prefixed with W/ and this must be
  ;; removed in order to do a subsequent comparsion.
  (if* (and string (prefixp "W/" string))
     then (subseq string 2)
     else string))


(defun flush-client-cache (cache &key expired all)
  ;; remove from the cache either all items or expired items
  (let ((to-remove)
        (now (get-universal-time)))
    (with-client-cache-lock (cache)
      (maphash #'(lambda (key centries)
                   (declare (ignore key))
                   (dolist (centry centries)
                     (if* (or all 
                              (and expired (> now (centry-expires centry))))
                        then (push centry to-remove))))
               (client-cache-table cache)))
    (dolist (centry to-remove)
      (eliminate-cache-entry cache centry))))

(defun ensure-cache-size-ok (cache &key preserve)
  ;; ensure that the size of the cache is within bounds
  ;; if not remove entries but never remove any entry in
  ;; the preserve list
  (let (shrink-to)
    (with-client-cache-lock (cache)
      (let ((max-size (client-cache-max-cache-size cache))
            (size     (client-cache-cache-size cache)))
        (if* (> (- size max-size) *cache-size-slop*)
           then (setq shrink-to (- max-size *cache-size-slop*)))))
    (if* shrink-to
       then (recover-bytes-from-cache cache shrink-to
                                      :preserve preserve))))
              
                     
(defun recover-bytes-from-cache (cache desired-size &key preserve)
  ;; recover space from the cache until the size is less than
  ;; or equal to desired-size.  Do not remove any cache entries
  ;; in the preserve list
  (let (ents)
    
    (with-client-cache-lock (cache)
      (maphash #'(lambda (key centries)
                   (declare (ignore key))
                   (setq ents (append centries ents)))
               (client-cache-table cache)))
      
    ;; sort by oldest to newest entry
    (setq ents (sort ents #'(lambda (centry1 centry2)
                              (< (centry-last-used centry1)
                                 (centry-last-used centry2)))))
      
    (loop
      (if* (null ents) then (return))
      (if* (> (client-cache-cache-size cache) desired-size)
         then (let* ((centry (pop ents)))
                (if* (not (member centry preserve :test #'eq))
                   then (eliminate-cache-entry cache centry)))
         else (return)))))
