;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; log.cl
;;
;; See the file LICENSE for the full license governing this code.
;;
;;

;; Description:
;;   iserve's logging

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

(defun log1 (category level message &key (logger *logger*))
  (log1* logger category level message))

(defgeneric log1* (logger category level message)
  (:documentation "This the new, extensible logger interface to which
all others defer. By default, category :access is handled by
log-request* while the rest goes to logmess-stream. Note message is
not necessarily a string: for instance it is a request object
for :access which allows for more flexibility in presentation.")
  (:method (logger category level message)
    (declare (ignore logger))
    (logmess-stream category level message *debug-stream*))
  (:method (logger (category (eql :xmit-server-response-headers)) level message)
    (declare (ignore logger))
    ;; time is :pre or :post depending on whether the headers are
    ;; generated before or after the body
    (destructuring-bind (time string) message
      (logmess-stream category level (format nil "~a ~s" time string)
                      *debug-stream*)))
  (:method (logger (category (eql :access)) level (request http-request))
    (declare (ignore logger level))
    (log-request* request)))

(defvar *enable-logging* t) ; to turn on/off the standard logging method

(defvar *save-commands* nil) ; if true then a stream to which to write commands

(defun logmess (message &optional (format :long))
  (log-for-wserver *wserver* message format))

(defmethod log-for-wserver ((wserver wserver) message format)
  ;; send log message to the default vhost's error stream
  (let ((*debug-stream* (vhost-error-stream (wserver-default-vhost wserver)))
        (*debug-format* format))
    (log1 :aserve :info message)))

(defvar *log-time-zone* 0)

(defmethod logmess-stream (category level message stream
                           &optional (format *debug-format*))
  ;; send the log message to the given stream which should be a
  ;; stream object and not a stream indicator (like t)
  ;; If the stream has a lock use that.
  (declare (ignore level))
  (multiple-value-bind (csec cmin chour cday cmonth cyear)
      (decode-universal-time (get-universal-time) *log-time-zone*)
    (let* ((*print-pretty* nil)
	   (str (ecase format
                  (:long
                   (format
                    nil "[~a] ~a: ~2,'0d/~2,'0d/~2,'0d - ~2,'0d:~2,'0d:~2,'0d - ~a~%"
                    category 
		    ;; Avoid bug24536 (*current-process* may be nil during finalization).
		    (when sys:*current-process* (mp:process-name sys:*current-process*))
                    cmonth cday (mod cyear 100) chour cmin csec
                    message))
                  (:brief
                   (format nil "~2,'0d:~2,'0d:~2,'0d - ~a~%" chour cmin csec
                           message))))
	   (lock (getf (excl::stream-property-list stream) :lock)))
      (if* lock
	 then (mp:with-process-lock (lock)
		(if* (open-stream-p stream)
		   then (write-sequence str stream)
			(finish-output stream)))
	 else (write-sequence str stream)
	      (finish-output stream)))))

(defmethod log-request ((req http-request))
  ;; after the request has been processed, write out log line
  (if* *enable-logging*
     then ;; By default this ends up calling log-request*.
          (log1 :access :info req))
  (if* *save-commands*
     then (multiple-value-bind (ok whole uri-string)
	      (match-re "^[^ ]+\\s+([^ ]+)" (request-raw-request req))
	    (declare (ignore ok whole))
	    (format *save-commands*
		    "((:method . ~s) (:uri . ~s) (:proto . ~s) ~% (:code . ~s)~@[~% (:body . ~s)~]~@[~% (:auth .  ~s)~]~@[~% (:ctype . ~s)~])~%" 
		    (request-method req)
		    uri-string
		    (request-protocol req)
		    (let ((obj (request-reply-code req)))
		      (if* obj
			 then (response-number obj)
			 else 999))
		    (let ((bod (request-request-body req)))
		      (and (not (equal "" bod)) bod))
		    (multiple-value-list (get-basic-authorization req))
		    (header-slot-value req :content-type)))
	  (force-output *save-commands*)))

;; The access log
;;
;; If every worker wrote its line under the log stream's lock, the
;; workers would spend much of their time waiting for that lock.  So
;; a worker just pushes its line onto a lock-free list, and the
;; server's log thread writes all the lines waiting, under the lock,
;; then does one force-output.  On an idle server each line is
;; written as soon as it's logged; on a busy one the lines that
;; arrive while a batch is being written make up the next batch.
;;
;; The simple server (no worker threads) has no log thread, and
;; there log-request* writes the line itself.
;;
;; So that a slow or stuck log stream can't fill memory, a worker
;; waits before adding its line while more than
;; *access-log-pending-limit* lines are waiting, until the log thread
;; takes them.  A stuck stream then stalls the workers, as it would if
;; they wrote the lines themselves.  At shutdown, if the log thread is
;; stuck writing, stop-log-thread stops it, drops the lines not
;; written, and reports how many.

(defvar *access-log-pending-limit* 10000
  ;; most access log lines waiting for the log thread before workers
  ;; wait for it; nil for no limit.  Read on each request.
  )

(defun log-room-p (server)
  ;; true if a worker may add a line to the pending list
  (let ((limit *access-log-pending-limit*))
    (or (null limit)
        (< (the fixnum (car (wserver-log-pending-count server)))
           (the fixnum limit)))))

(defun wait-for-log-room (server)
  ;; wait until the log thread takes the waiting lines, or is stopped
  (let ((room (wserver-log-room server)))
    (loop
      ; close the gate before looking, so that a take after the look
      ; opens it again
      (mp:close-gate room)
      (if* (or (log-room-p server) (null (wserver-log-thread server)))
         then ; reopen it for any other worker that closed it
              (mp:open-gate room)
              (return))
      (mp:process-wait "waiting for room in the access log"
                       #'mp:gate-open-p room))))

(defun log-request* (req)
  (let ((line (cons (request-vhost req) (format-access-log-entry req)))
        (server (and (boundp '*wserver*) *wserver*)))
    (if* (and server (wserver-log-thread server))
       then (if* (not (log-room-p server))
               then (wait-for-log-room server))
            (if* (null (cdr (push-atomic line
                                         (car (wserver-log-pending server)))))
               then ; the first line waiting wakes the log thread
                    (mp:open-gate (wserver-log-gate server)))
            (atomic-incf (car (wserver-log-pending-count server)))
            (if* (null (wserver-log-thread server))
               then ; the log thread was stopped, and may have taken
                    ; its last lines before we pushed ours
                    (write-pending-log-lines server))
       else (write-log-lines (list line)))))

(defun write-log-lines (lines &optional server)
  ;; lines is a list of (vhost . entry), oldest first.  Write each
  ;; entry to its vhost's log stream, taking the stream's lock once and
  ;; doing one force-output for each run of lines to the same stream.
  ;;
  ;; With a server (from the log thread), an error writing to a stream
  ;; loses only that stream's lines: the rest of the run, and its later
  ;; lines in this batch, are dropped, and each stream that failed is
  ;; reported once with the number of lines lost.  Lines for other
  ;; streams are still written.  Without a server the error is
  ;; signalled, as when a worker wrote its own line.
  (let ((failed nil)) ; (stream count condition) for each failed stream
    (loop
      (if* (null lines) then (return))
      (let* ((stream (vhost-log-stream (caar lines)))
             (failure (assoc stream failed :test #'eq)))
        (if* failure
           then ; this stream failed earlier in the batch
                (pop lines)
                (incf (second failure))
           else (let ((lock (and (streamp stream)
                                 (getf (excl::stream-property-list stream)
                                       :lock)))
                      (run lines))
                  (flet ((write-run ()
                           (loop
                             (format stream "~a~%" (cdr (pop lines)))
                             (if* (or (null lines)
                                      (not (eq stream
                                               (vhost-log-stream
                                                (caar lines)))))
                                then (return)))
                           (force-output stream)))
                    (flet ((write-locked ()
                             (if* lock
                                then (mp:with-process-lock (lock)
                                       ; in case stream switched out while
                                       ; we weren't busy get the stream
                                       ; again
                                       (setq stream
                                         (vhost-log-stream (caar lines)))
                                       (write-run))
                                else (write-run))))
                      (if* server
                         then (handler-case (write-locked)
                                (error (c)
                                  ; drop the rest of the run
                                  (loop
                                    (if* (or (null lines)
                                             (not (eq stream
                                                      (vhost-log-stream
                                                       (caar lines)))))
                                       then (return))
                                    (pop lines))
                                  (push (list stream
                                              (length (ldiff run lines))
                                              c)
                                        failed)))
                         else (write-locked))))))))
    (dolist (failure (reverse failed))
      (destructuring-bind (stream count condition) failure
        (ignore-errors
         (log-for-wserver
          server
          (let ((*print-readably* nil))
            (format nil "error writing the access log to ~s: ~a; up to ~d ~
                         lines lost"
                    stream condition count))
          :long))))))

(defun take-pending-log-lines (server)
  ;; remove and return the lines waiting for the log thread, oldest first
  (let ((pending (wserver-log-pending server)))
    (loop
      (let ((lines (car pending)))
        (if* (atomic-conditional-setf (car pending) nil lines)
           then (if* lines
                   then (atomic-decf (car (wserver-log-pending-count server))
                                     (length lines))
                        (mp:open-gate (wserver-log-room server)))
                (return (nreverse lines)))))))

(defun write-pending-log-lines (server)
  (let ((lines (take-pending-log-lines server)))
    (if* lines
       then (write-log-lines lines server))))

(defvar *log-thread-stop-timeout* 10
  ;; seconds shutdown waits for the log thread to finish, and again
  ;; after interrupting it
  )

(defvar *log-thread-batch* nil
  ;; bound in the log thread: the number of lines it's writing, or nil
  ;; when it isn't writing.  stop-log-thread's interrupt reads it.
  )

(defun log-thread (server done)
  (let ((gate (wserver-log-gate server))
        (*log-thread-batch* nil))
    (unwind-protect
        (catch 'stop-log-thread ; thrown by stop-log-thread's interrupt
          (loop
            (mp:process-wait "waiting for log lines" #'mp:gate-open-p gate)
            ; close it before taking the lines, so that a line pushed
            ; after the take opens it again
            (mp:close-gate gate)
            (let ((lines (take-pending-log-lines server)))
              (if* lines
                 then (setq *log-thread-batch* (length lines))
                      (write-log-lines lines server)
                      (setq *log-thread-batch* nil)))
            (if* (not (eq (wserver-log-thread server) sys:*current-process*))
               then (return))))
      (mp:open-gate done))))

(defun start-log-thread (server)
  (let ((*wserver* server)
        (done (mp:make-gate nil)))
    (let ((proc (mp:make-process
                 :name (format nil "~A-log-~d"
                               (if* *log-wserver-name*
                                  then (wserver-name server)
                                  else "aserve")
                               (atomic-incf *thread-index*))
                 :initial-bindings (initial-bindings))))
      (mp:process-preset proc #'log-thread server done)
      (setf (mp:process-keeps-lisp-alive-p proc) nil)
      ; set these before it runs, since it exits when it's not the
      ; server's log thread
      (setf (wserver-log-thread-done server) done)
      (setf (wserver-log-thread server) proc)
      (mp:process-enable proc))))

(defun stop-log-thread (server)
  ;; stop the log thread, after it writes the lines it has, then write
  ;; any lines pushed after it stopped.
  ;;
  ;; If it hasn't finished after *log-thread-stop-timeout* seconds, it's
  ;; stuck writing to a log stream, or just slow.  Interrupt it: if
  ;; it's writing, it throws out of the write and exits.  If it has
  ;; exited without being stuck, write the rest as usual.  Otherwise
  ;; the stream isn't taking lines, so drop the ones waiting rather than
  ;; block on it too, and report what was lost.  A thread blocked in an
  ;; operating system write can't take the interrupt until the write
  ;; returns, so then shutdown gives up on it.
  (let ((proc (wserver-log-thread server)))
    (if* proc
       then (setf (wserver-log-thread server) nil)
            (mp:open-gate (wserver-log-gate server))
            ; workers waiting for room write their lines themselves
            (mp:open-gate (wserver-log-room server))
            (let ((done (wserver-log-thread-done server))
                  (timeout *log-thread-stop-timeout*)
                  (stuck (list nil)))
              (if* (not (mp:process-wait-with-timeout
                         "stopping the log thread" timeout
                         #'mp:gate-open-p done))
                 then (ignore-errors
                       (mp:process-interrupt
                        proc
                        #'(lambda ()
                            ;; throw only out of a write, never between
                            ;; taking lines and writing them
                            (let ((batch *log-thread-batch*))
                              (if* batch
                                 then (setf (car stuck) batch)
                                      (throw 'stop-log-thread nil))))))
                      (let ((exited (mp:process-wait-with-timeout
                                     "interrupting the log thread" timeout
                                     #'mp:gate-open-p done)))
                        (if* (or (not exited) (car stuck))
                           then (report-unwritten-log-lines
                                 server
                                 (and exited (car stuck))
                                 (length (take-pending-log-lines server)))
                                (return-from stop-log-thread nil)))))))
  (write-pending-log-lines server))

(defun report-unwritten-log-lines (server batch waiting)
  ;; report the access log lines that shutdown dropped.  batch is the
  ;; number of lines the log thread was writing when it was stopped, or
  ;; nil if it didn't respond.  Reported from a thread of its own, since
  ;; the error stream may be the same stuck stream.
  (let ((message
         (if* batch
            then (format nil "access log not written at shutdown: the log ~
                              thread was stuck writing a batch of ~d lines, ~
                              and ~d more were waiting"
                         batch waiting)
            else (format nil "access log not written at shutdown: the log ~
                              thread did not respond; its batch may be ~
                              incomplete, and ~d more lines were waiting"
                         waiting))))
    (let ((proc (mp:process-run-function "aserve access log report"
                  #'(lambda ()
                      (ignore-errors
                       (log-for-wserver server message :long))))))
      (setf (mp:process-keeps-lisp-alive-p proc) nil))))

(defun format-access-log-entry (req)
  (let* ((ipaddr (socket:remote-host (request-socket req)))
         (time   (request-reply-date req))
         (code   (let ((obj (request-reply-code req)))
                   (if* obj
                      then (response-number obj)
                      else 999)))
         (length (or (request-reply-content-length req)
                     #+(and allegro (version>= 6))
                     (excl::socket-bytes-written 
                      (request-socket req)))))
    (format nil "~A~A~a - - [~a] ~s ~s ~s"
            (if* *log-wserver-name* 
               then (wserver-name *wserver*) 
               else "")
            (if* *log-wserver-name* 
               then " " 
               else "")
            (socket:ipaddr-to-dotted ipaddr)
            (maybe-universal-time-to-date time)
            (request-raw-request req)
            code
            (or length -1))))
    
(defun log-proxy (uri level action extra)
  ;; log information from the proxy module
  ;;
  (logmess 
   (format nil "~a ~d ~a ~a~@[ ~s~]"
	   ;; Avoid bug24536 (*current-process* may be nil during finalization). 
	   (when mp:*current-process*
	     (or (getf (mp:process-property-list mp:*current-process*)
		       'short-name)
		 (mp:process-name mp:*current-process*)))
	   level
	   action
	   (if* (stringp uri) 
	      then uri 
	      else (net.uri:render-uri uri nil))
	   extra)
   :brief))
