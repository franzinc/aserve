(in-package :neo)


(defun logmess (message)
  (multiple-value-bind (csec cmin chour cday cmonth)
      (decode-universal-time (get-universal-time))
    
    (format t "~a: ~2,'0d/~2,'0d/99 - ~2,'0d:~2,'0d:~2,'0d - ~a~%"
	    (mp:process-name sys:*current-process*)
	    cmonth cday chour cmin csec
	    message)))






(defun log-timed-out-request-read (socket)
  (logmess (format nil "No request read from address ~a" 
		   (socket::ipaddr-to-dotted (socket::remote-host socket)))))
