;;
;; load up the webaction code.
;;

(require :aserve)

	 
(defvar *loadswitch* :compile-if-needed)
(defparameter *webactions-root* (directory-namestring *load-pathname*))

(defparameter *webactions-files*
    '("clpage"
      "webact"
      "websession"
      
      "clpcode/clp"
      "clpcode/wa"
      "clpcode/http"
      "clpcode/time"))


(with-compilation-unit nil
  (dolist (file *webactions-files*)
    (case *loadswitch*
      (:compile-if-needed (compile-file-if-needed 
			   (format nil "~a~a.cl" *webactions-root* file)))
      (:compile (compile-file 
		 (merge-pathnames (format nil "~a~a.cl" 
					  *webactions-root* file))))
      (:load nil))
    (load (format nil "~a~a.fasl" *webactions-root* file))))

    
(defun make-webactions.fasl ()
  (wa-copy-files-to *webactions-files* "webactions.fasl" 
		    :root *webactions-root*)
  ; in place for require
  (sys:copy-file (concatenate 'string
	       *webactions-root* "webactions.fasl")
		 "code/webactions.fasl"
		 :overwrite t)
	       
  )



(defun wa-copy-files-to (files dest &key (root ""))
  ;; copy the contents of all files to the file named dest.
  ;; append .fasl to the filenames (if no type is present)
  
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (p (concatenate 'string root dest)
		     :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
      (dolist (file files)
	(setq file (concatenate 'string root file))
	(if* (and (null (pathname-type file))
		  (not (probe-file file)))
	   then (setq file (concatenate 'string file  ".fasl")))
	(with-open-file (in file :element-type '(unsigned-byte 8))
	  (loop
	    (let ((count (read-sequence buffer in)))
	      (if* (<= count 0) then (return))
	      (write-sequence buffer p :end count))))))))
