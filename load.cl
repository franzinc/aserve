;; load in neo

(defvar *loadswitch* :compile-if-needed)
;(require :defftype)

(defparameter *neo-files* 
    '("../htmlgen/htmlgen"
      "macs"
      "main"
      "parse"
      "decode"
      "publish"
      "log" ))

(defparameter *neo-examples*
    '("examples"))


(dolist (file (append *neo-files* *neo-examples*))
  (case *loadswitch*
    (:compile-if-needed (compile-file-if-needed (format nil "~a.cl" file)))
    (:compile (compile-file (format nil "~a.cl" file)))
    (:load nil))
  (load (format nil "~a.fasl" file)))

      

(defun makeapp ()
  (run-shell-command "rm -fr neoserver")
  (generate-application
   "neoserver"
   "neoserver/"
   '(:sock :process :defftype :foreign :ffcompat "loadonly.cl" "load.cl")
   :restart-init-function 'neo::start-cmd
   :application-administration '(:resource-command-line
				 ;; Quiet startup:
				 "-Q")
   :read-init-files nil
   :print-startup-message nil
   :purify nil
   :include-compiler nil
   :include-devel-env nil
   :include-debugger t
   :include-tpl t
   :include-ide nil
   :discard-arglists t
   :discard-local-name-info t
   :discard-source-file-info t
   :discard-xref-info t
 
   :ignore-command-line-arguments t
   :suppress-allegro-cl-banner t))


(defun make-distribution ()
  ;; make a distributable version of neo
  (run-shell-command "rm -fr neo-dist")
  (run-shell-command "mkdir neo-dist")
  (copy-files-to *neo-files* "neo.fasl")
  (copy-files-to '("../htmlgen/htmlgen.html")
		 "neo-dist/htmlgen.html")
  (dolist (file '("neo.fasl"
		  "neo.html"
		  "readme.txt"
		   "examples.cl"
		   "examples.fasl"
		   "foo.txt"
		   "fresh.jpg"
		   "prfile9.jpg"))
    (copy-files-to (list file)
		   (format nil "neo-dist/~a" file))))
		
  
  

(defun copy-files-to (files dest)
  ;; copy the contents of all files to the file named dest.
  ;; append .fasl to the filenames (if no type is present)
  
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (p dest :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
      (dolist (file files)
	(if* (null (pathname-type file))
	   then (setq file (concatenate 'string file  ".fasl")))
	(with-open-file (in file :element-type '(unsigned-byte 8))
	  (loop
	    (let ((count (read-sequence buffer in)))
	      (if* (<= count 0) then (return))
	      (write-sequence buffer p :end count))))))))
