;; load in iServe
;;
;; $Id: load.cl,v 1.12.2.6 2000/03/15 20:38:18 jkf Exp $
;;

(defvar *loadswitch* :compile-if-needed)
;(require :defftype)

(defparameter *iserve-files* 
    '("htmlgen/htmlgen"
      "macs"
      "main"
      "parse"
      "decode"
      "publish"
      "authorize"
      "log" ))

(defparameter *iserve-other-files*
    ;; other files that make up the iserve dist
    '("readme.txt"
      "source-readme.txt"
      "ChangeLog"
      "license-lgpl.txt"
      "examples/examples.cl"
      "examples/foo.txt"
      "examples/fresh.jpg"
      "examples/prfile9.jpg"
      "examples/tutorial.cl"
      "load.cl"
      "doc/iserve.html"
      "doc/tutorial.html"
      "htmlgen/htmlgen.html"
      ))

(defparameter *iserve-examples*
    '("examples/examples"
      ))


(with-compilation-unit  nil
  (dolist (file (append *iserve-files* *iserve-examples*))
    (case *loadswitch*
      (:compile-if-needed (compile-file-if-needed 
			   (merge-pathnames (format nil "~a.cl" file)
					    *load-truename*)))
      (:compile (compile-file 
		 (merge-pathnames (format nil "~a.cl" file)
				  *load-truename*)))
      (:load nil))
    (load (merge-pathnames 
	   (format nil "~a.fasl" file)
	   *load-truename*))))



(defun makeapp ()
  (run-shell-command "rm -fr iserveserver")
  (generate-application
   "iserveserver"
   "iserveserver/"
   '(:sock :process :defftype :foreign :ffcompat "loadonly.cl" "load.cl")
   :restart-init-function 'net.iserve::start-cmd
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
  ;; make a distributable version of iserve
  (run-shell-command "rm -fr iserve-dist")
  (run-shell-command "mkdir iserve-dist iserve-dist/doc iserve-dist/examples")
  (copy-files-to *iserve-files* "iserve.fasl")
  (copy-files-to '("htmlgen/htmlgen.html")
		 "iserve-dist/htmlgen.html")
  (dolist (file '("iserve.fasl"
		  "doc/iserve.html"
		  "doc/tutorial.html"
		  "readme.txt"
		   "examples/examples.cl"
		   "examples/examples.fasl"
		   "examples/foo.txt"
		   "examples/fresh.jpg"
		   "examples/prfile9.jpg"))
    (copy-files-to (list file)
		   (format nil "iserve-dist/~a" file))))
		

(defun make-src-distribution ()
  ;; make a source distribution of iserve
  ;;
  (run-shell-command "rm -fr iserve-src")
  (run-shell-command "mkdir iserve-src iserve-src/iserve iserve-src/iserve/htmlgen")
  (run-shell-command "mkdir iserve-src/iserve/doc iserve-src/iserve/examples")
  (dolist (file (append (mapcar #'(lambda (file) (format nil "~a.cl" file))
				*iserve-files*)
			*iserve-other-files*))
    (copy-files-to
     (list file)
     (format nil "iserve-src/iserve/~a" file))))

  
  

(defun copy-files-to (files dest)
  ;; copy the contents of all files to the file named dest.
  ;; append .fasl to the filenames (if no type is present)
  
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (p dest :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
      (dolist (file files)
	(if* (and (null (pathname-type file))
		  (not (probe-file file)))
	   then (setq file (concatenate 'string file  ".fasl")))
	(with-open-file (in file :element-type '(unsigned-byte 8))
	  (loop
	    (let ((count (read-sequence buffer in)))
	      (if* (<= count 0) then (return))
	      (write-sequence buffer p :end count))))))))
