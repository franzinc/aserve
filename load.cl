;; load in iServe
;;
;; $Id: load.cl,v 1.23 2000/03/27 20:47:48 jkf Exp $
;;

(defvar *loadswitch* :compile-if-needed)
(defparameter *iserve-root* (directory-namestring *load-truename*))

(defparameter *iserve-files* 
    '("htmlgen/htmlgen"
      "macs"
      "main"
      "parse"
      "decode"
      "publish"
      "authorize"
      "log" 
      "client"
      ))

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
      "examples/iservelogo.gif"
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



;; after running this function you'll have a lisp binary
;; with the webserver loaded.
;; you can cd to iserveserver and start with
;;   nohup ./iserverserver -f ../examples/examples.fasl >& errs &
;; and it will run the server in the background, serving the iserve
;; examples.
;;
(defun makeapp ()
  (run-shell-command "rm -fr iserveserver")
  (make-iserve.fasl)
  (generate-application
   "iserveserver"
   "iserveserver/"
   '(:sock :process :defftype :foreign 
     :ffcompat "iserve.fasl")
   ; strange use of find-symbol below so this form can be read without
   ; the net.iserve package existing
   :restart-init-function (find-symbol (symbol-name :start-cmd) :net.iserve)
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
   :suppress-allegro-cl-banner nil))


(defun make-distribution ()
  ;; make a distributable version of iserve
  (run-shell-command 
   (format nil "rm -fr ~aiserve-dist" *iserve-root*))
   
  (run-shell-command 
   (format nil "mkdir ~aiserve-dist ~aiserve-dist/doc ~aiserve-dist/examples"
	   *iserve-root*
	   *iserve-root*
	   *iserve-root*))
   
  (copy-files-to *iserve-files* "iserve.fasl" :root *iserve-root*)
  (copy-files-to '("htmlgen/htmlgen.html")
		 "iserve-dist/doc/htmlgen.html"
		 :root *iserve-root*
		 )
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
		   (format nil "iserve-dist/~a" file)
		   :root *iserve-root*)))


;; checklist for publishing iserve source for source-master:
;; 1. incf version number, edit ChangeLog and commit
;; 2. make clean
;; 3. start lisp and load iserve/load to compile all files, there should
;;    be no warnings.
;; 4. start the server (net.iserve:start :port 8000) 
;;	and run through the samples from Netscape and IE
;; 5. :cl test/t-iserve
;; 6. (make-src-distribution)
;; 7. (ftp-publish-src)
;; 8. (publish-docs)   ;  to put latest docs on iserve web page
;; 9. on beast run /fi/sa/bin/iserve-sync
;; 


(defparameter iserve-version-name 
    (apply #'format nil "iserve-~d.~d.~d" 
	   (symbol-value
	    (find-symbol 
	     (symbol-name :*iserve-version*)
	     :net.iserve))))


(defun make-iserve.fasl ()
  (copy-files-to *iserve-files* "iserve.fasl" :root *iserve-root*))



(defun make-src-distribution ()
  ;; make a source distribution of iserve
  ;;
    
  (run-shell-command 
   (format nil "rm -fr ~aiserve-src" *iserve-root*))
    
  (run-shell-command 
   (format nil "mkdir ~aiserve-src ~aiserve-src/~a ~aiserve-src/~a/htmlgen"
	   *iserve-root*
	   
	   *iserve-root*
	   iserve-version-name
	   
	   *iserve-root*
	   iserve-version-name
	   ))
  
  (run-shell-command 
   (format nil "mkdir ~aiserve-src/~a/doc ~aiserve-src/~a/examples"
	   *iserve-root*
	   iserve-version-name
	   
	   *iserve-root*
	   iserve-version-name))
	   
  (dolist (file (append (mapcar #'(lambda (file) (format nil "~a.cl" file))
				*iserve-files*)
			*iserve-other-files*))
    (copy-files-to
     (list file)
     (format nil "iserve-src/~a/~a" iserve-version-name file)
     :root *iserve-root*)))


(defun ftp-publish-src ()
  ;; assuming tha we've made the source distribution, tar it
  ;; and copy it to the ftp directory
  (run-shell-command
   (format nil "(cd ~aiserve-src ; tar cfz ~a.tgz ~a)"
	   *iserve-root*
	   iserve-version-name
	   iserve-version-name))
  (run-shell-command 
   (format nil "cp ~aiserve-src/~a.tgz /net/candyman/home/ftp/pub/iserve"
	   *iserve-root*
	   iserve-version-name)))

(defun publish-docs ()
  ;; copy documentation to the external web site
  (run-shell-command
   (format nil "cp ~ahtmlgen/htmlgen.html ~adoc/iserve.html ~adoc/tutorial.html /net/cobweb/www/people/jkf/iserve"
	   *iserve-root*
	   *iserve-root*
	   *iserve-root*)))
	   
	    
  

(defun copy-files-to (files dest &key (root ""))
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
