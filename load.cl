;; load in aserve
;;
;; $Id: load.cl,v 1.31 2000/05/16 14:01:25 jkf Exp $
;;

(defvar *loadswitch* :compile-if-needed)
(defparameter *aserve-root* (directory-namestring *load-truename*))

(defparameter *aserve-files* 
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

(defparameter *aserve-other-files*
    ;; other files that make up the aserve dist
    '("readme.txt"
      "source-readme.txt"
      "ChangeLog"
      "htmlgen/ChangeLog"
      "license-lgpl.txt"
      "license-allegroserve.txt"
      "examples/examples.cl"
      "examples/foo.txt"
      "examples/fresh.jpg"
      "examples/prfile9.jpg"
      "examples/tutorial.cl"
      "examples/aservelogo.gif"
      "load.cl"
      "test/t-aserve.cl"
      "doc/aserve.html"
      "doc/tutorial.html"
      "doc/htmlgen.html"
      ))

(defparameter *aserve-examples*
    '("examples/examples"
      ))


(with-compilation-unit  nil
  (dolist (file (append *aserve-files* *aserve-examples*))
    #+allegro-cl-lite
    (progn
      ;; aServe doesn't work very well under 5.0.1 Lite due to
      ;; socket problem which are patched in the normal 5.0.1 but
      ;; not the lite version
      (if* (equal file "examples/examples")
	 then (load (merge-pathnames (format nil "~a.cl" file)
				     *load-truename*))
	 else (excl:load-compiled (merge-pathnames (format nil "~a.cl" file)
						   *load-truename*)))
      (gc t) ; must compact to keep under the heap limit
      )
    #-allegro-cl-lite
    (progn (case *loadswitch*
	     (:compile-if-needed (compile-file-if-needed 
				  (merge-pathnames (format nil "~a.cl" file)
						   *load-truename*)))
	     (:compile (compile-file 
			(merge-pathnames (format nil "~a.cl" file)
					 *load-truename*)))
	     (:load nil))
	   (load (merge-pathnames 
		  (format nil "~a.fasl" file)
		  *load-truename*)))))



;; after running this function you'll have a lisp binary
;; with the webserver loaded.
;; you can cd to aserveserver and start with
;;   nohup ./aserverserver -f ../examples/examples.fasl >& errs &
;; and it will run the server in the background, serving the aserve
;; examples.
;;
(defun makeapp ()
  (run-shell-command "rm -fr aserveserver")
  (make-aserve.fasl)
  (generate-application
   "aserveserver"
   "aserveserver/"
   '(:sock :process :defftype :foreign 
     :ffcompat "aserve.fasl")
   ; strange use of find-symbol below so this form can be read without
   ; the net.aserve package existing
   :restart-init-function (find-symbol (symbol-name :start-cmd) :net.aserve)
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
  ;; make a distributable version of aserve
  (run-shell-command 
   (format nil "rm -fr ~aaserve-dist" *aserve-root*))
   
  (run-shell-command 
   (format nil "mkdir ~aaserve-dist ~aaserve-dist/doc ~aaserve-dist/examples ~aaserve-dist/test"
	   *aserve-root*
	   *aserve-root*
	   *aserve-root*
	   *aserve-root*
	   ))
   
  (copy-files-to *aserve-files* "aserve.fasl" :root *aserve-root*)
  
  (dolist (file '("aserve.fasl"
		  "doc/aserve.html"
		  "doc/tutorial.html"
		  "doc/htmlgen.html"
		  "readme.txt"
		  "examples/examples.cl"
		  "examples/examples.fasl"
		  "examples/foo.txt"
		  "examples/fresh.jpg"
		  "examples/prfile9.jpg"))
    (copy-files-to (list file)
		   (format nil "aserve-dist/~a" file)
		   :root *aserve-root*)))


;; checklist for publishing aserve source for source-master:
;; 1. incf version number in main.cl, edit ChangeLog and commit
;; 2. make clean
;; 3. start lisp and load aserve/load to compile all files, there should
;;    be no warnings.
;; 4. start the server (net.aserve:start :port 8000) 
;;	and run through the samples from Netscape and IE
;; 5. :cl test/t-aserve
;; 6. (make-src-distribution)
;; 7. (ftp-publish-src)
;; 8. (publish-docs)   ;  to put latest docs on aserve web page
;; 9. on beast run /fi/sa/bin/aserve-sync
;; 10. ftp download.sourceforge.net and put the tar file in the
;;     incoming directory, then go to the aserve sourceforget web page and 
;;     select the file manager and publish it.
;; 11. cd to /www/opensource/htdocs/aserve 
;;     on cobweb and rsync the files with SourceForge


(defparameter aserve-version-name 
    (apply #'format nil "aserve-~d.~d.~d" 
	   (symbol-value
	    (find-symbol 
	     (symbol-name :*aserve-version*)
	     :net.aserve))))


(defun make-aserve.fasl ()
  (copy-files-to *aserve-files* "aserve.fasl" :root *aserve-root*))



(defun make-src-distribution ()
  ;; make a source distribution of aserve
  ;;
    
  (run-shell-command 
   (format nil "rm -fr ~aaserve-src" *aserve-root*))
    
  (run-shell-command 
   (format nil "mkdir ~aaserve-src ~aaserve-src/~a ~aaserve-src/~a/htmlgen "
	   *aserve-root*
	   
	   *aserve-root*
	   aserve-version-name
	   
	   *aserve-root*
	   aserve-version-name
	   ))
  
  (run-shell-command 
   (format nil "mkdir ~aaserve-src/~a/doc ~aaserve-src/~a/examples ~aaserve-src/~a/test"
	   *aserve-root*
	   aserve-version-name
	   
	   *aserve-root*
	   aserve-version-name
	   
	   *aserve-root*
	   aserve-version-name
	   
	   ))
	   
  (dolist (file (append (mapcar #'(lambda (file) (format nil "~a.cl" file))
				*aserve-files*)
			*aserve-other-files*))
    (copy-files-to
     (list file)
     (format nil "aserve-src/~a/~a" aserve-version-name file)
     :root *aserve-root*)))


(defun ftp-publish-src ()
  ;; assuming tha we've made the source distribution, tar it
  ;; and copy it to the ftp directory
  (run-shell-command
   (format nil "(cd ~aaserve-src ; tar cfz ~a.tgz ~a)"
	   *aserve-root*
	   aserve-version-name
	   aserve-version-name))
  (run-shell-command 
   (format nil "cp ~aaserve-src/~a.tgz /net/candyman/home/ftp/pub/aserve"
	   *aserve-root*
	   aserve-version-name)))

(defun publish-docs ()
  ;; copy documentation to the external web site
  (run-shell-command
   (format nil "cp ~adoc/htmlgen.html ~adoc/aserve.html ~adoc/tutorial.html /net/cobweb/www/opensource/htdocs/aserve"
	   *aserve-root*
	   *aserve-root*
	   *aserve-root*)))
	   
	    
  

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
