;; load in aserve

;
; loading this file will compile and load AllegroServe and Webactions
;
; calling (make-aserve.fasl) will build
;   aserve.fasl  - just allegroserve
;   webactions/webactions.fasl  - just webactions 

(in-package :user)

(defvar *loadswitch* :compile-if-needed)
(defparameter *aserve-root* (directory-namestring *load-pathname*))

(defparameter *aserve-files* 
    ;; this list is in cl/src/sys/make.cl as well... keep in sync
    '("htmlgen/htmlgen"
      "packages"
      "macs"
      "queue"
      "main"
      "headers"
      "parse"
      "decode"
      "publish"
      "authorize"
      "log" 
      "client"
      "proxy"
      "cgi"
      "chunker"
      #+include-playback "playback"   ;;; not part of production release
      ))

(defparameter *aserve-other-files*
    ;; other files that make up the aserve dist
    '("README.md"
      "license-lgpl.txt"
      "license-allegroserve.txt"
      "examples/examples.cl"
      "examples/foo.txt"
      "examples/fresh.jpg"
      "examples/prfile9.jpg"
      "examples/tutorial.cl"
      "examples/aservelogo.gif"
      "examples/aservepowered.gif"
      "examples/chat.cl"
      "examples/file2000.txt"
      "examples/puzzle.cl"
      "examples/urian.cl"
      "examples/locale.cl"
      "load.cl"
      "test/t-aserve.cl"
      "test/server.pem"
      "test/testdir/suba/subsuba/foo.html"
      "test/testdir/suba/access.cl"
      "test/testdir/suba/foo.html"
      "test/testdir/suba/subd/ddd.html"
      "test/testdir/subc/ccc.html"
      "test/testdir/subd/ddee.html"
      "test/testdir/access.cl"
      "test/testdir/aaa.foo"
      "test/testdir/bbb.ign"
      "test/testdir/ccc.html"
      "test/testdir/readme"
      "test/testdir/subb/access.cl"
      "test/testdir/subb/foo.html"
      "examples/cgitest.sh"
      "doc/aserve.html"
      "doc/tutorial.html"
      "doc/htmlgen.html"
      ))

(defparameter *aserve-examples*
    '("examples/examples"
      "examples/puzzle"
      "examples/urian"
      "examples/locale"
      ))

(defparameter *aserve-international-only*
    ;; files that should only be loaded into a international lisp
    '("examples/puzzle"
      "examples/urian"
      "examples/locale"
      ))



(defparameter *webactions-files*
    ;; this list of source files that are compiled to make 
    ;; webactions
    '("webactions/clpage"
      "webactions/webact"
      "webactions/websession"
      
      "webactions/clpcode/clp"
      "webactions/clpcode/wa"
      "webactions/clpcode/http"
      "webactions/clpcode/time"))

(defparameter *webactions-other-files*
    ;; other files to distribute with a source distribution
    '("webactions/load.cl"
      "webactions/doc/using-webactions.html"
      "webactions/doc/webactions.html"
      "webactions/test/t-webactions.cl"
      "webactions/test/sitea/project.cl"
      "webactions/test/sitea/file1.clp"
      "webactions/test/sitea/file2.clp"
      "webactions/test/sitea/file3.clp"))
      

    
;; end experimental

(eval-when (compile eval load)
  (require :sock) ;; so we can tell if we have hiper sockets
  )
;(setq *features* (delete :hiper-socket *features*))

(with-compilation-unit  nil
  (dolist (file (append *aserve-files* *aserve-examples*
			*webactions-files*))
    #+allegro-cl-lite
    (progn
      ;; aServe doesn't work very well under 5.0.1 Lite due to
      ;; socket problem which are patched in the normal 5.0.1 but
      ;; not the lite version
      (if* (equal file "examples/examples")
	 then (load (merge-pathnames (format nil "~a.cl" file)
				     *load-pathname*))
	 else (excl:load-compiled (merge-pathnames (format nil "~a.cl" file)
						   *load-pathname*)))
      (gc t) ; must compact to keep under the heap limit
      )
    #-allegro-cl-lite
    (if* (or  (member :ics *features* :test #'eq)
	      (not (member file *aserve-international-only* :test #'equal)))
       then (progn (case *loadswitch*
		     (:compile-if-needed (compile-file-if-needed 
					  (merge-pathnames (format nil "~a.cl" file)
							   *load-pathname*)))
		     (:compile (compile-file 
				(merge-pathnames (format nil "~a.cl" file)
						 *load-pathname*)))
		     (:load nil))
		   (load (merge-pathnames 
			  (format nil "~a.fasl" file)
			  *load-pathname*))))))



;; after running this function you'll have a lisp binary
;; with the webserver loaded.
;; you can cd to aserveserver and start with
;;   nohup ./aserverserver -f ../examples/examples.fasl >& errs &
;; and it will run the server in the background, serving the aserve
;; examples.
;;
(defun makeapp ()
  (run-shell-command "rm -fr aserveserver" :show-window :hide)
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

  (run-shell-command (format nil "rm -fr ~aaserve-dist" *aserve-root*)
		     :show-window :hide)
   
  (copy-files-to *aserve-files* "aserve.fasl" :root *aserve-root*)
  
  (dolist (file '("aserve.fasl"
		  "doc/aserve.html"
		  "doc/tutorial.html"
		  "doc/htmlgen.html"
		  "doc/cvs.html"
		  "readme.txt"
		  "examples/examples.cl"
		  "examples/examples.fasl"
		  "examples/foo.txt"
		  "examples/fresh.jpg"
		  "examples/prfile9.jpg"
		  "examples/cgitest.sh"))
    (copy-files-to (list file)
		   (format nil "aserve-dist/~a" file)
		   :root *aserve-root*)))


;; checklist for publishing aserve source for source-master:
;; 1. incf version number in main.cl,doc/aserve.html, edit ChangeLog and commit
;; 2. make clean
;; 3. start lisp and load aserve/load to compile all files, there should
;;    be no warnings.
;; 4. start the server (net.aserve:start :port 8000) 
;;	and run through the samples from Netscape and IE
;; 5a. :cl test/t-aserve
;; 5b: :cl webactions/test/t-webactions
;; 6. (make-src-distribution)
;; 7. (ftp-publish-src)
;; 8. on cobweb in /fi/opensource/src/aserve 
;;    do cvs update to put code on opensource site
;; 9. on cobweb as root do:
;;    rsh cvs 'cd /repository/cvs-public/aserve && rsync -a --delete /cvs/aserve/ . && chgrp -R cvspublic .'
;;
;; 10. ftp upload.sourceforge.net and put the tar file in the
;;     incoming directory, then go to the aserve sourceforge web page and 
;;     select the file manager and publish it.
;; 11. cd /www/opensource/devel/www/aserve 
;;     on cobweb and rsync the files with SourceForge


(defparameter aserve-version-name 
    (apply #'format nil "aserve-~d.~d.~d" 
	   (symbol-value
	    (find-symbol 
	     (symbol-name :*aserve-version*)
	     :net.aserve))))


(defun make-aserve.fasl ()
  ;; make both aserve and webactions
  (copy-files-to *aserve-files* "aserve.fasl" :root *aserve-root* 
		 :verbose t)
  (copy-files-to *webactions-files* "webactions/webactions.fasl" 
		 :root *aserve-root*
		 :verbose t)
  )



(defun make-src-distribution (&optional (dist-name aserve-version-name))
  ;; make a source distribution of aserve

  (run-shell-command (format nil "rm -fr ~aaserve-src" *aserve-root*)
		     :show-window :hide)
  
  (dolist (file (append (mapcar (lambda (file) (format nil "~a.cl" file))
				(append *aserve-files*
					*webactions-files*))
			*aserve-other-files*
			*webactions-other-files*))
    (copy-files-to (list file)
		   (format nil "aserve-src/~a/~a" dist-name file)
		   :root *aserve-root*)))


(defun ftp-publish-src ()
  ;; assuming tha we've made the source distribution, tar it
  ;; and copy it to the ftp directory
  (run-shell-command
   (format nil "(cd ~aaserve-src ; tar cfz ~a.tgz ~a)"
	   *aserve-root*
	   aserve-version-name
	   aserve-version-name)
   :show-window :hide)
  (run-shell-command 
   (format nil "cp ~aaserve-src/~a.tgz /fi/ftp/pub/aserve"
	   *aserve-root*
	   aserve-version-name)
   :show-window :hide))

(defun publish-docs ()
  ;; copy documentation to the external web site
  (run-shell-command
   (format nil "cp ~adoc/htmlgen.html ~adoc/aserve.html ~adoc/tutorial.html /fi/www/sites/opensource/devel/www/aserve"
	   *aserve-root*
	   *aserve-root*
	   *aserve-root*)
   :show-window :hide)
  (run-shell-command "rsh cobweb bin/sync-a-opensource"
		     :show-window :hide))
	   
	    
  

(defun copy-files-to (files dest &key (root "") verbose)
  ;; copy the contents of all files to the file named dest.
  ;; append .fasl to the filenames (if no type is present)

  (setq dest (concatenate 'string root dest))
  (ensure-directories-exist dest)
  
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (p dest :direction :output :if-exists :supersede
		     #-(and allegro (version>= 6))
		     :element-type
		     #-(and allegro (version>= 6))
		     '(unsigned-byte 8))
      (if* verbose
	 then (format t "Creating ~s~%" dest))
      (dolist (file files)
	(setq file (concatenate 'string root file))
	(if* (and (null (pathname-type file))
		  (not (probe-file file)))
	   then (setq file (concatenate 'string file  ".fasl")))
	(with-open-file (in file
			 #-(and allegro (version>= 6))
			 :element-type
			 #-(and allegro (version>= 6))
			 '(unsigned-byte 8))
	  (loop
	    (let ((count (read-sequence buffer in)))
	      (if* (<= count 0) then (return))
	      (write-sequence buffer p :end count))))))))
