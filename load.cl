;; load in neo

(defvar *loadswitch* :compile-if-needed)
;(require :defftype)

(dolist (file '("../htmlgen/htmlgen"
		"neo"
		"publish"
		"examples"))
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
