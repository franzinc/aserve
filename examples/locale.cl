(defpackage :locale
  (:use :excl :common-lisp))

(in-package :locale)

(eval-when (compile load eval)
  (if* (not (featurep '(:version>= 6 1)))
    then (error "~
This file not supported in Allegro CL releases earlier than 6.1.")))

(eval-when (compile load eval)
  (if* (not (featurep ':ics))
    then (error "~
This file only supported in International Allegro CL.")))

(eval-when (compile load eval)
  (require :aserve))

(eval-when (compile load eval)
  (use-package :net.aserve)
  (use-package :net.html.generator))  

;;
;;  C o r e  F u n c t i o n a l i t y  S e c t i o n .
;;
;; The core function is html-locale-display.  Its purpose is to illustrate
;; how one can simply change the locale, by binding *locale*, to affect the
;; locale specific functions and format directives.
;;
;; html-locale-display generates html output.  It can be called directly in
;; lisp.  Example:
;; 
;;  (with-open-file (s "output.html" :direction :output)
;;    (html-locale-display s "en_US"))
;;
;; creates output.html using the en_US locale.  Alternatively, you can just
;; do the following to see the html output directly:
;; 
;;  (html-locale-display *standard-output* "en_US")
;;
;; Note that the output is in UTF-8.
;;
(defun html-locale-display (ostream locale
			    ;; We use &aux as a shorthand for rebinding
			    ;; *locale* while in this function's context.
			    ;; While we're at it, we remember ostream's
			    ;; external-format here as well.
			    &aux (*locale* (find-locale locale))
				 (ef (stream-external-format ostream)))
  (unwind-protect
      ;; Here we effectively rebind the stream's external-format to utf-8 for
      ;; the display.  When we're done, we restore the external-format.
      (progn
	(setf (stream-external-format ostream)
	  (find-external-format :utf8))
	(html-locale-display1 ostream))
    ;; cleanup forms -- restore stream's original external-format.
    (setf (stream-external-format ostream) ef)))

(defun html-locale-display1 (utf8-stream)
  (net.html.generator:html-stream
   utf8-stream
   (:html
    (:head (:title "Locale Display")
	   ((:meta http-equiv "content-type"
		   ;; Specify that the client should expect utf-8.
		   content "text/html; charset=utf-8")))
    (:body
     (:p
      (:i #.(format nil "~
Characters appearing as question-marks or empty boxes indicate those not ~
displayable with the client (e.g., browser) fonts.")))
     (:b (:princ-safe (format nil "In locale ~a [~a / ~a]:"
			      (locale-name *locale*)
			      (locale-attribute *locale* :language)
			      (locale-attribute *locale* :territory))))
     ((:table :border 1 :cellpadding 2)
      (:tr (:td "International Currency Symbol")
	   (:td (:princ (format nil "~a"
				(locale-int-curr-symbol *locale*)))))
      (:tr (:td "Locale Currency Symbol")
	   (:td (:princ (format nil "~a"
				(locale-currency-symbol *locale*)))))
      (:tr (:td "Monetary Display of 1234.50")
	   (:td (:princ (format nil "~/locale-format-monetary/" 1234.50))))
      (:tr (:td "International Monetary Display of 1234.50")
	   (:td (:princ (format nil "~:/locale-format-monetary/" 1234.50))))
      (:tr (:td "Monetary Display of " (:nobr "-1234.50"))
	   (:td (:princ (format nil "~/locale-format-monetary/" -1234.50))))
      (:tr (:td "International Monetary Display of " (:nobr "-1234.50"))
	   (:td (:princ (format nil "~:/locale-format-monetary/" -1234.50))))
      (:tr (:td "Number Display of 9876543.21d0")
	   (:td (:princ (format nil "~:/locale-format-number/" 9876543.21d0))))
      (:tr (:td "Number Display of " (:nobr "-9876543.21d0"))
	   (:td (:princ (format nil "~:/locale-format-number/"
				-9876543.21d0))))
      (:tr (:td "AM/PM Time")
	   (:td (:princ (format nil "~/locale-format-time/"
				(get-universal-time)))))
      (:tr (:td "Time")
	   (:td (:princ (format nil "~@/locale-format-time/"
				(get-universal-time)))))
      (:tr (:td "Date")
	   (:td (:princ (format nil "~:/locale-format-time/"
				(get-universal-time)))))
      (:tr (:td "Date and Time")
	   (:td (:princ (format nil "~:@/locale-format-time/"
				(get-universal-time)))))
      (:tr (:td "Day Names")
	   (:td (:princ (format nil " ~{~a ~}"
				(locale-attribute *locale* :day)))))
      (:tr (:td "Month Names")
	   (:td (:princ (format nil " ~{~a ~}"
				(locale-attribute *locale* :mon)))))
      (:tr (:td "Abbreviated Month Names")
	   (:td (:princ (format nil " ~{~a ~}"
				(locale-attribute *locale* :abmon)))))
      (:tr (:td "Yes String:")
	   (:td (:princ (format nil "~a"
				(locale-attribute *locale* :yesstr)))))
      (:tr (:td "No String:")
	   (:td (:princ (format nil "~a"
				(locale-attribute *locale* :nostr))))))
     :br
     ((:a :href "/locale") "To Locale Demo Home Page")))))

(defun html-locale-display-home-page (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html
       (:head (:title "Locale Displayer"))
       (:body
	(:p #.(format nil "~
The code for this demo illustrates how easy it is to support multiple ~
simultaneous locales in Allegro CL.  In Lisp, it is just a matter of binding ~
the symbol ")
	    (:b (:tt "excl:*locale* "))
	    #.(format nil "~
to a locale object.  In this way, different threads (i.e., Lisp processes) ~
can operate in their own locales.  ~
Allegro CL includes several pre-defined locales.  Allegro CL users can add ~
new locales."))
	(:p "Instructions:  Click on a " (:nobr (:i "Locale Name "))
	    "entry to activate display for that locale.")
	(:p "A blank entry for " (:nobr (:i "Language / Territory "))
	    #.(format nil "~
means the locale information has not yet been loaded into the Lisp server.  ~
Such locale information gets auto-loaded into Lisp when needed.  ~
Hitting the browser refresh button will request the latest update of this ~
table to be displayed, thus updating the ")
	    (:nobr (:i "Language / Territory "))
	    "entries currently loaded in the server.")
	(:table
	 (:tr (:td (:u (:i "Locale Name")))
	      (:td (:u (:i "Language / Territory"))))
	 (dolist (ldef (sort (delete-if #'(lambda (x)
					    (equal "CVS" (pathname-name x)))
					(directory *locales-dir*))
			     #'(lambda (x y)
				 (string< (pathname-name x)
					  (pathname-name y)))))
	   (let* ((locale-name (pathname-name ldef))
		  (loaded-locale (find locale-name (all-locales)
				       :key #'locale-name
				       :test #'string-equal)))
	     (html
	      (:tr (:td ((:a :href (format nil "/locale-display?locale=~a"
					   locale-name))
			 (:b (:princ-safe locale-name))))
		   (:td
		    (when loaded-locale
		      (html
		       (:b (:princ-safe
			    (format nil "~a / ~a"
				    (locale-attribute loaded-locale
						      :language)
				    (locale-attribute
				     loaded-locale
				     :territory)))))))))))))))))

(defun html-locale-display-locale-page (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent :external-format (crlf-base-ef :utf-8))
      (html-locale-display *html-stream*
			   (cdr (assoc "locale" (request-query req)
				       :test #'string=))))))

(publish :path "/locale"
	 :content-type "text/html"
	 :function #'html-locale-display-home-page)

(publish :path "/locale-display"
	 :content-type "text/html; charset=utf-8"
	 :function #'html-locale-display-locale-page)

