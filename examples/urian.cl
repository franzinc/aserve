;; -*- mode: common-lisp; package: net.aserve.examples -*-
;;
;; urian.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation; 
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;

;; Description:
;;   urian example


;;
;; Web page character analyzer.
;; This example retrieves a web page associated with a url, parses it with
;; parse-html, and then displays all texts found to have non-ascii characters.
;; Each character is a link.  Clicking on one of these links displays a
;; description of the linked character.
;;
;; Original Author: Charles A. Cox, Franz Inc., October 2000
;;
;; To use, compile/load this file into Allegro CL 6.0.  Then,
;; start allegroserve, eg, (net.aserve:start :port 8000) starts on port 8000.
;; The main published page for this example is "/urian".

(defpackage :urian
  (:use :common-lisp :excl))

(in-package :urian)

(eval-when (compile load eval)
  (if* (not (featurep '(:version>= 6 0)))
    then (error "This file not supported in Allegro CL releases earlier than 6.0")))

(eval-when (compile load eval)
  (require :aserve)
  (handler-case (require :phtml)
    ; didn't find it, check to see if it's where it would be in 
    ; a non-user file layout
    (error (c)
      (declare (ignore c))
      (let (name)
	(if* (or (probe-file (setq name (concatenate 'string
					  (directory-namestring *load-pathname*)
					  "../xmlutils/phtml.fasl")))
		 (probe-file (setq name (concatenate 'string
					  (directory-namestring *load-pathname*)
					  "../../xmlutils/phtml.fasl"))))
		 
	   then (load name)
	   else (format t " not at ~s~%, tn is ~s~%" name
			*load-pathname*)
		(error "can't locate phtml module"))))))

(defpackage :urian
  (:use :net.html.generator :net.aserve :net.html.parser))

(pushnew :x-sjis (ef-nicknames (find-external-format :shiftjis)))
(pushnew :shift-jis (ef-nicknames (find-external-format :shiftjis)))
(pushnew :iso-8859-1 (ef-nicknames (find-external-format :latin1)))
(pushnew :windows-1252 (ef-nicknames (find-external-format :1252)))

(defparameter *blocks*
    '((#x0000 #x007f "Basic Latin")
      (#x0080 #x00ff "Latin-1 Supplement")
      (#x0100 #x017f "Latin Extended-A")
      (#x0180 #x024f "Latin Extended-B")
      (#x0250 #x02af "IPA Extensions")
      (#x02b0 #x02ff "Spacing Modifier Letters")
      (#x0300 #x036f "Combining Diacritical Marks")
      (#x0370 #x03ff "Greek")
      (#x0400 #x04ff "Cyrillic")
      (#x0530 #x058f "Armenian")
      (#x0590 #x05ff "Hebrew")
      (#x0600 #x06ff "Arabic")
      (#x0700 #x074f "Syriac  ")
      (#x0780 #x07bf "Thaana")
      (#x0900 #x097f "Devanagari")
      (#x0980 #x09ff "Bengali")
      (#x0a00 #x0a7f "Gurmukhi")
      (#x0a80 #x0aff "Gujarati")
      (#x0b00 #x0b7f "Oriya")
      (#x0b80 #x0bff "Tamil")
      (#x0c00 #x0c7f "Telugu")
      (#x0c80 #x0cff "Kannada")
      (#x0d00 #x0d7f "Malayalam")
      (#x0d80 #x0dff "Sinhala")
      (#x0e00 #x0e7f "Thai")
      (#x0e80 #x0eff "Lao")
      (#x0f00 #x0fff "Tibetan")
      (#x1000 #x109f "Myanmar ")
      (#x10a0 #x10ff "Georgian")
      (#x1100 #x11ff "Hangul Jamo")
      (#x1200 #x137f "Ethiopic")
      (#x13a0 #x13ff "Cherokee")
      (#x1400 #x167f "Unified Canadian Aboriginal Syllabics")
      (#x1680 #x169f "Ogham")
      (#x16a0 #x16ff "Runic")
      (#x1780 #x17ff "Khmer")
      (#x1800 #x18af "Mongolian")
      (#x1e00 #x1eff "Latin Extended Additional")
      (#x1f00 #x1fff "Greek Extended")
      (#x2000 #x206f "General Punctuation")
      (#x2070 #x209f "Superscripts and Subscripts")
      (#x20a0 #x20cf "Currency Symbols")
      (#x20d0 #x20ff "Combining Marks for Symbols")
      (#x2100 #x214f "Letterlike Symbols")
      (#x2150 #x218f "Number Forms")
      (#x2190 #x21ff "Arrows")
      (#x2200 #x22ff "Mathematical Operators")
      (#x2300 #x23ff "Miscellaneous Technical")
      (#x2400 #x243f "Control Pictures")
      (#x2440 #x245f "Optical Character Recognition")
      (#x2460 #x24ff "Enclosed Alphanumerics")
      (#x2500 #x257f "Box Drawing")
      (#x2580 #x259f "Block Elements")
      (#x25a0 #x25ff "Geometric Shapes")
      (#x2600 #x26ff "Miscellaneous Symbols")
      (#x2700 #x27bf "Dingbats")
      (#x2800 #x28ff "Braille Patterns")
      (#x2e80 #x2eff "CJK Radicals Supplement")
      (#x2f00 #x2fdf "Kangxi Radicals")
      (#x2ff0 #x2fff "Ideographic Description Characters")
      (#x3000 #x303f "CJK Symbols and Punctuation")
      (#x3040 #x309f "Hiragana")
      (#x30a0 #x30ff "Katakana")
      (#x3100 #x312f "Bopomofo")
      (#x3130 #x318f "Hangul Compatibility Jamo")
      (#x3190 #x319f "Kanbun")
      (#x31a0 #x31bf "Bopomofo Extended")
      (#x3200 #x32ff "Enclosed CJK Letters and Months")
      (#x3300 #x33ff "CJK Compatibility")
      (#x3400 #x4db5 "CJK Unified Ideographs Extension A")
      (#x4e00 #x9fff "CJK Unified Ideographs")
      (#xa000 #xa48f "Yi Syllables")
      (#xa490 #xa4cf "Yi Radicals")
      (#xac00 #xd7a3 "Hangul Syllables")
      (#xd800 #xdb7f "High Surrogates")
      (#xdb80 #xdbff "High Private Use Surrogates")
      (#xdc00 #xdfff "Low Surrogates")
      (#xe000 #xf8ff "Private Use")
      (#xf900 #xfaff "CJK Compatibility Ideographs")
      (#xfb00 #xfb4f "Alphabetic Presentation Forms")
      (#xfb50 #xfdff "Arabic Presentation Forms-A")
      (#xfe20 #xfe2f "Combining Half Marks")
      (#xfe30 #xfe4f "CJK Compatibility Forms")
      (#xfe50 #xfe6f "Small Form Variants")
      (#xfe70 #xfefe "Arabic Presentation Forms-B")
      (#xfeff #xfeff "Specials")
      (#xff00 #xffef "Halfwidth and Fullwidth Forms")
      (#xfff0 #xfffd "Specials")))

(publish
 :path "/urian"
 :content-type "text/html; charset=utf-8"
 :function
 #'(lambda (req ent)
     (let* ((uri (cdr (assoc "uri" (request-query req) :test #'equal)))
	    (results nil))
       (when uri
	 (unless  (find #\: uri)
	   (setq uri (concatenate 'string "http://" uri)))
	 (setq results (chanal uri)))
       (with-http-response (req ent)
	 (with-http-body (req ent
			      :external-format :utf8-base)
	   (html
	    (:html
	     (:head (:title (:princ-safe
			     (format nil "String Analysis~@[ for `~a'~]"
				     uri))))
	     (:body
	      (if* (stringp results)
		 then (html (:p "AllegroServe got error:  "
				(:b (:princ-safe results))))
		 else (when results
			(when (first results)
			  (html
			   (:p (:princ-safe
				(format nil "Server set charset to `~s'."
					(car (first results))))
			       :br
			       (:princ-safe
				(format nil "Switched to External-Format `~s'."
					(ef-name (cdr (first results))))))))
			(when (second results)
			  (html
			   (:p (:princ-safe
				(format
				 nil
				 "A page meta tag specified charset as `~s'."
				 (car (second results))))
			       :br
			       (:princ-safe
				(format
				 nil "Switched to external-format: `~s'."
				 (ef-name (cdr (second results))))))))
			(html (:p "Scanned URL:  " ((:a :href uri
							target "_blank")
						    (:princ-safe uri))))
			(if* (cddr results)
			   then (html
				 (:p
				  "The following texts were found to contain "
				  "non-ASCII characters.  "
				  :br
				  "Click on a character for its description."))
				"Strings found on URL:  "
				(dolist (result (cddr results))
				  (html
				   :hr
				   (san-html result *html-stream*)))
			   else (html
				 (:p
				  "No texts containing non-ASCII characters "
				  "were found on the page.")))))
	      :hr
	      (macrolet ((item (title url)
			   ;; Assumes title and url are string literals
			   (let ((ref (format nil "/urian?uri=~a"
					      (uriencode-string url))))
			     `(html
			       (:ul (:li (:princ-safe ,title)
					 " ("
					 (:princ-safe ,url)
					 ")"
					 :br
					 ((:a href ,url
					      target "_blank")
					  "View Page (new browser window)")
					 :br
					 ((:a href ,ref) "Analyze")))))))
		(html
		 (:p
		  "Select a sample page:"
		  (item "UTF-8 Sampler"
			"http://www.columbia.edu/kermit/utf8.html")
		  (item "The \"anyone can be provincial!\" page"
			"http://www.trigeminal.com/samples/provincial.html")
		  (item "The Japan Netscape Netcenter Page"
			"http://home.netscape.com/ja")
		  (item "The Spain Yahoo! Page"
			"http://es.yahoo.com"))))
	      :br
	      ((:form :action "urian"
		      :method "get")
	       "Or Enter New URL to analyze:  "
	       ((:input :type "text" :name "uri" :size 50)))))))))))

(defun san-html (string stream)
  (net.html.generator:html-stream
   stream
   (net.html.generator:html
    (:p "\""
	(dotimes (i (length string))
	  (net.html.generator:html
	   ((:a href
		(format nil "/chdescribe?char=~a"
			(net.aserve:uriencode-string
			 (format nil "u+~4,'0x:~s"
				 (char-code
				  (schar string i))
				 (schar string i)))))
	    (:princ (schar string i)))))
	"\""))))

(defun chanal (uri
	       &aux (server-ef nil)
		    (lhtml nil)
		    (metatag-ef nil))
  (handler-case
      (multiple-value-bind (body response-code headers ruri)
	  (net.aserve.client:do-http-request uri :external-format :latin1-base)
	(declare (ignore response-code ruri))
	(setq server-ef (let ((content-type (cdr (assoc :content-type
							headers))))
			  (find-charset-from-content-type content-type)))
	(setq lhtml (net.html.parser:parse-html body))
	(setq metatag-ef (update-ef lhtml))
	(cons server-ef
	      (cons metatag-ef
		    (delete-duplicates
		     (chanal-body lhtml (or (cdr metatag-ef)
					    (cdr server-ef)
					    ;; www.yahoo.co.jp uses euc without
					    ;; specifying it.  Let's try using
					    ;; euc, then, as default.
					    (crlf-base-ef
					     (find-external-format :latin1))))
		     :test #'string=))))
    (error (c)
      (format nil "~a" c))))

(defun chanal-body (body ef)
  (if* (stringp body)
     then (let ((s (octets-to-string
		    (string-to-octets body :external-format :latin1-base)
		    :external-format ef)))
	    (dotimes (i (length s))
	      (when (> (char-code (schar s i)) #x7f)
		;; non-ascii
		(return-from chanal-body (list s))))
	    nil)
   elseif (consp body)
     then ;; skip unparsed <script> and <style> forms
	  (if* (or (eq :script (car body))
		   (eq :style (car body))
		   (eq :comment (car body))
		   (and (listp (car body))
			(or (eq :script (caar body))
			    (eq :style (caar body)))))
	     then nil
	     else (nconc (chanal-body (car body) ef)
			 (chanal-body (cdr body) ef)))))

(defun find-charset-from-content-type (content-type)
  (let ((charsetp (search "charset=" content-type
			  :test #'string-equal))
	(cs-name nil))
    (when charsetp
      (setq cs-name (subseq content-type
			    (1+ (position #\= content-type
					  :start charsetp))
			    (position #\; content-type
				      :start charsetp)))
      (cons cs-name
	    (crlf-base-ef
	     (find-external-format
	      (let ((*package* (find-package :keyword)))
		(read-from-string
		 (string-downcase cs-name)))))))))

(defun update-ef (lhtml)
  (when (listp lhtml)
    (dolist (html-body lhtml)
      (when (eq :html (car html-body))
	(let ((html-component (second html-body)))
	  (when (eq :head (car html-component))
	    (dolist (x (cdr html-component))
	      (let ((charset-string (charset-metatag-p x)))
		(when charset-string
		  (return-from update-ef
		    (find-charset-from-content-type charset-string)))))))))))

(defun charset-metatag-p (head-component)
  (when (listp head-component)
    (let ((arg-tag (car head-component)))
      (when (and (listp arg-tag)
		 (eq :meta (car arg-tag)))
	(when (and (>= (length arg-tag) 6) ; spr31997
		   (equalp '(:http "http" :equiv "content-type" :content)
			   (subseq arg-tag 1 6)))
	  (return-from charset-metatag-p (elt arg-tag 6)))
	(when (equalp '(:http-equiv "content-type" :content)
		      (subseq arg-tag 1 4))
	  (return-from charset-metatag-p (elt arg-tag 4)))))))

(defmacro cjk-p (code)
  `(or
    ;; CJK Ideographs
    (<= #x4e00 ,code #x9fff)
    ;; Hangul Syllables
    (<= #xac00 ,code #xd7a3)))

(publish
 :path "/chdescribe"
 :content-type "text/html; charset=utf-8"
 :function
 #'(lambda (req ent)
     (let ((lookup
	    (assoc "char" (request-query req)
		   :test #'string=)))
       (when lookup
	 (setq lookup
	   (let ((*read-base* 16))
	     (read-from-string
	      (subseq (cdr lookup)
		      #.(length "u+")
		      #.(length "u+xxxx"))))))
       (with-http-response (req ent)
	 (with-http-body (req ent
			      :external-format :utf8-base)
	   (html
	    (:html
	     (:head (:title "Character Description"))
	     (:body
	      (:p
	       (:princ (format nil "Unicode value:  U+~4,'0x"
			       lookup)))
	      (:p
	       "Lisp Character Name:  "
	       ((:font :size "+3")
		(:prin1 (code-char lookup))))
	      (:p
	       "Browser Font Display:  "
	       ((:font :size "+3")
		(:princ (code-char lookup)))
	       :br
	       #.(format nil "~
Characters that appear as dots or empty boxes or question-marks likely look
that way because your browser is missing the needed font(s)."))
	      (unless (cjk-p lookup)
		(let ((uglyph (format nil "~
http://charts.unicode.org/Glyphs/~2,'0x/U~4,'0x.gif"
				      (ldb (byte 8 8) lookup)
				      lookup)))
		  (html ((:table border 0)
			 (:tr
			  (:td #.(format nil "~
Glyph GIF (from Unicode web site -- not all characters have gifs):")
			       :br
			       (:princ (format nil "[Loading from `~a'.]"
					       uglyph)))
			  (:td
			   ((:img :src uglyph
				  :alt (format nil "~s" (code-char lookup))
				  :border 2))))))))
	      (html
	       (:p "Character is in the "
		   (:b
		    (:princ
		     (dolist (b *blocks*)
		       (when (<= lookup (second b))
			 (return (third b))))))
		   " unicode block.")
	       (when (cjk-p lookup)
		 (html
		  (:p "More information may be available from Unicode site: "
		      (let ((upage (format nil "~
http://charts.unicode.org/unihan/unihan.acgi$0x~4,'0x"
					   lookup)))
			(html
			 ((:a href upage) (:princ-safe upage))))))))))))))))
