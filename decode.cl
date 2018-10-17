;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; decode.cl
;;
;; See the file LICENSE for the full license governing this code.
;;

;; Description:
;;   decode/encode code

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)

(eval-when (compile) (declaim (optimize (speed 3))))

;---------------- urlencoding ----------------
; there are two similar yet distinct encodings for character strings
; that are referred to as "url encodings".  We'll refer to
; the first as uriencoding and the second as form-urlencoding
;
; 1. uri's.   rfc2396 describes the format of uri's 
;       uris use only the printing characters.
;	a url can be broken down into a set of a components using
;	a regular expression matcher.
;	Each component consists of a string of characters.  Certain
;	characters must be escaped with %xy in order to put them 
;	in the uri, and others need only be escaped in certain components
;	where not escaping them would change the meaning.  It's legal
;	to over-escape though.
;	Here are the characters that need never be escaped:
;		lower case a-z
;		upper case A-Z
;		numbers    0-9
;	        mark chars: - _ . ! ~ * ' ( )
;       
;	anything else should be escaped.
;
;       The encoding (converting characters to their %xy form) must be
;	done on a component by component basis for a uri.
;	You can't just give a function a complete uri and say "encode this"
;	because if it's a uri then it's already encoded.   You can
;	give a function a filename to be put into a uri and 
;	say "encode this" and that function
;	could look for reserved characters in the filename and convert them
;	to %xy form.
;
; 2. x-www-form-urlencoded
;	when the result of a form is to be sent to the web server
;	it can be sent in one of two ways:
;	1. the "get" method where the form data is passed in the uri
;	    after a "?".
;	2  the "post" method where the data is stored in the body
;	   of the post with an application/x-www-form-urlencoded  
;	   mime type.
;
;	the form data is sent in this format
;		name=value&name2=value2&name3=value3
;	where each of the name,value items is is encoded
;	such that
;	    alphanumerics are unchanged
;	    space turns into "+"
;	    linefeed turns into "%0d%0a"
;           The following characters don't have to be encoded:
;		- _ . ! ~ * ' (  )
;           Everything else must be escaped.  While the escaping
;	    isn't necessary to be stored as the body of a post form
;           we want to use the same function to encode queries
;	    to be placed in uris, and there escaping is more necessary.


;--- uriencoding

(defvar *uri-encode*
    ;; maps 7 bit characters to t iff they have to be encoded
    ;; all characters with the 8th bit set must be encoded
    (let ((res (make-array 128 :initial-element t)))
      
      ; the alphanums
      (dolist (range '((#\a #\z)
		       (#\A #\Z)
		       (#\0 #\9)))
	(do ((i (char-code (car range)) (1+ i)))
	    ((> i (char-code (cadr range))))
	  (setf (svref res i) nil)))
      
      ; the mark characters:
      (dolist (ch '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\)))
	(setf (svref res (char-code ch)) nil))
      
      res))

(defun uri-encode-p (code)
  ;; return t iff the character must be encoded as %xy in a uri
  (if* (>= code 128)
     then t
     else (svref *uri-encode* code)))

(defun uriencode-string (str &key (external-format 
				      *default-aserve-external-format*))
  ;; encode the given string using uri encoding.
  ;; It may return the same string if no encoding need be done
  ;;
  (let ((len (native-string-sizeof str :external-format external-format))
	(count 0))
    (excl::with-dynamic-extent-usb8-array (mbvec len)
      ;; We use string-to-mb for 5.0.1 compatibility.  string-to-octets is
      ;; generally prefered after 6.0.
      (string-to-mb str :external-format external-format
		    :null-terminate nil
		    :mb-vector mbvec)
      ;; count the number of encodings that must be done
      (dotimes (i len)
	(if* (uri-encode-p (aref mbvec i)) then (incf count)))

      (if* (zerop count)
	 then str ;; just return the string, no encoding done
	 else (excl::with-dynamic-extent-usb8-array (newmbvec
						     (+ len (* 2 count)))
		(let ((j 0))
		  (dotimes (i len)
		    (let ((code (aref mbvec i)))
		      (if* (uri-encode-p code)
			 then (setf (aref newmbvec j) #.(char-code #\%))
			      (macrolet ((hexdig (code)
					   ;; return char code of hex digit
					   `(if* (< ,code 10)
					       then (+ ,code
						       #.(char-code #\0))
					       else (+ (- ,code 10)
;;;; Amazon's EC2 requires that an uppercase character be used here,
;;;; otherwise the signing of EC2 requests via the REST API don't match.
						       #.(char-code #\A)))))
				(let* ((upcode (logand #xf (ash code -4)))
				       (downcode (logand #xf code)))
				  (setf (aref newmbvec (+ j 1))
				    (hexdig upcode))
				  (setf (aref newmbvec (+ j 2))
				    (hexdig downcode))))
			      (incf j 3)
			 else (setf (aref newmbvec j) code)
			      (incf j)))))
		(values
		 ;; use values to suppress multiple values returned by
		 ;; octets-to-string.
		 ;; We use mb-to-string for 5.0.1 compatibility.
		 ;; octets-to-string is generally prefered after 6.0.
		 (mb-to-string newmbvec
			       :external-format :latin1-base
			       :end (+ len (* 2 count)))))))))


(defun uridecode-string (str &key (external-format 
				      *default-aserve-external-format*))
  ;; decoded the uriencoded string, returning possibly the
  ;; same string
  ;;
  (un-hex-escape str nil :external-format external-format)
  )








;----  form-urlencoding

(defvar *url-form-encode*
    ;; maps 7 bit characters to t iff they have to be encoded
    ;; all characters with the 8th bit set must be encoded 
    ;;
    ;; what's stored in the table is
    ;;  nil - no encoding needed
    ;;  N (integer) - how many extra characters are needed to encode this
    ;;               (i.e. one less than the total size encoded)
    
    (let ((res (make-array 128 :initial-element 2) ; assume all escaped
	       ))
      
      ; don't escape the alphanumerics
      (dolist (range '((#\a #\z)
		       (#\A #\Z)
		       (#\0 #\9)))
	(do ((i (char-code (car range)) (1+ i)))
	    ((> i (char-code (cadr range))))
	  (setf (svref res i) nil)))
      
      
      ; these 'mark' characters don't need escaping either
      (dolist (ch '(#\- #\_ #\. #\! #\~ #\* #\' #\(  #\)))
	(setf (svref res (char-code ch)) nil))
      
      ; note: character needing special handling are space and linefeed
      (setf (svref res #.(char-code #\space)) 0)
      (setf (svref res #.(char-code #\linefeed)) 5)
      
      
      res))


(defun query-to-form-urlencoded (query &key (external-format 
					     *default-aserve-external-format*))
  ;; query is a list of conses, each of which has as its 
  ;; car the query name and as its cdr the value.  A value of
  ;; nil means we encode  name=   and nothing else
  ;; encode into single string
  (let (res)
    (dolist (ent query)
      (if* res
	 then (push "&" res) ; separator
	      )
      (push (encode-form-urlencoded (car ent) :external-format external-format)
	    res)
      (push "=" res)
      (if* (cdr ent)
	 then (push (encode-form-urlencoded (cdr ent)
					    :external-format external-format)
		    res)))
    
    (apply #'concatenate 'string (nreverse res))))
      


(defmacro with-tohex-cvt-buffer ((buffer-var str) &body body)

  #-(and allegro (version>= 6 0))
  ;; Not using a separate buffer
  `(let ((,buffer-var ,str))
     (macrolet ((buf-elt (buf i)
		  `(char-code (char ,buf ,i))))
       ,@body))

  #+(and allegro (version>= 6 0))
  `(let ((,buffer-var (string-to-octets ,str
					:external-format external-format
					:null-terminate nil)))
     (macrolet ((buf-elt (buf i)
		  `(aref ,buf ,i)))
       ,@body)))

(defun encode-form-urlencoded (str &key (external-format 
					     *default-aserve-external-format*))
  ;; encode the given string using form-urlencoding
  
  ;; a x-www-form-urlencoded string consists of a sequence 
  ;; of name=value items separated by &'s. 
  ;; Each of the names and values is separately encoded using this function.
  
  ;; to build a complete x-www-form-urlencoded string use 
  ;; query-to-form-urlencoded.
  
  ; first compute if encoding has to be done and what it will
  ; cost in space
  
  (declare (ignorable external-format))

  (if* (not (stringp str))
     then (setq str (format nil "~a" str)))
  
  (with-tohex-cvt-buffer (buf str)
    (let (extra)
      (dotimes (i (length buf))
	(let ((code (buf-elt buf i)))
	  (let ((this-extra (if* (< code 128)
			       then (svref *url-form-encode* code)
			       else 2	; encode as %xy
				    )))
	    (if* this-extra
	       then (setq extra (+ (or extra 0) this-extra))))))
    
      (if* (null extra)
	 then ; great, no encoding necessary
	      str
	 else ; we have to encode
	      (let ((ret (make-string (+ (length buf) extra))))
		(do ((from 0 (1+ from))
		     (end (length buf))
		     (to 0))
		    ((>= from end))
		  (let* ((code (buf-elt buf from)))
		    (if* (eq code #.(char-code #\space))
		       then ; space -> +
			    (setf (schar ret to) #\+)
			    (incf to)
		     elseif (eq code #.(char-code #\linefeed))
		       then (dolist (nch '(#\% #\0 #\d #\% #\0 #\a))
			      (setf (schar ret to) nch)
			      (incf to))
		     elseif (or (>= code 128)
				(svref *url-form-encode* code))
		       then ; char -> %xx
			    (macrolet ((hex-digit-char (num)
					 ; number to hex char
					 `(let ((xnum ,num))
					    (if* (> xnum 9)
					       then (code-char
						     (+ #.(char-code #\a)
							(- xnum 10)))
					       else (code-char
						     (+ #.(char-code #\0)
							xnum))))))
			      (setf (schar ret to) #\%)
			      (setf (schar ret (+ to 1)) 
				(hex-digit-char (logand #xf (ash code -4))))
			      (setf (schar ret (+ to 2)) 
				(hex-digit-char (logand #xf code))))
			    (incf to 3)
		       else ; normal char
			    (setf (schar ret to) (code-char code))
			    (incf to))))
		ret)))))

			  
			  
	      
				  
  
  

(defun form-urlencoded-to-query (str &key (external-format 
					   *default-aserve-external-format*))
  ;; decode the x-www-form-urlencoded string returning a list
  ;; of conses, the car being the name and the cdr the value, for
  ;; each form element.  This list is called a query list.
  ;;
  
  (if* (not (typep str 'simple-array))
     then ; we need it to be a simple array for the code below to work
	  (setq str (copy-seq str)))
  
  (let ((res nil)
	(max (length str)))
    
    (do ((i 0)
	 (start 0)
	 (name)
	 (max-minus-1 (1- max))
	 (seenpct)
         ;; The following is a flag which determines whether we should do
         ;; external-format processing on the source string.
         ;; Note that we are assuming the source string not to be in Unicode,
         ;; but to contain one latin1 octet per element.  This is the way
         ;; a uri gets returned by parse-uri.
	 (seen-non-ascii nil)
	 (ch))
	((>= i max))
      (setq ch (schar str i))
      
      (let (obj)
	(if* (or (eq ch #\=)
		 (eq ch #\&))
	   then (setq obj (buffer-substr str start i))
		(setq start (1+ i))
           else
                (if* (eql i max-minus-1)
                   then (setq obj (buffer-substr str start (1+ i))))
                
                (if* (and (not seenpct) (or (eq ch #\%)
                                            (eq ch #\+)))
                   then (setq seenpct t)
                 elseif (and (not seen-non-ascii)
                             (>= (char-code ch) #.(expt 2 7)))
                   then (setq seen-non-ascii t)))
      
	(if* obj
	   then (if* (or seenpct seen-non-ascii)
		   then (setq obj (un-hex-escape
				   obj t
				   :external-format external-format)
			      seenpct nil))
	      
		(if* name
		   then (push (cons name obj) res)
			(setq name nil)
		 elseif (or (eq ch #\&)
			    (eq i max-minus-1))
		   then ; a name with no value
			(push (cons obj "") res)
		   else ; assert (eq ch #\=)
			(setq name obj))))
      
      (incf i))
    
    (nreverse res)))

(defmacro with-unhex-cvt-buffer ((buffer-var size)
				 &body body)
  #-(and allegro (version>= 6 0))
  ;; Buffer is a string, which gets returned
  `(let ((,buffer-var (make-string ,size)))
     (macrolet ((cvt-buf-to-string (x &key external-format end)
		  (declare (ignore external-format))
		  x)
		(set-buf-elt (buf i char)
		  `(setf (schar ,buf ,i) ,char))
		(buf-elt (buf i)
		  `(schar ,buf ,i)))
       ,@body))

  #+(and allegro (version>= 6 0))
  ;; Buffer is a static octet array, which gets converted to a string.
  `(excl::with-dynamic-extent-usb8-array (,buffer-var ,size)
     (macrolet ((cvt-buf-to-string (x &key external-format end)
		  `(values
		    (octets-to-string ,x :end ,end
				      :external-format ,external-format)))
		(set-buf-elt (buf i char)
		  `(setf (aref ,buf ,i) (char-code ,char)))
		(buf-elt (buf i)
		  `(code-char (aref ,buf ,i))))
       ,@body)))

(defun un-hex-escape (given spacep
		      &key (external-format 
				      *default-aserve-external-format*))
  ;; convert a string with %xx hex escapes into a string without
  ;; if spacep it true then also convert +'s to spaces
  ;;
  (declare (ignorable external-format))
  (let ((count 0)
	(seenplus nil)
	 ;; The following is a flag which determines whether we should do
	 ;; external-format processing on the source string.
	 ;; Note that we are assuming the source string not to be in Unicode,
	 ;; but to contain one latin1 octet per element.  This is the way
	 ;; a uri gets returned by parse-uri.
	(seen-non-ascii nil)
	(len (length given)))
    
    ; compute the number of %'s (times 2)
    (do ((i 0 (1+ i)))
	((>= i len))
      (let ((ch (schar given i)))
	(if* (eq ch #\%) 
	   then ; check for %0a%0d which is to be converted to #\linefeed
		(if* (and (< (+ i 5) len) ; enough chars left
			  (do ((xi (+ i 1) (+ xi 1))
			       (end (+ i 6))
			       (pattern '(#\0 #\d #\% #\0 #\a)
					(cdr pattern)))
			      ((>= xi end) t)
			    (if* (not (char-equal (schar given xi)
						  (car pattern)))
			       then (return nil))))
		   then ; we are looking at crlf, turn into
			; lindfeed
			(incf count 5)	; 5 char shrinkage
			(incf i 5)
		   else (incf count 2) 
			(incf i 2))
	 elseif (eq ch #\+)
	   then (setq seenplus t)
	 elseif (>= (char-code ch) #.(expt 2 7))
	   then (setq seen-non-ascii t))))
    
    (if* (and (null seenplus)
	      (null seen-non-ascii)
	      (eq 0 count))
       then ; move along, nothing to do here
	    (return-from un-hex-escape given))

    (macrolet ((cvt-ch (ch)
		 ;; convert hex character to numeric equiv
		 `(let ((mych (char-code ,ch)))
		    (if* (<= mych #.(char-code #\9))
		       then (- mych #.(char-code #\0))
		       else (+ 9 (logand mych 7))))))
			    
      (with-unhex-cvt-buffer (str (- len count))
	(do ((to 0 (1+ to))
	     (from 0 (1+ from)))
	    ((>= from len)
	     (cvt-buf-to-string str :end to :external-format external-format))
	  (let ((ch (schar given from)))
	    (if* (eq ch #\%)
	       then (let ((newchar 
			   (code-char (+ (ash (cvt-ch (schar given (1+ from))) 
					      4)
					 (cvt-ch (schar given (+ 2 from)))))))
		      (if* (and (eq newchar #\linefeed)
				(> to 0)
				(eq (buf-elt str (1- to)) #\return))
			 then ; replace return by linefeed
			      (decf to))
		      
		      (set-buf-elt str to newchar))
		    
		    (incf from 2)
	     elseif (and spacep (eq ch #\+))
	       then (set-buf-elt str to #\space)
	       else (set-buf-elt str to ch))))))))










;----------------- base64 --------------------


;;;; encoding algorithm:
;; each character is an 8 bit value.
;; three 8 bit values (24 bits) are turned into four 6-bit values (0-63)
;; which are then encoded as characters using the following mapping.
;; Zero values are added to the end of the string in order to get
;; a size divisible by 3 (these 0 values are represented by the = character
;; so that the resulting characters will be discarded on decode)
;; 
;; encoding
;; 0-25   A-Z
;; 26-51  a-z
;; 52-61  0-9
;; 62     +
;; 63     /
;;


(defvar *base64-decode* 
    ;;
    ;; use in decoding to map characters to values
    ;;
    (let ((arr (make-array 128 
			   :element-type '(unsigned-byte 8)
			   :initial-element 0)))
      (do ((i 0 (1+ i))
	   (ch (char-code #\A) (1+ ch)))
	  ((> ch #.(char-code #\Z)))
	(setf (aref arr ch) i))
      (do ((i 26 (1+ i))
	   (ch (char-code #\a) (1+ ch)))
	  ((> ch #.(char-code #\z)))
	(setf (aref arr ch) i))
      (do ((i 52 (1+ i))
	   (ch (char-code #\0) (1+ ch)))
	  ((> ch #.(char-code #\9)))
	(setf (aref arr ch) i))
      (setf (aref arr (char-code #\+)) 62)
      (setf (aref arr (char-code #\/)) 63)
      
      arr))


(defvar *base64-encode*
    ;;
    ;; used in encoding to map 6-bit values to characters
    ;;
    (let ((arr (make-array 64 :element-type 'character)))
      (dotimes (i 26)
	(setf (schar arr i)
	  (code-char (+ (char-code #\A) i))))
      (dotimes (i 26)
	(setf (schar arr (+ 26 i))
	  (code-char (+ (char-code #\a) i))))
      (dotimes (i 10)
	(setf (schar arr (+ 52 i))
	  (code-char (+ (char-code #\0) i))))
      (setf (schar arr 62) #\+)
      (setf (schar arr 63) #\/)
      arr))

;; note:  12/5/03
;; beginning in acl 6.2 patch excl.003 there are more efficient built-in 
;; functions for doing this conversion:
;;  excl:string-to-base64-string	[encode]
;;  excl:base64-string-to-string        [decode]
;;
;; At some future point we'll make use of these functions in AllegroServe
;; and drop the functions below.
;;


#+allegro
(defun base64-decode (string)
  (excl:base64-string-to-string string))

#-allegro
(defun base64-decode (string)
  ;;
  ;; given a base64 string, return it decoded.
  ;; beware: the result will not be a simple string
  ;;
  (let ((res (make-array (length string) :element-type 'character
			 :fill-pointer 0
			 :adjustable t))
	(arr *base64-decode*))
    (declare (type (simple-array (unsigned-byte 8) (128)) arr))
    (do ((i 0 (+ i 4))
	 (cha)
	 (chb))
	((>= i (length string)))
      
      ; for multiline decoding, ignore cr and lfs
      (loop
	(let ((ch (char string i)))
	  (if* (or (eq ch #\linefeed) (eq ch #\return))
	     then (incf i)
		  (if* (>= i (length string)) 
		     then (return-from base64-decode res))
	     else (return))))
	  
      (let ((val (+ (ash (aref arr (char-code (char string i))) 18)
		    (ash (aref arr (char-code (char string (+ i 1)))) 12)
		    (ash (aref arr (char-code 
				    (setq cha (char string (+ i 2)))))
			 6)
		    (aref arr (char-code 
			       (setq chb (char string (+ i 3))))))))
	(vector-push-extend (code-char (ash val -16)) res)
	;; when the original size wasn't a mult of 3 there may be
	;; non-characters left over
	(if* (not (eq cha #\=))
	   then (vector-push-extend (code-char (logand #xff (ash val -8))) res))
	(if* (not (eq chb #\=))
	   then (vector-push-extend (code-char (logand #xff val)) res))))
    res))


#+allegro
(defun base64-encode (str)
  (excl:string-to-base64-string str))

#-allegro
(defun base64-encode (str)
  ;;
  ;; take the given string and encode as a base64 string
  ;; beware: the result will not be a simple string
  ;;
  (let ((output (make-array (ceiling (* 1.3 (length str)))
			    :element-type 'character  
			    :fill-pointer 0
			    :adjustable t))
	v1 v2 v3 eol
	(from 0)
	(max (length str))
	)
      
    (loop
      (if* (>= from max) 
	 then (return))
      (setq v1 (char-code (schar str from)))
	
      (incf from)
	
      (if* (>= from max)
	 then (setq v2 0
		    eol t)
	 else (setq v2 (char-code (schar str from))))
	
      (incf from)
	
      ; put out first char of encoding
      (vector-push-extend (schar *base64-encode* (logand #x3f
							 (ash v1 -2)))
			  output)
	
      ; put out second char of encoding
	
      (vector-push-extend (schar *base64-encode* 
				 (+ (ash (logand 3 v1) 4)
				    (logand #xf (ash v2 -4))))
							   
			  output)
	
      (if* eol
	 then ; two pads
	      (vector-push-extend #\= output)
	      (vector-push-extend #\= output)
	      (return))
	
      (if* (>= from max)
	 then (setq v3 0
		    eol t)
	 else (setq v3 (char-code (schar str from))))
	
      (incf from)
	
	
      ; put out third char of encoding
	
      (vector-push-extend (schar *base64-encode* 
				 (+ (ash (logand #xf v2) 2)
				    (logand 3 (ash v3 -6))))
							   
			  output)
	
      (if* eol
	 then (vector-push-extend #\= output)
	      (return))
	
      ; put out fourth char of encoding
	
      (vector-push-extend (schar *base64-encode* (logand #x3f v3))
			  output))
      
    output))


