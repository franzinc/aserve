;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; parse.cl
;;
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
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
;;   parsing and encoding code  

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(in-package :net.aserve)

(check-smp-consistency)

;; parseobj -- used for cons-free parsing of strings
(defconstant parseobj-size 20)

(defstruct parseobj
  (start (make-array parseobj-size))  ; first charpos
  (end   (make-array parseobj-size))  ; charpos after last
  (next  0) ; next index to use
  (max  parseobj-size)
  )

(defvar-mp *parseobjs* nil)  ;; never bound; this is a global variable

(defun allocate-parseobj ()
  (smp-case
   ((t :macros)
    (let ((res (pop-atomic *parseobjs*)))
	(if* res
	   then (setf (parseobj-next res) 0)
		res
	   else (make-parseobj))))
   (nil
    (let (res)
      (mp::without-scheduling  ;; in a #-smp form
	(if* (setq res (pop *parseobjs*))
	   then (setf (parseobj-next res) 0)
		res
	   else (make-parseobj)))))))

(defun free-parseobj (po)
  (smp-case
   ((t :macros)
    (push-atomic po (si:global-symbol-value '*parseobjs*)))
   (nil
    (mp::without-scheduling ;; in a #-smp form
      (push po *parseobjs*)))))

(defun add-to-parseobj (po start end)
  ;; add the given start,end pair to the parseobj
  (let ((next (parseobj-next po)))
    (if* (>= next (parseobj-max po))
       then ; must grow it
	    (let ((ostart (parseobj-start po))
		  (oend   (parseobj-end   po)))
	      (let ((nstart (make-array (+ 10 (length ostart))))
		    (nend   (make-array (+ 10 (length ostart)))))
		(dotimes (i (length ostart))
		  (setf (svref nstart i) (svref ostart i))
		  (setf (svref nend   i) (svref oend   i)))
		(setf (parseobj-start po) nstart)
		(setf (parseobj-end   po) nend)
		(setf (parseobj-max   po) (length nstart))
		)))
  
    (setf (svref (parseobj-start po) next) start)
    (setf (svref (parseobj-end   po) next) end)
    (setf (parseobj-next po) (1+ next))
    next))

;;;;;;


















(defun parse-http-command (buffer end)
  ;; buffer is a string buffer, with 'end' bytes in it.  
  ;; return 3 values
  ;;	command  (kwd naming it or nil if bogus)
  ;;    url      uri object
  ;;    protocol  (kwd naming it or nil if bogus)
  ;;
  (let ((blankpos)
	(cmd)
	(urlstart))

    ; search for command first
    (dolist (possible *http-command-list* 
	      (return-from parse-http-command nil) ; failure
	      )
      (let ((str (car possible)))
	(if* (buffer-match buffer 0 str)
	   then ; got it
		(setq cmd (cdr possible))
		(setq urlstart (length (car possible)))
		(return))))
    
    
    (setq blankpos (find-it #\space buffer urlstart end))
    
    (if* (eq blankpos urlstart)
       then ; bogus, no url
	    (return-from parse-http-command nil))
    
    
    (if* (null blankpos)
       then ; must be http/0.9
	    (return-from parse-http-command (values cmd 
						    (parse-uri
						     (buffer-substr buffer
								   urlstart
								   end))
						    :http/0.9)))
    
    (let ((url (buffer-substr buffer urlstart blankpos))
	  (prot))
      
      ; parse url and if that fails get out right away
      (if* (null (setq url (parse-uri url)))
	 then (return-from parse-http-command nil))
      
      (if* (buffer-match buffer (1+ blankpos) "HTTP/1.")
	 then (if* (eq #\0 (schar buffer (+ 8 blankpos)))
		 then (setq prot :http/1.0)
	       elseif (eq #\1 (schar buffer (+ 8 blankpos)))
		 then (setq prot :http/1.1)))
      
      (values cmd url prot))))






(defun new-read-request-headers (req sock)
  ;; use the new strategy to read in the request headers
  ;;
  ;; return nil if we don't get all the way to the crlf crlf
  (let ((buff (get-sresource *header-block-sresource*))
	(end))
    
    (setf (request-header-block req) buff)
    ;; read in all the headers, stop at the last crlf
    (if* (setq end (read-headers-into-buffer sock buff))
       then (debug-format :xmit-server-request-headers "~s"
                          (octets-to-string buff :end end
                                            :external-format :octets))
            (let ((otherheaders (parse-header-block buff 0 end)))
	      
              (if* otherheaders
                 then ; non standard headers present...store
                      ; separately
                      (dolist (otherheader otherheaders)
                        (setf (car otherheader) 
                              (header-keywordify (car otherheader))))
                                
                      (setf (request-headers req)
                            (append (request-headers req) otherheaders))))
            
	    t)))
	 
(defvar *headername-to-kwd* nil)

(defun header-keywordify (name)
  ;; convert name to keyword.. check cache first

  ;; mm 2010-12: This code is not SMP-safe but little is lost thereby.
  ;;    The way this is coded, some kw conversions might get lost and
  ;;    thus need to be repeated, if a push gets overwritten.
  ;;    There may be duplicates in the alist if two threads try to add
  ;;    the same kw at the same time.
  ;;    But adding a lock seems like overkill.  
  ;;    Even the cache may be of dubious value (intern may be faster than assoc).

  (or (cdr (assoc name *headername-to-kwd* :test #'equal))
      (let ((kwd (intern (if* (eq *current-case-mode* :case-insensitive-upper)
			    then (string-upcase name)
			    else name)
			 (find-package :keyword))))
	(push (cons name kwd) *headername-to-kwd*)
	
	kwd)))
 





(defun read-headers-into-buffer (sock buff)
  ;; read the header data into the buffer
  ;; return the index of the character following the last header
  ;; or nil if the whole header wasn't read in
  ;;
  (let ((len (- (length buff) 500)) ; leave space for index at end
	(i 0)
	(state 2))
	
    (loop
      (if* (>= i len) 
	 then (return nil)) ; failure
      
      (let ((ch (read-byte sock nil nil)))
	(if* (null ch) 
	   then (return nil) ; eof - failure
		)
	
	; enable this if things are really messed
	; up and you need to see characters as they come in
	#+ignore
	(if* echo 
	   then (write-char (code-char ch) *debug-stream*)
		(force-output *debug-stream*))
	
	
	(setf (aref buff i) ch)
	(incf i)
	(case state
	  (0 ; seen nothing interesting
	   (if* (eq ch #.(char-code #\linefeed))
	      then (setq state 2)
	    elseif (eq ch #.(char-code #\return))
	      then (setq state 1)))
	  (1 ; seen cr
	   (if* (eq ch #.(char-code #\linefeed))
	      then (setq state 2)
	    elseif (eq ch #.(char-code #\return))
	      then nil ; stay in 1
	      else (setq state 0)))
	  (2 ; seen [cr] lf
	   (if* (eq ch #.(char-code #\linefeed))
	      then (setq state 4)
	    elseif (eq ch #.(char-code #\return))
	      then (setq state 3)
	      else (setq state 0)))
	  (3 ; seen [cr] lf cr
	   (if* (eq ch #.(char-code #\linefeed))
	      then (setq state 4)
	    elseif (eq ch #.(char-code #\return))
	      then (setq state 1)
	      else (setq state 0))))
	
	(if* (eql state 4)
	   then ; all done
		; back up over [cr] lf
		(if* (>= i 2)
		   then (decf i 2)  ; i points at cr if there is one
			(if* (not (eq (aref buff i) #.(char-code #\return)))
			   then (incf i)))
		; i points to the [cr] lf
		(return i))))))

	  
	     
    
			






   
    
    
;------- header value parsing
;
; most headers value this format
;    value
;    value1, value2, value3
;    value; param=val; param2=val
;
; notes: in the comma separated lists, it's legal to use more than one
;	   comma between values in which case the intermediate "null" values
;          are ignored e.g   a,,b is the same as a,b
;
;        the semicolon introduces a parameter, it doesn't end a value.
;        the semicolon has a higher binding power than the comma,
;	 so    A; b=c; d=e, F
;           is two values, A and F, with A having parameters b=c and d=e.
;
;        A header value that doesn't follow the above rules in 
;	 the one for set-cookie
;	    set-cookie: val=yes; expires=Fri, 01-Jan-2010 08:00:00 GMT; path=/
;        note how it starts off with param=val, and then the value
;	 can have commas in it, thus we don't use comm as a separator

(defconstant ch-alpha 0)
(defconstant ch-space 1)
(defconstant ch-sep   2)  ; separators

(defvar *syntax-table*
    (let ((arr (make-array 256 :initial-element ch-alpha)))
      
      ; the default so we don't have to set it
      #+ignore (do ((code (char-code #\!) (1+ code)))
	  ((> code #.(char-code #\~)))
	(setf (svref arr code) ch-alpha))
      
      (setf (svref arr (char-code #\space)) ch-space)
      (setf (svref arr (char-code #\ff)) ch-space)
      (setf (svref arr (char-code #\tab)) ch-space)
      (setf (svref arr (char-code #\return)) ch-space)
      (setf (svref arr (char-code #\linefeed)) ch-space)
      
      (setf (svref arr (char-code #\,)) ch-sep)
      (setf (svref arr (char-code #\;)) ch-sep)
      (setf (svref arr (char-code #\()) ch-sep)
      
      arr))



(defun header-value-nth (parsed-value n)
  ;; return the nth value in the list of header values
  ;; a value is either a string or a list (:param value  params..)
  ;;
  ;; nil is returned if we've asked for a non-existant element
  ;; (nil is never a valid value).
  ;;
  (if* (and parsed-value (not (consp parsed-value)))
     then (error "bad parsed value ~s" parsed-value))
  
  (let ((val (nth n parsed-value)))
    (if* (atom val)
       then val
       else ; (:param value ...)
	    (cadr val))))
  

(defun header-value-member (val parsed-value)
  ;; test to see if the given value is a member of the list
  ;; of values in the parsed value.  parse the value if needed
  (if* parsed-value
     then (setq parsed-value (ensure-value-parsed parsed-value))
	  (dolist (par parsed-value)
	    (if* (consp par)
	       then (setq par (cadr par)))
	    (if* (equalp val par)
	       then (return t)))))

(defun ensure-value-parsed (str &optional singlep)
  ;; parse the header value if it hasn't been parsed.
  ;; a parsed value is a cons.. easy to distinguish
  (if* (consp str) 
     then str
     else (parse-header-value str singlep)))


	      

(defun parse-header-value (str &optional singlep (start 0) (end (length str)))
  ;; scan the given string and return either a single value
  ;; or a list of values.
  ;; A single value is a string or (:param value paramval ....) for
  ;; values with parameters.  A paramval is either a string or
  ;; a cons of two strings (name . value)  which are the parameter
  ;; and its value.
  ;;
  ;; if singlep is true then we expect to see a single value which
  ;; main contain commas.  This is seen when Netscape sends
  ;; an if-modified-since header and it may in fact be a bug in 
  ;; Netscape (since parameters aren't defined for if-modified-since's value)
  ;;

  ;; split by comma first
  (let (po res)
    
    (if* singlep
       then ; don't do the comma split, make everything
	    ; one string
	    (setq po (allocate-parseobj))
	    (setf (svref (parseobj-start po) 0) start)
	    (setf (svref (parseobj-end  po) 0) end)
	    (setf (parseobj-next po) 1)
       else (setq po (split-string str #\, t nil nil start end)))
    
		    
    
    ; now for each split, by semicolon
    
    (dotimes (i (parseobj-next po))
      (let ((stindex (parseobj-next po))
	    (params)
	    (thisvalue))
	(split-string str #\; t nil po
		      (svref (parseobj-start po) i)
		      (svref (parseobj-end   po) i))
	; the first object we take whole
	(setq thisvalue (trimmed-parseobj str po stindex))
	(if* (not (equal thisvalue ""))
	   then ; ok, it's real, look for params
		(do ((i (1+ stindex) (1+ i))
		     (max (parseobj-next po))
		     (paramkey nil nil)
		     (paramvalue nil nil))
		    ((>= i max)
		     (setq params (nreverse params))
		     )
		  
		  ; split the param by =
		  (split-string str #\= t 1 po
				(svref (parseobj-start po) i)
				(svref (parseobj-end   po) i))
		  (setq paramkey (trimmed-parseobj str po max))
		  (if* (> (parseobj-next po) (1+ max))
		     then ; must have been an equal
			  (setq paramvalue (trimmed-parseobj str po
							     (1+ max))))
		  (push (if* paramvalue
			   then (cons paramkey paramvalue)
			   else paramkey)
			params)
		  
		  (setf (parseobj-next po) max))
		
		(push (if* params
			 then `(:param ,thisvalue
				       ,@params)
			 else thisvalue)
		      res))))
    
    (free-parseobj po)
    
    (nreverse res)))
    



(defun parse-header-line-equals (str &optional (start 0) (end (length str)))
  ;; parse a header line consisting of comma separated values
  ;;
  ;;  a=b, c="d asd",  e="fff"
  ;;
  ;; return (("a" . "b") ("c" . "d asd") ("e" . "fff"))
  ;;
  (let ((po (allocate-parseobj))
	(res))
    
    (unwind-protect
	(progn
	  (split-string str #\, nil nil po start end)
	  
	  (do ((i 0 (1+ i))
	       (max (parseobj-next po))
	       (paramkey nil nil)
	       (paramvalue nil nil))
	      
	      ((>= i max))
	    
	    (split-string str #\= nil 1 po
				(svref (parseobj-start po) i)
				(svref (parseobj-end   po) i))
	    
	    (setq paramkey (trimmed-parseobj str po max))
	    
	    (if* (> (parseobj-next po) (1+ max))
		     then ; must have been an equal
			  (setq paramvalue (trimmed-parseobj str po
							     (1+ max))))
	    
	    (push (cons paramkey paramvalue) res)
	    
	    (setf (parseobj-next po) max)))
      
      (free-parseobj po))
      
    (nreverse res)))

(defun assoc-paramval (key paramvals)
  ;; search the paramvals for the given key.
  ;; this takes into account that paramvals isn't an assoc
  ;; list since the items my be strings or (string . string)
  ;; Also we use equalp as the test
  ;;
  (dolist (val paramvals)
    (if* (stringp val)
       then (if* (equalp key val)
	       then (return val))
     elseif (equalp (car val) key)
       then (return val))))

  
		
		
(defun trimmed-parseobj (str po index)
  ;; return the string pointed to by the given index in 
  ;; the parseobj -- trimming blanks around both sides
  ;;
  ;; if surrounded by double quotes, trim them off too
  
  (let ((start (svref (parseobj-start po) index))
	(end   (svref (parseobj-end   po) index)))
    
    ;; trim left
    (loop
      (if* (>= start end)
	 then (return-from trimmed-parseobj "")
	 else (let ((ch (schar str start)))
		(if* (eq ch-space (svref *syntax-table*
				       (char-code ch)))
		   then (incf start)
		   else (return)))))
    
    ; trim right
    (loop
      (decf end)
      (let ((ch (schar str end)))
	(if* (not (eq ch-space (svref *syntax-table* (char-code ch))))
	   then (incf end)
		(return))))
    
    ; trim matching double quotes
    (if* (and (> end (1+ start))
	      (eq #\" (schar str start))
	      (eq #\" (schar str (1- end))))
       then (incf start)
	    (decf end))
    
    ; make string
    (let ((newstr (make-string (- end start))))
      (dotimes (i (- end start))
	(setf (schar newstr i) 
	  (schar str (+ start i))))
      
      newstr)))
    
    
		  
		  
				
		  
    

(defun split-string (str split &optional 
			       magic-parens 
			       count 
			       parseobj
			       (start 0) 
			       (end  (length str)))
  ;; divide the string where the character split occurs
  ;; return the results in parseobj object
  (let ((po (or parseobj (allocate-parseobj)))
	(st start)
	)
    ; states
    ; 0 initial, scanning for interesting char or end
    (loop
      (if* (>= start end)
	 then (add-to-parseobj po st start)
	      (return)
	 else (let ((ch (schar str start)))
		
		(if* (eq ch split)
		   then ; end this one
			(add-to-parseobj po st start)
			(setq st (incf start))
			(if* (and count (zerop (decf count)))
			   then ; get out now
				(add-to-parseobj po st end)
				(return))
		 elseif (and magic-parens (eq ch #\())
		   then ; scan until matching paren
			(let ((count 1))
			  (loop
			    (incf start)
			    (if* (>= start end)
			       then (return)
			       else (setq ch (schar str start))
				    (if* (eq ch #\))
				       then (if* (zerop (decf count))
					       then (return))
				     elseif (eq ch #\()
				       then (incf count)))))
			   
			(if* (>= start end)
			   then (add-to-parseobj po st start)
				(return))
		 elseif (eq ch #\")
		   then ; double quoted value.. skip over this
			(loop
			  (incf start)
			  (if* (>= start end)
			     then (return)
			     else (setq ch (schar str start))
				  (if* (eq ch #\")
				     then (return)
				   elseif (eq ch #\\)
				     then ; single char quote
					  (incf start)
					  (if* (>= start end)
					     then (return)))))
			(if* (>= start end)
			   then (add-to-parseobj po st start)
				(return)
			   else (incf start))
		   else (incf start)))))
    po))


(defun split-on-character (str char &key count)
  ;; given a string return a list of the strings between occurances
  ;; of the given character.
  ;; If the character isn't present then the list will contain just
  ;; the given string.
  (let ((loc (position char str))
	(start 0)
	(res))
    (if* (null loc)
       then ; doesn't appear anywhere, just the original string
	    (list str)
       else ; must do some work
	    (loop
	      (push (subseq str start loc) res)
	      (setq start (1+ loc))
	      (if* count then (decf count))
	      (setq loc (position char str :start start))
	      (if* (or (null loc)
		       (eql 0 count))
		 then (if* (< start (length str))
			 then (push (subseq str start) res)
			 else (push "" res))
		      (return (nreverse res)))))))

    


(defun split-into-words (str)
  ;; split the given string into words (items separated by white space)
  ;;
  (let ((state 0)
	(i 0)
	(len (length str))
	(start nil)
	(res)
	(ch)
	(spacep))
    (loop
      (if* (>= i len)
	 then (setq ch #\space)
	 else (setq ch (char str i)))
      (setq spacep (eq ch-space (svref *syntax-table* (char-code ch))))
      
      (case state
	(0  ; looking for non-space
	 (if* (not spacep)
	    then (setq start i
		       state 1)))
	(1  ; have left anchor, looking for space
	 (if* spacep
	    then (push (subseq str start i) res)
		 (setq state 0))))
      (if* (>= i len) then (return))
      (incf i))
    (nreverse res)))


(defun parse-range-value (str)
  ;; parse the value passed to a Range header
  ;; return  (n .  m) n and m integers meaning bytes from n to m inclusive
  ;;         (nil . m) meaning the last m bytes 
  ;;	     (n . nil) meaning bytes from n to the end
  ;;
  (let ((top (split-on-character str #\=))
	(res))
    (and (equalp (car top) "bytes")
	 (stringp (cadr top))
	 (dolist (range (split-on-character (cadr top) #\,))
	   (let ((startend (split-on-character range #\-))
		 (first)
		 (second))
	     (if* (not (equal "" (car startend)))
		then (setq first (string-to-number (car startend))))
	     (if* (not (equal "" (cadr startend)))
		then (setq second (string-to-number (cadr startend))))
	     (push (cons first second) res))))
    (nreverse res)))
		     
	   
	

;; this isn't needed while the web server is running, it just
;; needs to be run periodically as new mime types are introduced.
#+ignore-until-mime-table-changed
(defun generate-mime-table (&optional (file "/etc/mime.types"))
  ;; generate a file type to mime type table based on file type
  (let (res)
    (with-open-file (p file :direction :input)
      (loop
	(let ((line (read-line p nil nil)))
	  (if* (null line) then (return))
	  (if* (and (> (length line) 0)
		    (eq #\# (schar line 0)))
	     thenret ; comment
	     else ; real stuff
		  (let ((data (split-into-words line)))
		    (if* data then (push data res)))))))
    (nreverse res)))
  
  
(defun match-head-p (val1 val2)
  ;; return t if val1 is a prefix of val2
  ;; val1 and val2 are simple strings
  (let ((len1 (length val1))
	(len2 (length val2)))
    (if* (<= len1 len2)
       then (dotimes (i len1 t)
	      (if* (not (eq (schar val1 i) (schar val2 i)))
		 then (return nil))))))
	    
(defun match-tail-p (val1 val2)
  ;; return t if val1 is a suffix of val2
  ;; val1 and val2 are simple strings
  (let ((len1 (length val1))
	(len2 (length val2)))
    (if* (<= len1 len2)
       then (let ((diff (- len2 len1)))
	      (dotimes (i len1 t)
		(if* (not (eq (schar val1 i) (schar val2 (+ diff i))))
		   then (return nil)))))))
		
  
;----
(defun split-namestring (file)
  ;; split the namestring into root and tail and then the tail
  ;; into name and type
  ;; 
  ;; any of the return value can be nil if the corresponding item
  ;; isn't present.
  ;;
  ;; rules for splitting the tail into name and type components:
  ;;  if the last period in the tail is at the beginning or end of the
  ;;  tail, then the name is exactly the tail and type is nil.
  ;;  Thus .foo and bar.  are just names, no type
  ;;  but .foo.c  has a name of ".foo" and a type of "c"
  ;;  Thus if there is a non-nil type then it means that 
  ;;    1. there will be a non nil name as well
  ;;    2. to reconstruct the filename you need to add a period between
  ;;       the name and type.
  ;;
  (let ((pos (min (or (or (position #\/ file :from-end t) most-positive-fixnum)
		      #+mswindows (position #\\ file :from-end t))))
	root
	tail)
    
    (if* (equal file "") then (return-from split-namestring nil))
    
    (if* (and pos (< pos most-positive-fixnum))
       then ; we have root and tail
	    (if* (eql pos (1- (length file)))
	       then ; just have root
		    (return-from split-namestring
		      (values file nil nil nil)))
	    
    
	    (setq root (subseq file 0 (1+ pos))
		  tail (subseq file (1+ pos)))
       else (setq tail file))
    
    
    ; split the tail
    (let ((pos (position #\. tail :from-end t)))
      (if* (or (null pos)
	       (zerop pos)
	       (equal pos (1- (length tail))))
	 then ; name begins or ends with . so it's not
	      ; a type separator
	      (values root tail tail nil)
	 else ; have all pieces
	      (values root tail
		      (subseq tail 0 pos)
		      (subseq tail (1+ pos)))))))
