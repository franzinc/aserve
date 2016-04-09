;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; headers.cl
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
;;   header parsing

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(in-package :net.aserve)

(defvar *header-byte-array*
    ;; unsigned-byte 8 vector contains the characters referenced by
    ;; the *header-lookup-array*  .  All characters are downcased.
    )

(defvar *header-lookup-array*
    ;; indexed by the length of the header name.
    ;; value is list of (hba-index header-number) for all headers
    ;; that have a name this length
    ;; hba index is the index in the *header-byte-array* where the header
    ;; name begins.
    ;; the header-number is in index into..
    )


;; *header-keyword-array* defvar'ed in main.cl
;; indexed by header-number, holds the keyword naming this header

(defvar *header-name-array*
    ;; indexed by header-number, holds the string containing the 'pretty'
    ;; version of this header
    )

(defvar *header-client-array*
    ;; indexed by header number, contains a kwd symbol specifying how
    ;; to treat his header when proxying a client request to a server
    )

(defvar *header-server-array*
    ;; indexed by header number, contains a kwd symbol specifying how
    ;; to treat his header when proxying a server response to a client
    )

#+ignore
(defvar *header-cache-match-array*
    ;; indexed by header number.  contains a kwd symbol or nil
    ;; describing how matching should be done
    )

(defvar *header-count*
    ;; the number of headers we're tracking
    )
    

(defconstant *header-block-size* 8192) ; bytes in a header block
(defconstant *header-block-used-size-index*
    ;; where the size of lower part of the buffer is kept
    (- *header-block-size* 2))
(defconstant *header-block-data-start-index*
    ;; where the start of the lowest data block is stored
    (- *header-block-size* 4))

(defmacro header-block-header-index (index)
  ;; where in the buffer the 2byte entry for header 'index' is located
  `(- *header-block-size* 6  (ash ,index 1)))

(eval-when (compile eval)
  ;; the headers from the http spec
  ;; Following the header name we specify how to 
  ;; 1.  transfer client  request headers and 
  ;; 2  server response headers
  ;;     (header-name client  server)
  ;;    client/server
  ;;		:p  - pass this header on
  ;;    	:np - don't pass this header on verbatim
  ;;		:nf - this header won't be found
  ;; 3.  how the proxy-cache compares a new request-header against
  ;;	 the request header stored with a cached response. 
  ;;     :mx - need exact match 
  ;;	 :mp - either exact match or no value for new request-header
  ;;     nil - no match needed
  ;;
  ;;***** note well: adding something to this table must be accompanied
  ;;      by modifying *headers-count* above and then removing all caches
  ;;      if we're using a proxy cache.
  (defparameter *http-headers* 
      '(("Accept" :p :nf :mp) 
	("Accept-Charset" :p :nf :mp)
	("Accept-Encoding" :p :nf :mp)
	("Accept-Language" :p :nf :mp)
	("Accept-Ranges" :p :nf   :mx)
	("Age" :nf :p nil)
	("Allow" :nf :p nil)
	("Authorization" :p :nf :mx)
	("Cache-control" :p :p  :mp)
	("Connection" :np :np nil)
	("Content-Disposition" :nf :nf nil) ; in multipart/form-data bodies
	("Content-Encoding" :p :p :mx)
	("Content-Language" :p :p :mx)
	("Content-Length" :np :np nil)
	("Content-Location" :p :p :mx)
	("Content-Md5" :p :p :mx)
	("Content-Range" :p :p :mx)
	("Content-Type" :p :p :mx)
	("Cookie" :p :p :mx)
	("Date" :p :p nil)
	("Etag" :nf :p nil)
	("Expect" :p :nf :mx)
	("Expires"             :nf   :p  nil)
	("From"                :p    :nf :mp)  ; mp?
	("Host"                :np   :nf :mx)
	("If-Match"            :p    :nf :mx)
	("If-Modified-Since"   :p    :n   nil)
	("If-None-Match"       :p    :nf :mx)
	("If-Range"            :p    :nf :mx)
	("If-Unmodified-Since" :p    :nf nil)
	("Last-Modified"       :nf   :p  nil)
	("Location" 	       :nf   :p  nil)
	("Max-Forwards"        :np   :nf nil)  
	("Pragma"              :p    :p  nil) ; on reloads browsers add pragms
	("Proxy-Authenticate"  :nf   :p   nil)
	("Proxy-Authorization" :p    :nf :mx)
	("Range" :p :nf :mx)
	("Referer" :p :nf nil)  ; should we match? who cares..
	("Retry-After" :nf :p nil )
	("Server" :nf :p nil)
	("Set-Cookie" :nf :p nil)
	("Status" :nf :nf nil) ; not real header but found in cgi responses
	("TE" :p :nf   :mx)
	("Trailer" :np :np nil)
	("Transfer-Encoding" :np :np nil)
	("Upgrade" :np :nf nil)
	("User-Agent" :p :nf :mp)
	("Vary" :nf :p nil)
	("Via" :np :np  nil)  ; modified by proxy both dierctions
	("Warning" :p :p :mx)
	("WWW-Authenticate" :nf :p nil)
	)))

;; number of headers.
;; we take advantage of this being a constant in the code below and 
;; in the proxy caches.  If this number should change all proxy caches
;; should be removed.
(defconstant *headers-count* #.(length *http-headers*))

(defmacro header-block-data-start ()
  ;; return index right above the first data index object stored
  `(- *header-block-size* 4 (* *headers-count* 2)))




(eval-when (compile eval)
  (defmacro build-header-lookup-table ()
    (let ((max-length 0)
	  (total-length 0))
      ; compute max and total length
      (dolist (header *http-headers*)
	(setq header (car header))
	(let ((len (length header)))
	  (setq max-length (max max-length len))
	  (incf total-length len)))
    
      (let ((header-byte-array (make-array total-length
					   :element-type '(unsigned-byte 8)))
	    (header-lookup-array (make-array (1+ max-length))))
      
	(let ((hba -1)
	      (header-number -1)
	      (header-kwds)
	      (plists)
	      )
	  (dolist (header *http-headers*)
	    (setq header (car header))
	    (let ((len (length header)))

	      (setq header (string-downcase header))
	      (let ((header-keyword (read-from-string 
				     (format nil ":~a" 
					     header))))
		(push header-keyword header-kwds)
		(push (list (1+ hba) 
			    (incf header-number))
		      (aref header-lookup-array len))
		(push (cons header-keyword header-number) plists)
		)
	  
	      (dotimes (i len)
		(setf (aref header-byte-array (incf hba)) 
		  (char-code  (schar header i))))))
	

	  `(progn (setq *header-byte-array* ',header-byte-array)
		  (setq *header-lookup-array* ',header-lookup-array)
		  (setq *header-keyword-array*
		    ',(make-array (length header-kwds)
				  :initial-contents
				  (reverse header-kwds)))
		  (setq *header-name-array*
		    ',(make-array (length *http-headers*)
				  :initial-contents
				  (mapcar #'first *http-headers*)))
		  
		  (setq *header-client-array*
		    ',(make-array (length *http-headers*)
				  :initial-contents
				  (mapcar #'second *http-headers*)))
		  
		  (setq *header-server-array*
		    ',(make-array (length *http-headers*)
				  :initial-contents
				  (mapcar #'third *http-headers*)))
		  
		  #+ignore
		  (setq *header-cache-match-array*
		    ',(make-array (length *http-headers*)
				  :initial-contents
				  (mapcar #'fourth *http-headers*)))
		  
		  (setq  *header-count* 
		    ;; number of distinct headers
		    ,(1+ header-number))
		  
		  (if* (not (eql *header-count* *headers-count*))
		     then (error "setq *headers-count* to ~d in headers.cl" 
				 *header-count*))
		  
			    
		  (dolist (hkw ',plists)
		    (setf (get (car hkw) 'kwdi) (cdr hkw)))
		  
		  ))))))
		  
		  
	  
(build-header-lookup-table)



(defparameter *header-block-sresource*
    ;; 8192 element usb8 arrays
    ;; used to hold header contents with index at the end
    (create-sresource
     :create #'(lambda (sresource &optional size)
		 (declare (ignore sresource))
		 (if* size
		    then (error "size can't be specifed for header blocks"))
		 
		 (make-array *header-block-size*
			     :element-type '(unsigned-byte 8)))))

(defparameter *header-block-plus-sresource*
    ;; (+ 8192 100) element usb8 arrays
    ;; used to hold things slight larger than a header block will hold
    (create-sresource
     :create #'(lambda (sresource &optional size)
		 (declare (ignore sresource))
		 (if* size
		    then (error "size can't be specifed for header blocks"))
		 
		 (make-array (+ *header-block-size* 100)
			     :element-type '(unsigned-byte 8)))))

(defparameter *header-index-sresource*
    ;; used in parsing to hold location of header info in header-block
    (create-sresource
     :create #'(lambda (sresource &optional size)
		 (declare (ignore sresource))
		 (if* size
		    then (error "size can't be specifed for header index"))
		 
		 (make-array *header-count*))
     :init #'(lambda (sresource buffer)
	       (declare (ignore sresource))
	       (dotimes (i (length buffer))
		 (setf (svref buffer i) nil)))))

(defun get-header-block ()
  (get-sresource *header-block-sresource*))




(defun free-header-blocks (blocks)
  ;; free a list of blocks
  (dolist (block blocks)
    (free-sresource *header-block-sresource* block)))

(defun free-header-block (block)
  (if* (and block (atom block))
     then (free-sresource *header-block-sresource* block)
   elseif block
     then (error "bad value passed to free-header-block ~s" block)))


(defun get-header-plus-block ()
  (get-sresource *header-block-plus-sresource*))

(defun free-header-plus-block (block)
  (if* block then (free-sresource *header-block-plus-sresource* block)))

	      
;; parsed header array
;;  We have to work with headers and to reduce consing we work with
;;  them in place in a structure called a parsed header block
;;  This is stored in a 8192 byte usb8 vector.
;;  The layout is:
;;  headers and values .. empty .. data-block header-index-block min-db size
;;
;;  The headers and values are exactly what's read from the web client/server.
;;  they end in a crlf after the last value.
;;  
;;  The size is a 2 byte value specifying the index after the last 
;;  header value.  It says where we can add new headers if we want.
;;  All values greater than 2 bytes are stored in big endian form.
;;
;;  min-db is a 2 byte value telling where the lowest data-block entry starts
;;
;;  The header-index-block is 2 bytes per entry and specifies an index
;;  into the data-block where a descriptor for the value associated with
;;  this header are located.  This file lists the headers we know about
;;  and each is given an index.  The header-index-block is stored
;;  so that index 0 is closest to the end of the array
;;  The data blocks have the format 
;;      count(1) start1(2) end1(2) ... ... startN(2) endN(2)
;;  which describes how many header values there are and where they
;;  are.  end is one byte beyond the header value.

(defmacro unsigned-16-value (array index)
  (let ((gindex (gensym)))
    `(let ((,gindex ,index))
       (declare (fixnum ,gindex))
       (the fixnum
	 (+ (the fixnum (ash (aref ,array ,gindex) 8))
	    (aref ,array (1+ ,gindex)))))))

(defsetf unsigned-16-value (array index) (value)
  (let ((gindex (gensym))
	(gvalue (gensym)))
    `(let ((,gindex ,index)
	   (,gvalue ,value))
       (setf (aref ,array ,gindex) (hipart ,gvalue))
       (setf (aref ,array (1+ ,gindex)) (lopart ,gvalue))
       ,gvalue)))

(defmacro hipart (x)
  `(the fixnum (logand #xff (ash (the fixnum ,x) -8))))

(defmacro lopart (x) 
  `(the fixnum (logand #xff (the fixnum ,x))))



(defun parse-header-block-internal (buff start end ans)
  ;; the buff is an (unsigned-byte 8) array containing headers
  ;; and their values from start to just before end.
  ;; ans is a simple-vector large enough to hold *header-count* values.
  ;;
  ;; modify ans to store by header-number showing what, if any, values
  ;; are associated with this header.  The values are a list
  ;; of cons (start . end) meaning the value is from start to end-1
  ;; spaces are trimmed from the sides
  ;;
  
  (let ((i start)
	(state 0)
	beginhv
	beginh
	hnum
	ch
	otherheaders
	otherheadername
	)
    (macrolet ((tolower-set (loc)
		 ;; return the value at loc, convert it to lower
		 ;; case and store back if the conversion was done
		 (let ((var (gensym)))
		   `(let ((,var ,loc))
		      (if* (<= #.(char-int #\A) ,var #.(char-int #\Z))
			 then ; must lower case
			      (incf ,var #.(- (char-int #\a) (char-int #\A)))
			      (setf ,loc ,var))
		      ,var))))
		       
      (block done
	(loop
	  ;(format t "i: ~d, st ~d  ch ~s~%" i state (code-char (aref buff i)))
	  (case state
	    (0 ; beginning a header
	     (if* (>= i end) 
		then (return))
      
	     ; starting out look for something in the character range
	     (setq ch (tolower-set (aref buff i)))
	     (if* (not (<= #.(char-int #\a) ch #.(char-int #\z)))
		then ; this can't be a header start
		     ; skip to the eol
		     (setq state 1)
		else ; got a header start, skip to the next colon
		     (setq beginh i)
		     (incf i)
		     (loop
		       (if* (>= i end) then (return-from done))
		
		       (setq ch (tolower-set (aref buff i)))
		       (if* (eq ch #.(char-int #\:))
			  then ; found a header
			       (setq hnum
				 (locate-header buff beginh i))
			       (incf i)
			       (if* (null hnum)
				  then ; unknown header, save specially
				       (setq otherheadername
					 (buffer-subseq-to-string
					  buff beginh (1- i))))
			       (setq state 2) ; skip to value
			       (return)
			  else (incf i)))))
	
	    (1 ; skip to eol ( a linefeed in this case)
	     (if* (>= i end) then (return))
	     (loop
	       (setq ch (aref buff i))
	       (if* (eq ch #.(char-int #\linefeed))
		  then (setq state 0)
		       (incf i)
		       (return)
		  else (incf i))
	       (if* (>= i end) then (return-from done))
	       ))
	
	    (2 ; accumulate a header value
	     (if* (>= i end) then (return))
	     (if* (null beginhv) then (setq beginhv i))
	     (loop
	       (setq ch (aref buff i))
	       (if* (eq ch #.(char-int #\linefeed))
		  then (incf i)
		       (return))
	       (incf i)
	       (if* (>= i end) then (return-from done)))
	 
	     ; hit eol, but there still could be a continuation
	     (setq state 3))
	
	    (3 ; read a header line, now this could be a continuation
	     ; or a new header or eo headers
	     (if* (or (>= i end)
		      (not (eq (aref buff i) #.(char-int #\space))))
		then ; end of one header's value
		     ; backup and ignore cr lf 
		     (let ((back (1- i)))
		       (loop
			 (let ((ch (aref buff back)))
			   (if* (or (eq ch #.(char-code #\return))
				    (eq ch #.(char-code #\linefeed)))
			      then (decf back)
			      else (return))))
		       
		       (incf back)
		       
		       ; now strip spaces from beginning 
		       (loop
			 (if* (>= beginhv back)
			    then (return)
			  elseif (eq (aref buff beginhv) #.(char-code #\space))
			    then (incf beginhv)
			    else (return)))
		       
		       ; strip from end
		       (loop
			 (if* (>= beginhv back)
			    then (return)
			  elseif (eq (aref buff (1- back))
				     #.(char-code #\space))
			    then (decf back)
			    else (return)))
		       
		       ; must keep the header items in the same order 
		       ; they were received (according to the http spec)
		       (if* hnum
			  then ; known header
			       (let ((cur (svref ans hnum))
				     (new (list (cons beginhv  back))))
				 (if* cur
				    then (setq cur (append cur new))
				    else (setq cur new))
			 
				 (setf (svref ans hnum) cur))
			  else ; unknown header
			       (push (cons (header-kwdize otherheadername)
					   (buffer-subseq-to-string
					    buff beginhv back))
				     otherheaders))
			 
				 
		       (setq beginhv nil)
		       (setq state 0))
		else (setq state 2))))))
      
    
      otherheaders)))

(defun parse-header-block (buff start end)
  (let ((ans (get-sresource *header-index-sresource*))
	(otherheaders))
    
    (setq otherheaders (parse-header-block-internal buff start end ans))
    
    ; store the info in ans into the buffer at the end

    (let* ((table-index (header-block-header-index 0))
	   (data-index (header-block-data-start)))
      (dotimes (i (length ans))
	(let ((data (svref ans i)))
	  (if*  data
	     then ; must store data and an index to it
		  (let* ((data-len (length data))
			 (size (+ 1 ; count
				  (ash data-len 2) ; 4 bytes per data entry
				  )))
		    (decf data-index size)

		    (setf (unsigned-16-value buff table-index) data-index)
		    
		    (setf (aref buff data-index) data-len)
		    (let ((i (1+ data-index)))
		      (dolist (datum data)
			(setf (unsigned-16-value buff i) (car datum))
			(incf i 2)
			  
			(setf (unsigned-16-value buff i) (cdr datum))
			(incf i 2))))
	     else ; nothing there, zero it out
		  (setf (aref buff table-index) 0)
		  (setf (aref buff (1+ table-index)) 0)))
	(decf table-index 2))
	
      (setf (unsigned-16-value buff *header-block-used-size-index*) end)
      (setf (unsigned-16-value buff *header-block-data-start-index* )
	data-index)
	      
    
      (if* (> end data-index)
	 then (error "header is too large")))
    
    (free-sresource *header-index-sresource* ans)
    otherheaders))


(defun free-req-header-block (req)
  ;; if the req has an associated header block, give it back
  (free-sresource *header-block-sresource* (request-header-block req))
  (setf (request-header-block req) nil))

			
(defun header-buffer-values (buff header-index)
  ;; the buff is a usb8 array that has been built by parse-header-block
  ;; we are asked to return the location of the value(s) for the header
  ;; with the given index
  ;; we return nil if the header has no value
  ;; otherwise we return values
  ;;  start index
  ;;  end index
  ;;  list of (start-index . end-index) for the rest of the values, if any
    
  ;; be a nice guy and handle a symbolic header keyword name
  (if* (symbolp header-index)
     then (let ((ans (get header-index 'kwdi)))
	    (if* (null ans)
	       then (error "no such header as ~s" header-index))
	    (setq header-index ans)))
    
	    
  (let ((table-index (header-block-header-index header-index))
	(data-index))
    
    (setq data-index (unsigned-16-value buff table-index))

    
    (if* (< 0 data-index (length buff))
       then ; get values
	    (let ((count (aref buff data-index))
		  (first-start (unsigned-16-value buff (+ 1 data-index)))
		  (first-end   (unsigned-16-value buff (+ 3 data-index))))
	      (if* (> count 1)
		 then ; must get a list of the rest
		      (incf data-index 5)
		      (let (res)
			(dotimes (i (1- count))
			  (push (cons (unsigned-16-value buff data-index)
				      (unsigned-16-value buff 
							 (+ 2 data-index)))
				res)
			  (incf data-index 4))
			(values first-start
				first-end
				(nreverse res)))
		 else (values first-start first-end))))))


(defun buffer-subseq-to-string (buff start end)
  ;; extract a subsequence of the usb8 buff and return it as a string
  (let ((str (make-string (- end start))))
    (do ((i start (1+ i))
	 (ii 0 (1+ ii)))
	((>= i end))
      (setf (schar str ii) 
	(code-char (aref buff i))))
    str))

(defun header-buffer-req-header-value (req header)
  ;; see header-buffer-header-value for what this does.
  (let ((buff (request-header-block req)))
    ; there will be no buffer for http/0.9 requests
    (and buff  
	 (header-buffer-header-value (request-header-block req) header))))


(defun header-buffer-header-value (buff header)
  ;; header is a number or keyword symbol.
  ;; return nil or the header value as a string
  ;; 
  ;; according to the http spec, multiple headers with the same name
  ;; is only allowed when the header value is a comma separated list
  ;; of items, and the sequence of header values can be considered
  ;; as one big value separated by commas
  ;;
  (if* (symbolp header)
     then (setq header (get header 'kwdi)))
  
  (if* (fixnump header)
     then 
	  (multiple-value-bind (start end others)
	      (header-buffer-values buff header)
	    ; we only get the first value
	    (if* start
	       then (let ((ans (buffer-subseq-to-string buff start end)))
		      (if* others
			 then ; must concatente the others as well
			      (let (res)
				(dolist (oth others)
				  (push (buffer-subseq-to-string buff 
								 (car oth)
								 (cdr oth))
					res)
				  (push ", " res))
				(apply #'concatenate 'string ans res))
			 else ans))))))

	
	
(defun locate-header (buff start end)
  ;; find the header-index of the header between start and end in buff.
  ;; buffer is an usb8 array.
  ;; return nil if no match
  (let ((size (- end start))
	(hba *header-byte-array*))
    (if* (< 0 size (length *header-lookup-array*))
       then (dolist (header (svref *header-lookup-array* size))
	      (let ((begin (car header)))
		(if* (dotimes (i size t)
		       (if* (not (eq (aref buff (+ start i))
				     (aref hba (+ begin i))))
			  then (return nil)))
		   then ; match
			(return (cadr header))))))))



		
(defun compute-client-request-headers (sock)
  ;; for the client code we return a list of headers or signal
  ;; an error.
  ;; 
  (let* ((buff (get-sresource *header-block-sresource*))
	 (end (read-headers-into-buffer sock buff)))
    (if* end
       then (debug-format :xmit-client-response-headers "~a"
                          (octets-to-string buff :end end
                                            :external-format :octets))
            (prog1 (parse-and-listify-header-block buff end)
	      (free-sresource *header-block-sresource* buff))
       else (free-sresource *header-block-sresource* buff)
	    (error "Incomplete headers sent by server"))))


(defun parse-and-listify-header-block (buff end)
  ;; buff is a header-block 
  ;; parse the headers in the block and then extract the info
  ;; in assoc list form
  (let ((ans  (get-sresource *header-index-sresource*))
	(headers))
    
    ; store the non-standard headers in the header array
    (setq headers (parse-header-block-internal buff 0 end ans))
		   
    ; now cons up the headers
    (dotimes (i *header-count*)
      (let ((res (svref ans i)))
	(if* res
	   then (let ((kwd (svref *header-keyword-array* i)))
		  (dolist (ent res)
		    (let ((start (car ent))
			  (end   (cdr ent)))
		      (let ((str (make-string (- end start))))
			(do ((i start (1+ i))
			     (ii 0 (1+ ii)))
			    ((>= i end))
			  (setf (schar str ii)
			    (code-char 
			     (aref buff i))))
				     
			(push (cons kwd str) headers))))))))
		   
    (free-sresource *header-index-sresource* ans)
		   
    (nreverse headers)))

(defun listify-parsed-header-block (buff)
  ;; the header block buff has been parsed.
  ;; we just extract all headers in conses
  (let (res)
    (dotimes (i *headers-count*)
      (let ((val (header-buffer-header-value buff i)))
	(if* val
	   then (push (cons (aref *header-keyword-array* i) val) res))))
    (nreverse res)))
  
    

(defun initialize-header-block (buf)
  ;; set the parsed header block buf to the empty state
  
  ; clear out the indicies pointing to the values
  (let ((index (header-block-header-index 0)))
    (dotimes (i *header-count*)
      (setf (unsigned-16-value buf index) 0)
      (decf index 2)))
  
  ; no headers yet
  (setf (unsigned-16-value buf *header-block-used-size-index*) 0)
  
  ; start of where to put data
  (setf (unsigned-16-value buf *header-block-data-start-index*)
    (header-block-data-start))
  
  buf)
  
  
  
(defun copy-headers (frombuf tobuf header-array)
  ;; copy the headers denoted as :p (pass) in header array 
  ;; in frombuf to the tobuf
  ;;
  ;; return the index after the last header stored.
  (let ((toi 0)
	(data-index (header-block-data-start))
	(this-data-index)
	(header-index (header-block-header-index 0)))
    (dotimes (i (length header-array))
      (if* (eq :p (svref header-array i))
	 then ; passed intact
	      (multiple-value-bind (start end others)
		  (header-buffer-values frombuf i)
		(if* start
		   then (let ((items (1+ (length others))))
			  (decf data-index (1+ (* items 4)))
			  (setf (aref tobuf data-index) items)
			  
			  (setf (unsigned-16-value tobuf header-index)
			    data-index)
			  
			  
			  (setq this-data-index (1+ data-index)))
			(loop
			  (if* (null start) then (return))
			  (let ((name (svref *header-name-array* i)))
			    
			    ; copy in header name
			    (dotimes (j (length name))
			      (setf (aref tobuf toi) (char-code (schar name j)))
			      (incf toi))
			    
			    (setf (aref tobuf toi) #.(char-code #\:))
			    (incf toi)
			    (setf (aref tobuf toi) #.(char-code #\space))
			    (incf toi)

			    ; set the start address
			    (setf (unsigned-16-value tobuf this-data-index)
			      toi)
			    (incf this-data-index 2)
			    
			    
			    ; copy in the header value
			    (do ((j start (1+ j)))
				((>= j end))
			      (setf (aref tobuf toi) (aref frombuf j))
			      (incf toi))

			    ; set the end address
			    (setf (unsigned-16-value tobuf this-data-index)
			      toi)
			    (incf this-data-index 2)
			    

			    ; add the obligatory crlf
			    (setf (aref tobuf toi) #.(char-code #\return))
			    (incf toi)
			    (setf (aref tobuf toi) #.(char-code #\linefeed))
			    (incf toi))
			  (let ((next (pop others)))
			    (if* next
			       then (setq start (car next)
					  end   (cdr next))
			       else (return))))
		   else (setf (unsigned-16-value tobuf header-index) 0)))
	 else ; clear out the header index
	      (setf (unsigned-16-value tobuf header-index) 0))
      (decf header-index 2))

    (setf (unsigned-16-value tobuf *header-block-used-size-index*) toi)
    (setf (unsigned-16-value tobuf *header-block-data-start-index* )
      data-index)
    
    toi))

	      
(defun insert-header (buff header value)
  ;; insert the header (kwd symbol or integer) at the end of the current buffer
  ;; end is the index of the next buffer position to fill
  ;; return the index of the first unfilled spot of the buffer
  ;;
  (if* (symbolp header)
     then (let ((val (get header 'kwdi)))
	    (if* (null val)
	       then (error "no such header as ~s" header))
	    (setq header val)))
  (let ((end (unsigned-16-value buff *header-block-used-size-index*))
	(starth)
	(endh))
    (let ((name (svref *header-name-array* header)))
      (dotimes (j (length name))
	(setf (aref buff end) (char-code (schar name j)))
	(incf end))
      (setf (aref buff end) #.(char-code #\:))
      (incf end)
      (setf (aref buff end) #.(char-code #\space))
      (incf end)
      (setq starth end)
      (dotimes (j (length value))
	(setf (aref buff end) (char-code (schar value j)))
	(incf end))
      (setq endh end)
      (setf (aref buff end) #.(char-code #\return))
      (incf end)
      (setf (aref buff end) #.(char-code #\linefeed))
      (incf end))
  
    ; now insert the information about this header in the data list
    (let ((this-data-index (unsigned-16-value buff (header-block-header-index
						    header)))
	  (data-start (unsigned-16-value buff *header-block-data-start-index*)))
      (let ((count 0))
	(if* (not (zerop this-data-index))
	   then ; must copy this one down and add to it
		(setq count (aref buff this-data-index)))
	(incf count) ; for our new one
	(decf data-start (+ 1 (* count 4)))
	(setf (unsigned-16-value buff (header-block-header-index header))
	  data-start)
	(setf (unsigned-16-value buff *header-block-data-start-index*)
	  data-start)
	(setf (aref buff data-start) count)
	; copy in old stuff
	(incf this-data-index)
	(incf data-start)
	(dotimes (i (* 4 (1- count)))
	  (setf (aref buff data-start) (aref buff this-data-index))
	  (incf data-start)
	  (incf this-data-index))
    
	; store in new info
	(setf (unsigned-16-value buff data-start) starth)
	(setf (unsigned-16-value buff (+ 2 data-start)) endh)))
    
    ; new end of headers
    (setf (unsigned-16-value buff *header-block-used-size-index*) end)))
    


(defun insert-non-standard-header (buff name value)
  ;; insert a header that's not known by index into the buffer
  ;;
  (setq name (string name))
  
  (let ((end (unsigned-16-value buff *header-block-used-size-index*)))
    (if* (> (+ end (length name) (length value) 4) 
	    (header-block-data-start))
       then ; no room
	    (return-from insert-non-standard-header nil))
    
    (dotimes (i (length name))
      (setf (aref buff end) (char-code (aref name i)))
      (incf end))
    
    (setf (aref buff end) #.(char-code #\:))
    (incf end)
    
    (setf (aref buff end) #.(char-code #\space))
    (incf end)
    
    (dotimes (i (length value))
      (setf (aref buff end) (char-code (aref value i)))
      (incf end))
    
    (setf (aref buff end) #.(char-code #\return))
    (incf end)
    
    (setf (aref buff end) #.(char-code #\linefeed))
    (incf end)
    
    (setf (unsigned-16-value buff *header-block-used-size-index*) end)))
		
	       
    
  
			     
#+ignore
(defun insert-end-of-headers (buff end)
  ;; put in the final crlf
  (setf (aref buff end) #.(char-code #\return))
  (incf end)
  (setf (aref buff end) #.(char-code #\linefeed))
  (incf end)
  end)
	    


(defun header-match-values (request-block cache-block i exactp)
  ;; compare the header value for the current request vrs the cache-block
  ;; they match if they are identical
  ;; or if (not exact) and the request-block value is not given.
  ;;
  (multiple-value-bind (rstart rend rest)
      (header-buffer-values request-block i)
    (multiple-value-bind (cstart cend cest)
	(header-buffer-values cache-block i)
      
      (or (and (null rstart) (null cstart)) ; both not present
	  (and (null rstart) (not exactp)) ; not given in request and !exact
	  
	  ; check for both present and identical
	  (and rstart cstart
	       (eql (- rend rstart) (- cend cstart)) ; same size
	       (equal rest cest)
	       
	       (loop
		 (do ((rr rstart (1+ rr))
		      (cc cstart (1+ cc)))
		     ((>= rr rend))
		   (if* (not (eq (aref request-block rr)
				 (aref cache-block cc)))
		      then (return-from header-match-values nil)))
		 
		 (if* rest
		    then (setq rstart (caar rest)
			       rend   (cdar rest)
			       
			       cstart (caar cest)
			       cend   (cdar cest))
			 (pop rest)
			 (pop cest)
		    else (return t))))))))
	       

(defun header-match-prefix-string (buff header string)
  ;; match the prefix of the header vlue against the given string
  
  (if* (symbolp header)
     then (let ((val (get header 'kwdi)))
	    (if* (null val)
	       then (error "no such header as ~s" header))
	    (setq header val)))
  
  (multiple-value-bind (rstart rend)
      (header-buffer-values buff header)
    (if* (and rstart (>= (- rend rstart) (length string)))
       then ; compare byte by byte
	    (dotimes (i (length string) t)
	      (if* (not (eql (ausb8 buff rstart) 
			     (char-code (schar string i))))
		 then (return nil))
	      (incf rstart)))))

  
(defun header-empty-p (buff header)
  ;; test to see if there is no value for this header
  
  (if* (symbolp header)
     then (let ((val (get header 'kwdi)))
	    (if* (null val)
	       then (error "no such header as ~s" header))
	    (setq header val)))
  
  (header-buffer-values buff header))

  
	       
(defun add-trailing-crlf (buff xx)
  ;; buff is a parsed header block.
  ;; find the end of the data and add a crlf and then return the
  ;; index right after the linefeed
  (declare (ignore xx))
  (let ((size (unsigned-16-value buff *header-block-used-size-index*)))
    (if* (not (<= 0 size (header-block-data-start)))
       then (error "buffer likely isn't a parsed header block"))
    
    (setf (aref buff size) #.(char-code #\return))
    (setf (aref buff (1+ size)) #.(char-code #\linefeed))
    
    (+ size 2)))
    
	  

#+ignore (defun testit ()
  ;; test the parsing
  (let ((str (format nil "Date: 12 jul 2000   ~c~%User-Agent: zipp ~c~%Host: foo.com  ~c~%Via: foo~c~% bar~c~% baz~c~%User-Agent: snorter~c~%"
		     #\return
		     #\return
		     #\return
		     #\return
		     #\return
		     #\return
		     #\return)))
    (let ((thebuf (get-sresource *header-block-sresource*)))
      (dotimes (i (length str))
	(setf (aref thebuf i)
	  (char-int (schar str i))))
      (parse-header-block thebuf 0 (length str))
      (setq *xx* thebuf)

      (format t "original~%")
      (dump-header-block thebuf)
      
      (insert-header thebuf :last-modified "dec 11, 1945")
      (insert-header thebuf :user-agent  "new agent")
      
      (format t "after mod~%")
      (dump-header-block thebuf)
      
      (let ((newbuf (get-sresource *header-block-sresource*)))
	(copy-headers thebuf newbuf *header-client-array*)
	(format t "after copy ~%")
	(dump-header-block newbuf))
      )))

	

		    
(defun dump-header-block (thebuf &optional (str t))
  ;; debugging function to print out the contents of a block
  ;; buffer that has been parsed and the parse table stored in it.
  (dotimes (i *header-count*)
    (multiple-value-bind (start end rest)
	(header-buffer-values thebuf i)
      (if* start
	 then (push (cons start end) rest))
      (dolist (res rest)
	  
	(if* res
	   then (format str "~d: ~a ~s '" 
			i
			(svref *header-name-array* i)
			res)
		(do ((ind (car res) (1+ ind)))
		    ((>= ind (cdr res)))
		  (write-char (code-char (aref thebuf ind)) str))
		(format str "'")
		(terpri str))))))

(defun compute-request-headers (req)
  ;; compute an alist of all headers from the request
  ;; This is slow so it's meant to be used during debugging only.
  ;; 
  (let (res)
    (dotimes (i *header-count*)
      (let ((val (header-buffer-req-header-value req i)))
	(if* val 
	   then (push (cons (aref *header-keyword-array* i)
			    val)
		      res))))
    (nreverse res)))
  
	      
			
	      
      
  
  
    
    




