;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; headers.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
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
;; $Id: headers.cl,v 1.2 2000/08/17 21:38:30 jkf Exp $

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

(defvar *header-keyword-array*
    ;; indexed by header-number, holds the keyword naming this header
    )

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

(defvar *header-count*
    ;; the number of headers we're tracking
    )
    

(eval-when (compile eval)
  ;; the headers from the http spec
  ;; Following the header name we specify how to transfer client
  ;; request headers and server response headers
  ;; (header-name client  server)
  ;; client/server
  ;;	:p  - pass this header on
  ;;    :np - don't pass this header on verbatim
  ;;	:nf - this header won't be found
  ;;
  (defparameter *http-headers* 
      '(("Accept" :p :nf) 
	("Accept-Charset" :p :nf)
	("Accept-Encoding" :p :nf)
	("Accept-Language" :p :nf)
	("Accept-Ranges" :p :nf)
	("Age" :nf :p)
	("Allow" :nf :p)
	("Authorization" :p :nf)
	("Cache-control" :p :p)
	("Connection" :np :np)
	("Content-Encoding" :p :p)
	("Content-Language" :p :p)
	("Content-Length" :np :np)
	("Content-Location" :p :p)
	("Content-Md5" :p :p)
	("Content-Range" :p :p)
	("Content-Type" :p :p)
	("Cookie" :nf :p)
	("Date" :p :p)
	("Etag" :p :p)
	("Expect" :np :nf)
	("Expires"             :nf   :p)
	("From"                :p    :nf)
	("Host"                :p    :nf)
	("If-Match"            :p    :nf)
	("If-Modified-Since"   :p    :nf)
	("If-None-Match"       :p    :nf)
	("If-Range"            :p    :nf)
	("If-Unmodified-Since" :p    :nf)
	("Last-Modified"       :nf   :p)
	("Location" 	       :nf   :p)
	("Max-Forwards"        :np   :nf)  
	("Pragma"              :p    :p)
	("Proxy-Authenticate"  :nf   :p)
	("Proxy-Authorization" :p    :nf)
	("Range" :p :nf)
	("Referer" :p :nf)
	("Retry-After" :nf :p)
	("Server" :nf :p)
	("TE" :p :nf)
	("Trailer" :np :np)
	("Transfer-Encoding" :np :np)
	("Upgrade" :np :nf)
	("User-Agent" :p :nf)
	("Vary" :nf :p)
	("Via" :np :np)  ; modified by proxy both dierctions
	("Warning" :p :p)
	("WWW-Authenticate" :nf :p)
	)))

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
		  
		  (setq  *header-count* 
		    ;; number of distinct headers
		    ,(1+ header-number))
		  
		  (dolist (hkw ',plists)
		    (setf (get (car hkw) 'kwdi) (cdr hkw)))
		  
		  ))))))
		  
		  
	  
(build-header-lookup-table)



(defparameter *header-block-sresource*
    ;; 4096 element usb8 arrays
    ;; used to hold header contents with index at the end
    (create-sresource
     :create #'(lambda (sresource &optional size)
		 (declare (ignore sresource))
		 (if* size
		    then (error "size can't be specifed for header blocks"))
		 
		 (make-array 4096
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
	  ; (format t "st ~d  ch ~s~%" state (code-char (aref buff i)))
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
				  then (setq state 1) ; skip to eol
				  else (setq state 2) ; skip to value
				       )
			       (return)
			  else (incf i)))))
	
	    (1 ; skip to eol ( a newline in this case)
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
	       (if* (eq ch #.(char-int #\newline))
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
		       (let ((cur (svref ans hnum))
			     (new (list (cons beginhv  back))))
			 (if* cur
			    then (setq cur (append cur new))
			    else (setq cur new))
			 
			 (setf (svref ans hnum) cur))
			 
				 
		       (setq beginhv nil)
		       (setq state 0))
		else (setq state 2))))))
    
      ans)))

(defun parse-header-block (buff start end)
  (let ((ans (get-sresource *header-index-sresource*)))
    (parse-header-block-internal buff start end ans)
    
    ; store the info in ans into the buffer at the end

    (macrolet ((hipart (x)
		 `(the fixnum (logand #xff (ash (the fixnum ,x) -8))))
	       (lopart (x) 
		 `(the fixnum (logand #xff (the fixnum ,x)))))
      (let* ((table-index (length buff))
	     (data-index (- table-index
			    (the fixnum (ash (the fixnum (length ans)) 1)))))
	(dotimes (i (length ans))
	  (decf table-index 2)
	  (let ((data (svref ans i)))
	    (if*  data
	       then ; must store data and an index to it
		    (let* ((data-len (length data))
			   (size (+ 1 ; count
				    (ash data-len 2) ; 4 bytes per data entry
				    )))
		      (decf data-index size)
		    
		      (setf (aref buff table-index) (hipart data-index))
		      (setf (aref buff (1+ table-index)) (lopart data-index))
		    
		      (setf (aref buff data-index) data-len)
		      (let ((i (1+ data-index)))
			(dolist (datum data)
			  (setf (aref buff i) (hipart (car datum)))
			  (setf (aref buff (+ 1 i)) (lopart (car datum))) 
			  (setf (aref buff (+ 2 i)) (hipart (cdr datum)))
			  (setf (aref buff (+ 3 i)) (lopart (cdr datum)))
			  (incf i 4))))
	       else ; nothing there, zero it out
		    (setf (aref buff table-index) 0)
		    (setf (aref buff (1+ table-index)) 0))))
    
	(if* (> end data-index)
	   then (error "header is too large"))))
    
    (free-sresource *header-index-sresource* ans)
    buff))


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
  (macrolet ((unsigned-16-value (array index)
	       `(the fixnum
		  (+ (the fixnum (ash (aref buff ,index) 8))
		     (aref buff (1+ ,index))))))
    
    ;; be a nice guy and handle a symbolic header keyword name
    (if* (symbolp header-index)
       then (let ((ans (get header-index 'kwdi)))
	      (if* (null ans)
		 then (error "no such header as ~s" header-index))
	      (setq header-index ans)))
    
	    
    (let ((table-index (- (length buff)
			  2
			  (ash header-index 1) ; *2 
			  ))
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
				  res))
			  (values first-start
				  first-end
				  (nreverse res)))
		   else (values first-start first-end)))))))

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
  (header-buffer-header-value (request-header-block req) header))


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
    (if* (<= 0 size *header-count*)
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
       then (let ((ans  (get-sresource *header-index-sresource*))
		  (headers))
	      (parse-header-block-internal buff 0 end ans)
		   
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
		   
	      (free-sresource *header-block-sresource* buff)
	      (free-sresource *header-index-sresource* ans)
		   
	      headers)
       else (free-sresource *header-block-sresource* buff)
	    (error "Incomplete headers sent by server"))))

							   
							   
		   
  

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
      ;(break "foo")

      
      (dotimes (i *header-count*)
	(let ((val (header-buffer-header-value thebuf i)))
	  (if* val
	     then (format t "~a: ~a~%"
			  (svref *header-name-array* i)
			  val)))))))
	

		    
(defun dump-header-block (thebuf)
  ;; debugging function to print out the contents of a block
  ;; buffer that has been parsed and the parse table stored in it.
  (dotimes (i *header-count*)
    (multiple-value-bind (start end rest)
	(header-buffer-values thebuf i)
      (if* start
	 then (push (cons start end) rest))
      (dolist (res rest)
	  
	(if* res
	   then (format t "~d: ~a ~s '" 
			i
			(svref *header-name-array* i)
			res)
		(do ((ind (car res) (1+ ind)))
		    ((>= ind (cdr res)))
		  (format t "~c" (code-char (aref thebuf ind))))
		(format t "'")
		(terpri))))))


	      
			
	      
      
  
  
    
    




