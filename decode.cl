;; neo system
;; decode/encode code
;;

(in-package :neo)




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


(defun base64-decode (string)
  ;;
  ;; given a base64 string, return it decoded.
  ;; beware: the result will not be a simple string
  ;;
  (let ((res (make-array 20 :element-type 'character
			 :fill-pointer 0
			 :adjustable t))
	(arr *base64-decode*))
    (declare (type (simple-array (unsigned-byte 8) 128) arr))
    (do ((i 0 (+ i 4))
	 (cha)
	 (chb))
	((>= i (length string)))
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


(defun base64-encode (str)
  ;;
  ;; take the given string and encode as a base64 string
  ;; beware: the result will not be a simple string
  ;;
  (let ((output (make-array 20 
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


