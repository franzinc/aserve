(defpackage :user (:use :htmlgen))

(defun simple-table-a ()
  (with-open-file (p "~/public_html/test.html"
		   :direction :output
		   :if-exists :supersede)
    
    (html-stream p 
		 (:html
		  (:head (:title "Test Table"))
		  (:body (:table 
			  (:tr (:td "0") (:td "0"))
			  (:tr (:td "1") (:td "1"))
			  (:tr (:td "2") (:td "4"))
			  (:tr (:td "3") (:td "9"))
			  (:tr (:td "4") (:td "16"))
			  (:tr (:td "5") (:td "25"))))))))

(defun simple-table-b ()
  (with-open-file (p "~/public_html/test.html"
		   :direction :output
		   :if-exists :supersede)
    
    (html-stream p 
		 (:html
		  (:head (:title "Test Table"))
		  (:body ((:table border 2)
			  (:tr (:td "0") (:td "0"))
			  (:tr (:td "1") (:td "1"))
			  (:tr (:td "2") (:td "4"))
			  (:tr (:td "3") (:td "9"))
			  (:tr (:td "4") (:td "16"))
			  (:tr (:td "5") (:td "25"))))))))


(defun simple-table-c (count)
  (with-open-file (p "~/public_html/test.html"
		   :direction :output
		   :if-exists :supersede)
    
    (html-stream p 
		 (:html
		  (:head (:title "Test Table"))
		  (:body ((:table border 2)
			  (dotimes (i count)
			    (html (:tr (:td (:princ i))
				       (:td (:princ (* i i))))))))))))

(defun simple-table-d (count border-width backg-color border-color)
  (with-open-file (p "~/public_html/test.html"
		   :direction :output
		   :if-exists :supersede)
    
    (html-stream p 
		 (:html
		  (:head (:title "Test Table"))
		  (:body ((:table border border-width
				  bordercolor  border-color
				  bgcolor backg-color
				  cellpadding 3)
			  (:tr ((:td bgcolor "blue") 
				((:font :color "white" :size "+1")
				 "Value"))
			       ((:td bgcolor "blue") 
				((:font :color "white" :size "+1")
				 "Square"))
			       )
			  (dotimes (i count)
			    (html (:tr (:td (:princ i))
				       (:td (:princ (* i i))))))))))))
		    


