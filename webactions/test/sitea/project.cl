(in-package :net.aserve.testwa)

(webaction-project "sitea"
		   :project-prefix "/sitea/"
		   :destination (directory-namestring *load-pathname*)
		   :map '(("pagea" "file1.clp")
			  ("action" 
			   action-sitea-pushit
			   action-sitea-pushit
			   action-sitea-pushit
			   "file1.clp")
			  
			  ("action2"
			   action-retname-two)
			  
			  ("action3"
			   action-retname-three)
			  
			  ("action4"
			   action-retname-four)
			  
			  ("act" action-sitea-pushit "file1.clp"
			   (:prefix t))
			  
			  ; push once and then redir to "action"
			  ("redirtry" action-sitea-pushit "action")
			  
			  ; test that the existing file file1.clp has
			  ; precedence over the prefix fil
			  ("fil"  action-sitea-pushit action-sitea-pushit
			   "file2.clp"
			   (:prefix t))
			  
			  ("testctype" "file3.clp")
			  ("file3.clp" (:content-type "text/plain"))
			  
			  ; test nested clp elements
			  ("file4"  "file4.clp")
			  ))
		   
		   


