(in-package :net.aserve.testwa)

(defparameter *sitea-webaction-entity*
    (webaction-project "sitea"
		   :project-prefix "/sitea/"
		   :destination (directory-namestring *load-pathname*)
                   :session-cookie-only *sitea-session-cookie-only*
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
                          
                          ;; redirect test
                          ("redirtest" "action" (:redirect t))
                          
                          ;; integer return code test
                          ("retcodetest" "ret201.clp")
                          
                          ;; session var return code test
                          ("retanycodetest" action-set-retany)
                          
                          ;; here the value is specified as a string
                          ("retanycodetest-two" action-set-retany-two)
                          
                          ;; here we have a bogus string value
                          ("retanycodetest-bogus" action-set-retany-bogus)
                          
                          ;; test clp_include with args
                          ("retincluded" "retwithinclude.clp")
                          
                          )))
		   
		   


