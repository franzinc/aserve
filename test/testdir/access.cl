;; access file

(:mime :types (("foo/bar" "foo" "foo2") ("foo/baz" "baz")))

(:files :ignore ("^access\\.cl$"  ; ignore exactly access.cl
		  "\\.ign$"	   ; ignore all files ending in .ign
		  ))

; allow from localhost only and don't inherit this rule
(:ip :patterns ((:accept "127.1") 
		:deny)
     :inherit nil)







       
