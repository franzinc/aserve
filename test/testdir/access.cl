;; access file
(:subdirectories :deny "subc")
(:subdirectories :deny "subd" :inherit t)

(:mime :types (("foo/bar" "foo" "foo2") ("foo/baz" "baz")))

(:files :deny ("^access\\.cl$"))  ; ignore exactly access.cl
(:files :deny  "\\.ign$")	   ; ignore all files ending in .ign

; allow from localhost only and don't inherit this rule
(:ip :patterns ((:accept "127.1") 
		:deny)
     :inherit nil)







       
