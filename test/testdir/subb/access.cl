;; need to be local and password approved

(:password :realm "foorealm"
	   :allowed (("joe" . "eoj")
		     ("fred" . "derf")))
(:ip :patterns ((:accept "127.1") :deny))
