(in-package :net.aserve.testwa)

(webaction-project "siteb"
		   :project-prefix "/siteb/"
		   :destination (directory-namestring *load-pathname*)
		   :map '(("pageb" "sub/file1.clp")))
