(defsystem "rsss"
  :version "0.1"
  :author "Jaidyn Ann <jadedctrl@teknik.io>"
  :license "GPLv3"
  :depends-on ("xmls")
  :components ((:module "src"
                :components
		((:file "package")
		(:file "main"))))
  :description
	"RSS parser library.")



