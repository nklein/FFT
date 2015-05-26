
(asdf:defsystem #:fft
    :author "Patrick Stein <pat@nklein.com>"
    :maintainer "Patrick Stein <pat@nklein.com>"
    :license "Public Domain"
    :version "0.1.20110324"
    :components ((:file "package")
		 (:file "utils"
			:depends-on ("package"))
		 (:file "fft"
		        :depends-on ("package" "utils"))))
