
(asdf:defsystem #:fft
    :author "Patrick Stein <pat@nklein.com>"
    :maintainer "Patrick Stein <pat@nklein.com>"
    :licence "Public Domain"
    :components ((:file "package")
		 (:file "utils"
			:depends-on ("package"))
		 (:file "fft"
		        :depends-on ("package" "utils"))))
