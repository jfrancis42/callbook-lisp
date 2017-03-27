;;;; callbook.asd

(asdf:defsystem #:callbook
  :description "A library for pulling ham radio license info for US hams from the callbook.info API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:cl-json
	       #:babel
	       #:drakma)
  :serial t
  :components ((:file "package")
               (:file "callbook")))

