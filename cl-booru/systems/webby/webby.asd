(asdf:defsystem
  :webby
  :version "0.1"
  :serial t
  :depends-on (:sb-fastcgi :cl-who :postmodern :split-sequence :cl-json :cl-ppcre :arnesi :rfc2388 :md5)
  :components ((:file "packages")
               (:file "utility")
               (:file "page")
               (:file "target")
               (:file "request")
               (:file "webapp")))
