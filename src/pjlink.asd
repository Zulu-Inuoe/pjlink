(defsystem #:pjlink
  :version "0.0.0"
  :description ""
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :serial t
  :components
  ((:static-file "qlfile")
   (:file "package")
   (:file "pjlink"))
  :depends-on
  (#:alexandria
   #:flexi-streams
   #:md5
   #:usocket))
