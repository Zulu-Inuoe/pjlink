(defsystem #:pjlink
  :version "0.0.0"
  :description ""
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:static-file "qlfile")
   (:file "package")
   (:file "pjlink")
   (:file "class1")
   (:file "class2"))
  :depends-on
  (#:alexandria
   #:flexi-streams
   #:md5
   #:split-sequence
   #:usocket))
