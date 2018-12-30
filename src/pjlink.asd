(defsystem #:pjlink
  :version "1.0.1"
  :description "A library for communicating with PJLink-compatible projectors over TCP/IP.
see https://pjlink.jbmia.or.jp/english/ for information on PJLink and compatible devices."
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
   #:bordeaux-threads
   #:flexi-streams
   #:ip-interfaces
   #:md5
   #:split-sequence
   #:trivial-garbage
   #:usocket))
