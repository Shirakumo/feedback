(asdf:defsystem #:feedback-client
  :serial T
  :components ((:file "package")
               (:file "system-info")
               (:file "client"))
  :depends-on (:drakma
               :yason
               :trivial-features
               :north-drakma))
