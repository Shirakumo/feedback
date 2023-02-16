(asdf:defsystem #:feedback-client
  :serial T
  :components ((:file "package")
               (:file "system-info")
               (:file "client"))
  :depends-on (:drakma
               :com.inuoe.jzon
               :trivial-features
               :north-drakma))
