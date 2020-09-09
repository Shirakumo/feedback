(asdf:defsystem #:feedback
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :serial T
  :version "0.0.0"
  :components ((:file "module")
               (:file "toolkit")
               (:file "db")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :auth)
               (:interface :mail)
               :r-data-model
               :r-oauth
               :r-clip
               :i-json))
