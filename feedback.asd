(asdf:defsystem #:feedback
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "db")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :relational-database)
               (:interface :auth)
               :r-data-model
               :r-oauth
               :r-clip
               :i-json))
