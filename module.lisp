(in-package #:modularize-user)
(define-module #:feedback
  (:use #:cl #:radiance)
  (:export)
  (:local-nicknames))
(in-package #:feedback)

(define-trigger startup ()
  (defaulted-config (make-random-string 32) :private-key)
  (defaulted-config (make-random-string 32) :salt))
