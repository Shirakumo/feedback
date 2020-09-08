(in-package #:modularize-user)
(define-module #:kandria-feedback
  (:use #:cl #:radiance)
  (:export)
  (:local-nicknames))
(in-package #:kandria-feedback)

(define-trigger startup ()
  (defaulted-config (make-random-string 32) :private-key)
  (defaulted-config (make-random-string 32) :salt))
