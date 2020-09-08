(in-package #:feedback)

(define-page overview "feedback/^$" (:access (perm feedback))
  )

(define-page project "feedback/^([^/]+)/$" (:uri-groups (project) :access (perm feedback project))
  )

(define-page project-new "feedback/^new$" (:uri-groups (project) :access (perm feedback project new))
  )

(define-page project-edit "feedback/^([^/]+)/edit$" (:uri-groups (project) :access (perm feedback project edit))
  )

(define-page entry "feedback/^([^/]+)/entry/([^/]+)$" (:uri-groups (project entry) :access (perm feedback entry))
  )
