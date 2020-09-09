(in-package #:feedback)

(defun render-page (page content &rest args)
  (r-clip:with-clip-processing ("template.ctml")
    (apply #'r-clip:process T
           :page page
           :version (asdf:component-version (asdf:find-system :courier))
           args)))

(define-page dashboard "feedback/^$" (:access (perm feedback))
  (render-page "Dashboard" (@template "dashboard.ctml")
               :projects (list-projects)
               :entries (list-entries)))

(define-page project "feedback/^([^/]+)/$" (:uri-groups (project) :access (perm feedback project))
  (let ((project (find-project project)))
    (render-page (dm:field project "name") (@template "entry-list.ctml")
                 :project project
                 :entries (list-entries project))))

(define-page project-new "feedback/^new$" (:uri-groups (project) :access (perm feedback project new))
  (render-page "New project" (@template "project-edit.ctml")
               :project (dm:hull 'project)))

(define-page project-edit "feedback/^([^/]+)/edit$" (:uri-groups (project) :access (perm feedback project edit))
  (let ((project (find-project project)))
    (render-page (dm:field project "name") (@template "project-edit.ctml")
                 :project project)))

(define-page entry "feedback/^([^/]+)/entry/([^/]+)$" (:uri-groups (project entry) :access (perm feedback entry))
  (let ((project (find-project project))
        (entry (ensure-entry entry)))
    (render-page (princ-to-string (dm:id entry)) (@template "entry-view.ctml")
                 :up (uri-to-url (format NIL "feedback/~a" (dm:field project "name")) :representation :external)
                 :up-text (dm:field project "name")
                 :project project
                 :entry entry)))
