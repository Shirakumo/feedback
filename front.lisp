(in-package #:feedback)

(defun project-url (project)
  (uri-to-url (format NIL "feedback/~a/"
                      (dm:field (ensure-project project) "name"))
              :representation :external))

(defun entry-url (entry)
  (uri-to-url (format NIL "feedback/~a/entry/~a"
                      (dm:field (ensure-project entry) "name")
                      (ensure-id entry))
              :representation :external))

(defun attachment-url (entry type)
  (uri-to-url (format NIL "feedback/~a/entry/~a/~a"
                      (dm:field (ensure-project entry) "name")
                      (ensure-id entry)
                      (dm:field type "name"))
              :representation :external))

(defun render-page (page content &rest args)
  (r-clip:with-clip-processing ("template.ctml")
    (apply #'r-clip:process T
           :page page
           :version "0.0.0"
           :content (plump:parse content)
           args)))

(define-page dashboard "feedback/^$" (:access (perm feedback))
  (render-page "Dashboard" (@template "dashboard.ctml")
               :projects (list-projects)
               :entries (list-entries)))

(define-page project "feedback/^([^/]+)(?:/(\\d+)?)?$" (:uri-groups (project page) :access (perm feedback project))
  (let* ((project (find-project project))
         (amount 50)
         (skip (* amount (max 0 (1- (parse-integer (or* page "1")))))))
    (render-page (dm:field project "name") (@template "project-view.ctml")
                 :project project
                 :entries (list-entries project :skip skip :amount amount))))

(define-page project-new ("feedback/^new$" 1) (:access (perm feedback project new))
  (render-page "New project" (@template "project-edit.ctml")
               :project (dm:hull 'project)))

(define-page project-edit "feedback/^([^/]+)/edit$" (:uri-groups (project) :access (perm feedback project edit))
  (let ((project (find-project project)))
    (render-page (dm:field project "name") (@template "project-edit.ctml")
                 :project project
                 :attachments (list-attachments project))))

(define-page entry "feedback/^([^/]+)/entry/([^/]+)$" (:uri-groups (project entry) :access (perm feedback entry))
  (let ((project (find-project project))
        (entry (ensure-entry entry)))
    (render-page (princ-to-string (dm:id entry)) (@template "entry-view.ctml")
                 :up (uri-to-url (format NIL "feedback/~a" (dm:field project "name")) :representation :external)
                 :up-text (dm:field project "name")
                 :project project
                 :entry entry
                 :attachments (list-attachments entry))))

(define-page entry-edit ("feedback/^([^/]+)/entry/([^/]+)/edit$" 1) (:uri-groups (project entry) :access (perm feedback entry edit))
  (let ((project (or (find-project project)
                     (ensure-project project)))
        (entry (ensure-entry entry)))
    (render-page (princ-to-string (dm:id entry)) (@template "entry-edit.ctml")
                 :up (uri-to-url (format NIL "feedback/~a" (dm:field project "name")) :representation :external)
                 :up-text (dm:field project "name")
                 :entry entry)))

(define-page attachment "feedback/^([^/]+)/entry/([^/]+)/([^/]+)$" (:uri-groups (project entry attachment) :access (perm feedback entry))
  (let* ((type (ensure-attachment (find-project project) attachment))
         (entry (ensure-entry entry))
         (path (attachment-pathname entry type))
         (filename (format NIL "~a ~a.~(~a~)" (dm:id entry) (dm:field type "name") (id->attachment-type (dm:field type "type")))))
    (setf (header "Content-Disposition") (format NIL "inline; filename=~s" filename))
    (serve-file path (attachment-type-content-type (dm:field type "type")))))
