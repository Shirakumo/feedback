(in-package #:feedback)

(defun project-url (project)
  (uri-to-url (format NIL "feedback/~a/"
                      (dm:field (ensure-project project) "name"))
              :representation :external))

(defun track-url (track)
  (let ((track (ensure-track track)))
    (uri-to-url (format NIL "feedback/~a/~a/"
                        (dm:field (ensure-project track) "name")
                        (dm:field track "name"))
                :representation :external)))

(defun entry-url (entry)
  (let ((entry (ensure-entry entry)))
    (uri-to-url (format NIL "feedback/~a/entry/~a"
                        (dm:field (ensure-project entry) "name")
                        (id-code entry))
                :representation :external)))

(defun track-entry-url (entry)
  (let ((entry (ensure-entry entry)))
    (uri-to-url (format NIL "feedback/~a~@[/~a~]"
                        (dm:field (ensure-project entry) "name")
                        (when (dm:field entry "track") (dm:field (ensure-track entry) "name")))
                :representation :external
                :fragment (id-code (dm:id entry)))))

(defun track-note-url (note)
  (let* ((note (ensure-note note))
         (entry (ensure-entry note)))
    (uri-to-url (if (dm:field entry "track")
                    (format NIL "feedback/~a/~a"
                            (dm:field (ensure-project entry) "name")
                            (dm:field (ensure-track entry) "name"))
                    (format NIL "feedback/~a/"
                            (dm:field (ensure-project entry) "name")))
                :representation :external
                :fragment (note-tag note))))

(defun snapshot-url (snapshot)
  (uri-to-url (format NIL "feedback/~a/snapshot/~a"
                      (dm:field (ensure-project snapshot) "name")
                      (ensure-id snapshot))
              :representation :external))

(defun note-url (note)
  (let* ((note (ensure-note note))
         (entry (ensure-entry note)))
    (uri-to-url (format NIL "feedback/~a/entry/~a"
                        (dm:field (ensure-project entry) "name")
                        (id-code (dm:id entry)))
                :representation :external
                :fragment (note-tag note))))

(defun note-tag (note)
  (format NIL "~a-~a" (id-code (dm:field note "entry")) (dm:id note)))

(defun trace-url (snapshot)
  (uri-to-url (format NIL "feedback/~a/snapshot/~a/trace"
                      (dm:field (ensure-project snapshot) "name")
                      (ensure-id snapshot))
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
               :page-idx 1
               :entries (list-entries (auth:current))))

(define-page project ("feedback/^([^/]+)(?:/(\\d+)?)?$" 1) (:uri-groups (project page) :access (perm feedback project))
  (let* ((project (find-project project))
         (amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (render-page (dm:field project "name") (@template "project-view.ctml")
                 :page-idx page
                 :project project
                 :tracks (list-tracks project)
                 :entries (list-entries project :skip skip :amount amount))))

(define-page project-new ("feedback/^new$" 2) (:access (perm feedback project new))
  (render-page "New project" (@template "project-edit.ctml")
               :project (dm:hull 'project)))

(define-page project-edit ("feedback/^([^/]+)/edit$" 2) (:uri-groups (project) :access (perm feedback project edit))
  (let ((project (find-project project)))
    (render-page "Edit" (@template "project-edit.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :members (list-members project)
                 :attachments (list-attachments project))))

(define-page track "feedback/^([^/]+)/([^/]+)(?:/(\\d+)?)?$" (:uri-groups (project track page) :access (perm feedback track))
  (let* ((project (find-project project))
         (track (find-track track project))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (render-page (dm:field track "name") (@template "track-view.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :page-idx page
                 :project project
                 :track track
                 :entries (list-entries track :skip skip :amount amount))))

(define-page track-new ("feedback/^([^/]+)/new$" 2) (:uri-groups (project) :access (perm feedback project edit))
  (let ((project (find-project project)))
    (render-page "New track" (@template "track-edit.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :track (dm:hull 'track :project (dm:id project)))))

(define-page track-edit ("feedback/^([^/]+)/([^/]+)/edit$" 2) (:uri-groups (project track) :access (perm feedback project edit))
  (let* ((project (find-project project))
         (track (find-track track project)))
    (render-page "Edit" (@template "track-edit.ctml")
                 :up (track-url track)
                 :up-text (dm:field track "name")
                 :project project
                 :track track)))

(define-page entry "feedback/^([^/]+)/entry/([^/]+)$" (:uri-groups (project entry) :access (perm feedback entry))
  (let ((project (find-project project))
        (entry (ensure-entry entry)))
    (render-page (id-code entry) (@template "entry-view.ctml")
                 :up (if (dm:field entry "track")
                         (track-url (dm:field entry "track"))
                         (project-url project))
                 :up-text (if (dm:field entry "track")
                              (dm:field (ensure-track entry) "name")
                              (dm:field project "name"))
                 :project project
                 :entry entry
                 :notes (list-notes entry)
                 :attachments (list-attachments entry))))

(define-page attachment "feedback/^([^/]+)/entry/([^/]+)/([^/]+)$" (:uri-groups (project entry attachment) :access (perm feedback entry))
  (let* ((type (ensure-attachment (find-project project) attachment))
         (entry (ensure-entry entry))
         (path (attachment-pathname entry type))
         (filename (format NIL "~a ~a.~(~a~)" (dm:id entry) (dm:field type "name") (id->attachment-type (dm:field type "type"))))
         (content-type (attachment-type-content-type (dm:field type "type"))))
    (setf (header "Content-Disposition") (format NIL "~:[attachment~;inline~]; filename=~s" (string= "text/plain" content-type) filename))
    (setf (header "Cache-Control") "private, max-age=31536000")
    (setf (header "Access-Control-Allow-Origin") "*")
    (serve-file path content-type)))

(define-page snapshots ("feedback/^([^/]+)/snapshot/$" 2) (:uri-groups (project) :access (perm feedback snapshot))
  (let* ((project (find-project project))
         (amount 50)
         (skip (* amount (max 0 (1- (parse-integer (or* (post/get "page") "1")))))))
    (render-page "Snapshots" (@template "snapshot-list.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :snapshots (list-snapshots project :user-id (or* (post/get "user"))
                                                    :session-id (or* (post/get "session"))
                                                    :skip skip
                                                    :amount amount))))

(define-page snapshot ("feedback/^([^/]+)/snapshot/([^/]+)$" 2) (:uri-groups (project snapshot) :access (perm feedback snapshot))
  (let ((project (find-project project))
        (snapshot (ensure-snapshot snapshot)))
    (render-page (princ-to-string (dm:id snapshot)) (@template "snapshot-view.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :snapshot snapshot)))

(define-page trace-file "feedback/^([^/]+)/snapshot/([^/]+)/trace$" (:uri-groups (project snapshot) :access (perm feedback snapshot))
  (declare (ignore project))
  (let* ((snapshot (ensure-snapshot snapshot))
         (path (make-pathname :name "trace" :type "dat" :defaults (snapshot-directory snapshot)))
         (filename (format NIL "~a.dat" (dm:id snapshot))))
    (setf (header "Content-Disposition") (format NIL "inline; filename=~s" filename))
    (setf (header "Cache-Control") "public, max-age=31536000")
    (setf (header "Access-Control-Allow-Origin") "*")
    (serve-file path "application/octet-stream")))
