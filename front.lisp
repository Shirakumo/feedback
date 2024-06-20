(in-package #:feedback)

(defmacro define-page* (name uri options &body body)
  `(define-page ,name ,uri ,options
     (let ((*author* (auth:current "anonymous"))
           (*ensure-cache* (make-hash-table :test 'equal)))
       ,@body)))

(defun object-url (object &rest args)
  (etypecase object
    (null (apply #'uri-to-url "feedback/" :representation :external args))
    (user:user (apply #'user-url object args))
    (dm:data-model
     (ecase (dm:collection object)
       (project (apply #'project-url object args))
       (track (apply #'track-url object args))
       (trace (apply #'trace-url object args))
       (entry (apply #'entry-url object args))
       (note (apply #'note-url object args))
       (snapshot (apply #'snapshot-url object args))
       (timeline (apply #'timeline-url object args))
       (tag (apply #'project-url (dm:field object "project") args))
       (attachment (apply #'entry-url (dm:field object "entry") args))
       (event (apply #'event-url object args))
       (deadline (apply #'deadline-url object args))))))

(defun user-url (user &rest args)
  (apply #'uri-to-url (format NIL "feedback/user/~a/" (user:username user))
         :representation :external args))

(defun project-url (project &rest args)
  (apply #'uri-to-url (format NIL "feedback/~a/"
                              (dm:field (ensure-project project) "name"))
         :representation :external args))

(defun track-url (track &rest args)
  (let ((track (ensure-track track)))
    (apply #'uri-to-url (format NIL "feedback/~a/~a/"
                                (dm:field (ensure-project track) "name")
                                (dm:field track "name"))
           :representation :external args)))

(defun entry-url (entry &rest args)
  (let ((entry (ensure-entry entry)))
    (apply #'uri-to-url (format NIL "feedback/~a/entry/~a"
                                (dm:field (ensure-project entry) "name")
                                (id-code entry))
           :representation :external args)))

(defun track-entry-url (entry &rest args)
  (let ((entry (ensure-entry entry)))
    (apply #'uri-to-url (format NIL "feedback/~a~@[/~a~]"
                                (dm:field (ensure-project entry) "name")
                                (when (dm:field entry "track") (dm:field (ensure-track entry) "name")))
           :representation :external
           :fragment (id-code (dm:id entry))
           args)))

(defun track-note-url (note &rest args)
  (let* ((note (ensure-note note))
         (entry (ensure-entry note)))
    (apply #'uri-to-url (if (dm:field entry "track")
                            (format NIL "feedback/~a/~a"
                                    (dm:field (ensure-project entry) "name")
                                    (dm:field (ensure-track entry) "name"))
                            (format NIL "feedback/~a/"
                                    (dm:field (ensure-project entry) "name")))
           :representation :external
           :fragment (note-tag note)
           args)))

(defun snapshot-url (snapshot &rest args)
  (apply #'uri-to-url (format NIL "feedback/~a/snapshot/~a"
                              (dm:field (ensure-project snapshot) "name")
                              (ensure-id snapshot))
         :representation :external
         args))

(defun note-url (note &rest args)
  (let* ((note (ensure-note note))
         (entry (ensure-entry note)))
    (apply #'uri-to-url (format NIL "feedback/~a/entry/~a"
                                (dm:field (ensure-project entry) "name")
                                (id-code (dm:id entry)))
           :representation :external
           :fragment (note-tag note)
           args)))

(defun timeline-url (timeline &rest args)
  (let ((timeline (ensure-timeline timeline)))
    (apply #'uri-to-url (format NIL "feedback/~a/tl/~a/"
                                (dm:field (ensure-project timeline) "name")
                                (dm:field timeline "name"))
           :representation :external args)))

(defun event-url (event &rest args)
  (let* ((event (ensure-event event))
         (timeline (ensure-timeline event)))
    (apply #'uri-to-url (format NIL "feedback/~a/tl/~a/"
                                (dm:field (ensure-project timeline) "name")
                                (dm:field timeline "name"))
           :representation :external
           :fragment (format NIL "ev-~a" (id-code event))
           args)))

(defun deadline-url (deadline &rest args)
  (let* ((deadline (ensure-deadline deadline))
         (timeline (ensure-timeline deadline)))
    (apply #'uri-to-url (format NIL "feedback/~a/tl/~a/"
                                (dm:field (ensure-project timeline) "name")
                                (dm:field timeline "name"))
           :representation :external
           :fragment (format NIL "dl-~a" (id-code deadline))
           args)))

(defun note-tag (note)
  (format NIL "~a-~a" (id-code (dm:field note "entry")) (dm:id note)))

(defun trace-url (snapshot &rest args)
  (apply #'uri-to-url (format NIL "feedback/~a/snapshot/~a/trace"
                              (dm:field (ensure-project snapshot) "name")
                              (ensure-id snapshot))
         :representation :external
         args))

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

(define-page* dashboard "feedback/^(?:/(\\d+)?)?$" (:uri-groups (page) :access (perm feedback dashboard view))
  (let* ((amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (render-page "Dashboard" (@template "dashboard.ctml")
                 :projects (list-projects)
                 :page-idx page
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-entries (auth:current) :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* user ("feedback/^user/([^/]+)(?:/(\\d+)?)?$" 1) (:uri-groups (user page) :access (perm feedback user view))
  (let* ((user (user:get user :if-does-not-exist :error))
         (amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (render-page (user:username user) (@template "user-view.ctml")
                 :up (object-url NIL)
                 :up-text "Dashboard"
                 :page-idx page
                 :user user
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-entries user :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* project ("feedback/^([^/]+)(?:/(\\d+)?)?$" 1) (:uri-groups (project page))
  (let* ((project (find-project project))
         (amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible project :view)
    (render-page (dm:field project "name") (@template "project-view.ctml")
                 :description (dm:field project "description")
                 :page-idx page
                 :project project
                 :tracks (list-tracks project)
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-entries project :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* project-new ("feedback/^new$" 2) (:access (perm feedback project new))
  (render-page "New project" (@template "project-edit.ctml")
               :project (dm:hull 'project)))

(define-page* project-edit ("feedback/^([^/]+)/edit$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "Edit" (@template "project-edit.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :members (list-members project)
                 :tags (list-tags project)
                 :attachments (list-attachments project))))

(define-page* project-import ("feedback/^([^/]+)/import$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "Import" (@template "project-import.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project)))

(define-page* timeline "feedback/^([^/]+)/tl/([^/]+)(?:/)?$" (:uri-groups (project timeline))
  (let* ((project (find-project project))
         (timeline (find-timeline timeline project)))
    (check-accessible timeline :view)
    (render-page (dm:field timeline "name") (@template "timeline-view.ctml")
                 :description (dm:field timeline "description")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :timeline timeline
                 :entries (list-events timeline))))

(define-page* timeline-new ("feedback/^([^/]+)/tl/new$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "New timeline" (@template "timeline-edit.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :timeline (dm:hull 'timeline :project (dm:id project)))))

(define-page* timeline-edit ("feedback/^([^/]+)/tl/([^/]+)/edit$" 2) (:uri-groups (project timeline))
  (let* ((project (find-project project))
         (timeline (find-timeline timeline project)))
    (check-accessible project :edit)
    (render-page "Edit" (@template "timeline-edit.ctml")
                 :up (timeline-url timeline)
                 :up-text (dm:field timeline "name")
                 :project project
                 :timeline timeline)))

(define-page* track "feedback/^([^/]+)/([^/]+)(?:/(\\d+)?)?$" (:uri-groups (project track page))
  (let* ((project (find-project project))
         (track (find-track track project))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible track :view)
    (render-page (dm:field track "name") (@template "track-view.ctml")
                 :description (dm:field track "description")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :page-idx page
                 :project project
                 :track track
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-entries track :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* track-new ("feedback/^([^/]+)/new$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "New track" (@template "track-edit.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :track (dm:hull 'track :project (dm:id project)))))

(define-page* track-edit ("feedback/^([^/]+)/([^/]+)/edit$" 2) (:uri-groups (project track))
  (let* ((project (find-project project))
         (track (find-track track project)))
    (check-accessible project :edit)
    (render-page "Edit" (@template "track-edit.ctml")
                 :up (track-url track)
                 :up-text (dm:field track "name")
                 :project project
                 :track track)))

(define-page* entry "feedback/^([^/]+)/entry/([^/]+)$" (:uri-groups (project entry))
  (let* ((project (find-project project))
         (entry (ensure-entry entry))
         (attachments (list-attachments entry)))
    (check-accessible entry :view)
    (render-page (id-code entry) (@template "entry-view.ctml")
                 :description (dm:field entry "description")
                 :up (if (dm:field entry "track")
                         (track-url (dm:field entry "track"))
                         (project-url project))
                 :up-text (if (dm:field entry "track")
                              (dm:field (ensure-track entry) "name")
                              (dm:field project "name"))
                 :image-url (loop for attachment in attachments
                                  for path = (attachment-pathname entry attachment)
                                  do (when (and (attachment-image-p (dm:field attachment "type"))
                                                (probe-file path))
                                       (return (attachment-url entry attachment))))
                 :project project
                 :entry entry
                 :notes (list-notes entry)
                 :attachments attachments)))

(define-page* attachment "feedback/^([^/]+)/entry/([^/]+)/([^/]+)$" (:uri-groups (project entry attachment))
  (let* ((project (find-project project))
         (type (ensure-attachment project attachment))
         (entry (ensure-entry entry))
         (path (attachment-pathname entry type))
         (filename (format NIL "~a ~a.~(~a~)" (dm:id entry) (dm:field type "name") (id->attachment-type (dm:field type "type"))))
         (content-type (attachment-type-content-type (dm:field type "type"))))
    (check-accessible project :view)
    (setf (header "Content-Disposition") (format NIL "~:[attachment~;inline~]; filename=~s" (string= "text/plain" content-type) filename))
    (setf (header "Cache-Control") "private, max-age=31536000")
    (setf (header "Access-Control-Allow-Origin") "*")
    (serve-file path content-type)))

(define-page* snapshots ("feedback/^([^/]+)/snapshot/$" 2) (:uri-groups (project))
  (let* ((project (find-project project))
         (amount 50)
         (skip (* amount (max 0 (1- (parse-integer (or* (post/get "page") "1")))))))
    (check-accessible project :view)
    (render-page "Snapshots" (@template "snapshot-list.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :snapshots (list-snapshots project :user-id (or* (post/get "user"))
                                                    :session-id (or* (post/get "session"))
                                                    :skip skip
                                                    :amount amount))))

(define-page* snapshot ("feedback/^([^/]+)/snapshot/([^/]+)$" 2) (:uri-groups (project snapshot))
  (let ((project (find-project project))
        (snapshot (ensure-snapshot snapshot)))
    (check-accessible snapshot :view)
    (render-page (princ-to-string (dm:id snapshot)) (@template "snapshot-view.ctml")
                 :up (project-url project)
                 :up-text (dm:field project "name")
                 :project project
                 :snapshot snapshot)))

(define-page* subscriptions ("feedback/^subscribe/([^/]+)/([^/]+)$" 2) (:uri-groups (type id))
  (if (string-equal type "user")
      (let ((object (user:get id :if-does-not-exist :error)))
        (render-page "Subscriptions" (@template "subscribe.ctml")
                     :up (object-url object)
                     :up-text (user:username object)
                     :object object
                     :subscriptions (list-subscriptions NIL object)
                     :have-entry T))
      (let* ((object (ensure-object type id))
             (subscriptions (list-subscriptions object)))
        (check-accessible (ensure-project object) :view)
        (render-page "Subscriptions" (@template "subscribe.ctml")
                     :up (object-url object)
                     :up-text (or (dm:field object "name")
                                  (dm:field object "title")
                                  (princ-to-string (dm:id object)))
                     :object-type type :object-id id :object object
                     :subscriptions subscriptions
                     :have-entry (find (user:id (auth:current)) subscriptions
                                       :key (lambda (sub) (dm:field sub "user")) :test #'equal)))))

(define-page* trace-file "feedback/^([^/]+)/snapshot/([^/]+)/trace$" (:uri-groups (project snapshot))
  (declare (ignore project))
  (let* ((snapshot (ensure-snapshot snapshot))
         (path (make-pathname :name "trace" :type "dat" :defaults (snapshot-directory snapshot)))
         (filename (format NIL "~a.dat" (dm:id snapshot))))
    (check-accessible snapshot :view)
    (setf (header "Content-Disposition") (format NIL "inline; filename=~s" filename))
    (setf (header "Cache-Control") "public, max-age=31536000")
    (setf (header "Access-Control-Allow-Origin") "*")
    (serve-file path "application/octet-stream")))
