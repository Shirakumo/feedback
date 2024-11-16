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
       (attachment (apply #'project-url (dm:field object "project") args))
       (event (apply #'event-url object args))
       (deadline (apply #'deadline-url object args))))))

(defun object-label (object)
  (etypecase object
    (user:user (user:username object))
    (dm:data-model
     (ecase (dm:collection object)
       ((project attachment tag track timeline deadline)
        (dm:field object "name"))
       (entry
        (id-code object))
       (note
        (note-tag object))
       ((members subscriber)
        (user:username (dm:field object "user")))
       (entry-tag
        (object-label (ensure-tag object)))
       (dependency
        (format NIL "~a -> ~a"
                (id-code (dm:field object "source"))
                (id-code (dm:field object "target"))))
       (snapshot
        (id-code object))
       (event
        (object-label (ensure-entry object)))))))

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
  (apply #'uri-to-url (format NIL "feedback/snapshot/~a"
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
                 :icon "fa-dashboard"
                 :projects (list-projects)
                 :page-idx page
                 :user (auth:current)
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-entries (auth:current) :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* user ("feedback/^user/([^/]+)(?:/(\\d+)?)?$" 5) (:uri-groups (user page) :access (perm feedback user view))
  (let* ((user (user:get user :if-does-not-exist :error))
         (amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (render-page (user:username user) (@template "user-view.ctml")
                 :icon "fa-user"
                 :page-idx page
                 :user user
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-entries user :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* user-changes ("feedback/^user/([^/]+)/changes(?:/(\\d+)?)?$" 5) (:uri-groups (user page) :access (perm feedback user view))
  (let* ((user (user:get user :if-does-not-exist :error))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (render-page "Changes" (@template "user-changes.ctml")
                 :icon "fa-clock-rotate-left"
                 :up (user-url user)
                 :up-icon "fa-user"
                 :up-text (user:username user)
                 :page-idx page
                 :user user
                 :change-content (plump:parse (template-file "change.ctml" :feedback))
                 :changelog (list-changes :author user :amount amount :skip skip))))

(define-page* user-subscriptions ("feedback/^user/([^/]+)/subscribe$" 5) (:uri-groups (user) :access (perm feedback user view))
  (let ((object (user:get user :if-does-not-exist :error)))
    (render-page "Subscriptions" (@template "user-subscribe.ctml")
                 :icon "fa-envelope"
                 :up (object-url object)
                 :up-icon "fa-user"
                 :up-text (user:username object)
                 :object object
                 :user object
                 :subscriptions (list-subscriptions NIL object)
                 :have-entry T)))

(define-page* project ("feedback/^([^/]+)(?:/(\\d+)?)?$" 1) (:uri-groups (project page))
  (let* ((project (find-project project))
         (amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible project :view)
    (render-page (dm:field project "name") (@template "project-view.ctml")
                 :icon "fa-diagram-project"
                 :description (dm:field project "description")
                 :page-idx page
                 :project project
                 :tracks (list-tracks project)
                 :timelines (list-timelines project)
                 :entry-content (plump:parse (template-file "entry.ctml" :feedback))
                 :entries (list-untracked-entries project :skip skip :amount amount :query (or* (post/get "query"))))))

(define-page* project-new ("feedback/^new$" 2) (:access (perm feedback project new))
  (render-page "New project" (@template "project-edit.ctml")
               :icon "fa-diagram-project"
               :project (dm:hull 'project)))

(define-page* project-edit ("feedback/^([^/]+)/edit$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "Edit" (@template "project-edit.ctml")
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :project project
                 :members (list-members project)
                 :tags (list-tags project)
                 :attachments (list-attachments project))))

(define-page* project-import ("feedback/^([^/]+)/import$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "Import" (@template "project-import.ctml")
                 :icon "fa-file-import"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :project project)))

(define-page* project-changes ("feedback/^([^/]+)/changes(?:/(\\d+)?)?$" 3) (:uri-groups (project page))
  (let* ((project (find-project project))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible project :view)
    (render-page "Changes" (@template "project-changes.ctml")
                 :icon "fa-clock-rotate-left"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :page-idx page
                 :project project
                 :change-content (plump:parse (template-file "change.ctml" :feedback))
                 :changelog (list-changes :object project :amount amount :skip skip))))

(define-page* project-subscribe ("feedback/^([^/]+)/subscribe$" 5) (:uri-groups (project))
  (let* ((project (find-project project))
         (subscriptions (list-subscriptions project)))
    (check-accessible project :view)
    (render-page "Subscriptions" (@template "project-subscribe.ctml")
                 :icon "fa-envelope"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :object-type 'project :object-id (dm:id project)
                 :project project
                 :subscriptions subscriptions
                 :have-entry (find (user:id (auth:current)) subscriptions
                                   :key (lambda (sub) (dm:field sub "user")) :test #'equal))))

(define-page* project-snapshots ("feedback/^([^/]+)/snapshots(?:/(\\d+)?)?$" 2) (:uri-groups (project page))
  (let* ((project (find-project project))
         (amount 50)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible project :view)
    (render-page "Snapshots" (@template "snapshot-list.ctml")
                 :icon "fa-camera"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :page-idx page
                 :project project
                 :snapshots (list-snapshots project :user-id (or* (post/get "user"))
                                                    :session-id (or* (post/get "session"))
                                                    :skip skip
                                                    :amount amount))))

(define-page* snapshot-view ("feedback/^snapshot/([^/]+)$" 5) (:uri-groups (snapshot))
  (let* ((snapshot (ensure-snapshot snapshot))
         (project (ensure-project snapshot)))
    (check-accessible snapshot :view)
    (render-page (princ-to-string (dm:id snapshot)) (@template "snapshot-view.ctml")
                 :icon "fa-camera"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :project project
                 :snapshot snapshot)))

(define-page* timeline "feedback/^([^/]+)/tl/([^/]+)(?:/)?$" (:uri-groups (project timeline))
  (let* ((project (find-project project))
         (timeline (find-timeline timeline project)))
    (check-accessible timeline :view)
    (render-page (dm:field timeline "name") (@template "timeline-view.ctml")
                 :icon "fa-timeline"
                 :description (dm:field timeline "description")
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :project project
                 :timeline timeline
                 :range (timeline-range timeline)
                 :entries (list-entries project :amount NIL)
                 :events (list-events timeline)
                 :deadlines (list-deadlines timeline))))

(define-page* timeline-new ("feedback/^([^/]+)/tl/new$" 2) (:uri-groups (project))
  (let ((project (find-project project)))
    (check-accessible project :edit)
    (render-page "New timeline" (@template "timeline-edit.ctml")
                 :icon "fa-timeline"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :project project
                 :timeline (dm:hull 'timeline :project (dm:id project)))))

(define-page* timeline-edit ("feedback/^([^/]+)/tl/([^/]+)/edit$" 2) (:uri-groups (project timeline))
  (let* ((project (find-project project))
         (timeline (find-timeline timeline project)))
    (check-accessible timeline :edit)
    (render-page "Edit" (@template "timeline-edit.ctml")
                 :up (timeline-url timeline)
                 :up-icon "fa-timeline"
                 :up-text (dm:field timeline "name")
                 :project project
                 :timeline timeline)))

(define-page* timeline-changes ("feedback/^([^/]+)/tl/^([^/]+)/changes(?:/(\\d+)?)?$" 3) (:uri-groups (project timeline page))
  (let* ((project (find-project project))
         (timeline (find-timeline timeline project))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible timeline :view)
    (render-page "Changes" (@template "timeline-changes.ctml")
                 :icon "fa-clock-rotate-left"
                 :up (project-url project)
                 :up-icon "fa-timeline"
                 :up-text (dm:field timeline "name")
                 :page-idx page
                 :project project
                 :timeline timeline
                 :change-content (plump:parse (template-file "change.ctml" :feedback))
                 :changelog (list-changes :object timeline :amount amount :skip skip))))

(define-page* track "feedback/^([^/]+)/([^/]+)(?:/(\\d+)?)?$" (:uri-groups (project track page))
  (let* ((project (find-project project))
         (track (find-track track project))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible track :view)
    (render-page (dm:field track "name") (@template "track-view.ctml")
                 :description (dm:field track "description")
                 :icon "fa-layer-group"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
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
                 :icon "fa-layer-group"
                 :up (project-url project)
                 :up-icon "fa-diagram-project"
                 :up-text (dm:field project "name")
                 :project project
                 :track (dm:hull 'track :project (dm:id project)))))

(define-page* track-edit ("feedback/^([^/]+)/([^/]+)/edit$" 2) (:uri-groups (project track))
  (let* ((project (find-project project))
         (track (find-track track project)))
    (check-accessible project :edit)
    (render-page "Edit" (@template "track-edit.ctml")
                 :up (track-url track)
                 :up-icon "fa-layer-group"
                 :up-text (dm:field track "name")
                 :project project
                 :track track)))

(define-page* track-changes ("feedback/^([^/]+)/([^/]+)/changes(?:/(\\d+)?)?$" 2) (:uri-groups (project track page) :access (perm feedback user view))
  (let* ((project (find-project project))
         (track (find-track track project))
         (amount 100)
         (page (parse-integer (or* page "1")))
         (skip (* amount (max 0 (1- page)))))
    (check-accessible track :view)
    (render-page "Changes" (@template "track-changes.ctml")
                 :icon "fa-clock-rotate-left"
                 :up (track-url track)
                 :up-icon "fa-layer-group"
                 :up-text (dm:field track "name")
                 :page-idx page
                 :project project
                 :track track
                 :change-content (plump:parse (template-file "change.ctml" :feedback))
                 :changelog (list-changes :object track :amount amount :skip skip))))

(define-page* track-import ("feedback/^([^/]+)/([^/]+)/import$" 2) (:uri-groups (project track))
  (let* ((project (find-project project))
         (track (find-track track project)))
    (check-accessible track :edit)
    (render-page "Import" (@template "track-import.ctml")
                 :icon "fa-file-import"
                 :up (track-url track)
                 :up-icon "fa-layer-group"
                 :up-text (dm:field track "name")
                 :project project
                 :track track)))

(define-page* track-subscribe ("feedback/^([^/]+)/([^/]+)/subscribe$" 2) (:uri-groups (project track))
  (let* ((project (find-project project))
         (track (find-track track project))
         (subscriptions (list-subscriptions track)))
    (check-accessible track :edit)
    (render-page "Subscriptions" (@template "track-subscribe.ctml")
                 :icon "fa-envelope"
                 :up (track-url track)
                 :up-icon "fa-layer-group"
                 :up-text (dm:field track "name")
                 :object-type 'track :object-id (dm:id track)
                 :project project
                 :track track
                 :subscriptions subscriptions
                 :have-entry (find (user:id (auth:current)) subscriptions
                                   :key (lambda (sub) (dm:field sub "user")) :test #'equal))))

(define-page* entry "feedback/^([^/]+)/entry/([^/]+)$" (:uri-groups (project entry))
  (let* ((project (find-project project))
         (entry (ensure-entry entry))
         (attachments (list-attachments entry)))
    (check-accessible entry :view)
    (render-page (id-code entry) (@template "entry-view.ctml")
                 :icon "fa-list-check"
                 :description (dm:field entry "description")
                 :up (if (dm:field entry "track")
                         (track-url (dm:field entry "track"))
                         (project-url project))
                 :up-icon "fa-layer-group"
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
                 :attachments attachments
                 :change-content (plump:parse (template-file "change.ctml" :feedback))
                 :changelog (list-changes :object entry))))

(define-page* entry-subscribe "feedback/^([^/]+)/entry/([^/]+)/subscribe$" (:uri-groups (project entry))
  (let* ((project (find-project project))
         (entry (ensure-entry entry))
         (subscriptions (list-subscriptions entry)))
    (check-accessible entry :edit)
    (render-page "Subscriptions" (@template "entry-subscribe.ctml")
                 :icon "fa-envelope"
                 :up (entry-url entry)
                 :up-icon "fa-list-check"
                 :up-text (id-code (dm:id entry))
                 :object-type 'entry :object-id (dm:id entry)
                 :project project
                 :entry entry
                 :subscriptions subscriptions
                 :have-entry (find (user:id (auth:current)) subscriptions
                                   :key (lambda (sub) (dm:field sub "user")) :test #'equal))))

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
