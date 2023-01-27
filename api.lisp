(in-package #:feedback)

(defun output (object message url-format &rest args)
  (let ((target (uri-to-url (apply #'format NIL url-format args)
                            :representation :external :query `(("message" . ,message)))))
    (if (string= "true" (post/get "browser"))
        (redirect target)
        (api-output object :message message :target target))))

(define-api feedback/project/list () (:access (perm feedback project list))
  (api-output (list-projects)))

(define-api feedback/project/new (name &optional trace-data-type description attachment-name[] attachment-type[]) (:access (perm feedback project new))
  (output (make-project name :description description
                             :trace-data-type trace-data-type
                             :attachments (loop for name in attachment-name[]
                                                for type in attachment-type[]
                                                collect (list name type)))
          "Project created." "feedback/~a/" name))

(define-api feedback/project/edit (project &optional name trace-data-type description attachment-name[] attachment-type[] member[]) (:access (perm feedback project edit))
  (output (edit-project project :name name
                                :description description
                                :trace-data-type trace-data-type
                                :attachments (loop for name in attachment-name[]
                                                   for type in attachment-type[]
                                                   collect (list name type))
                                :members member[])
          "Project edited." "feedback/~a/" name))

(define-api feedback/project/delete (project) (:access (perm feedback project delete))
  (delete-project (ensure-project project))
  (output NIL "Project deleted." "feedback/"))

(define-api feedback/project/subscribe (project type[]) (:access (perm feedback project subscribe))
  (let ((project (ensure-project project)))
    (subscribe-to project type[])
    (output NIL "Subscription updated." "feedback/~a/" (dm:field project "name"))))

(define-api feedback/project/usubscribe (project type[]) (:access (perm feedback project subscribe))
  (let ((project (ensure-project project)))
    (unsubscribe-from project type[])
    (output NIL "Subscription updated." "feedback/~a/" (dm:field project "name"))))

(define-api feedback/project/set-subscription (project type[]) (:access (perm feedback project subscribe))
  (let ((project (ensure-project project)))
    (set-subscription project type[])
    (output NIL "Subscription updated." "feedback/~a/" (dm:field project "name"))))

(define-api feedback/track/new (project name &optional description) (:access (perm feedback track new))
  (let ((project (ensure-project project)))
    (output (make-track project name :description description)
            "Track created." "feedback/~a/~a" (dm:field project "name") name)))

(define-api feedback/track/edit (track &optional name description) (:access (perm feedback track new))
  (let ((track (ensure-track track)))
    (output (edit-track track :name name
                              :description description)
            "Track edited." "feedback/~a/~a" (dm:field track "project") (dm:field track "name"))))

(define-api feedback/track/delete (track) (:access (perm feedback track delete))
  (delete-track (ensure-track track))
  (output NIL "Track deleted." "feedback/"))

(define-api feedback/track/subscribe (track type[]) (:access (perm feedback track subscribe))
  (let ((track (ensure-track track)))
    (subscribe-to track type[])
    (output NIL "Subscription updated." "feedback/~a/~a" (dm:field track "project") (dm:field track "name"))))

(define-api feedback/track/usubscribe (track type[]) (:access (perm feedback track subscribe))
  (let ((track (ensure-track track)))
    (unsubscribe-from track type[])
    (output NIL "Subscription updated." "feedback/~a/~a" (dm:field track "project") (dm:field track "name"))))

(define-api feedback/track/set-subscription (track type[]) (:access (perm feedback track subscribe))
  (let ((track (ensure-track track)))
    (set-subscription track type[])
    (output NIL "Subscription updated." "feedback/~a/~a" (dm:field track "project") (dm:field track "name"))))

(define-api feedback/entry/list (&optional project track) (:access (perm feedback entry list))
  (api-output (list-entries (cond (project (ensure-project project))
                                  (track (ensure-track track))
                                  (T (error 'api-argument-missing :argument "project"))))))

(define-api feedback/entry/new (project user-id &optional track status version os-type cpu-type gpu-type os-info cpu-info gpu-info description assigned-to severity relates-to notify) (:access (perm feedback entry new))
  (db:with-transaction ()
    (let* ((project (or (find-project project)
                        (ensure-project project)))
           (types (list-attachments project))
           (entry (make-entry project :track track
                                      :status status
                                      :version version
                                      :user-id user-id
                                      :os-type os-type
                                      :os-info os-info
                                      :cpu-type cpu-type
                                      :cpu-info cpu-info
                                      :gpu-type gpu-type
                                      :gpu-info gpu-info
                                      :description description
                                      :assigned-to assigned-to
                                      :relates-to relates-to
                                      :severity severity
                                      :notify notify)))
      (loop for type in types
            for file = (post-var (dm:field type "name"))
            do (when file
                 (ensure-directories-exist (attachment-pathname entry type))
                 (uiop:copy-file (first file) (attachment-pathname entry type))))
      (if (string= "true" (post/get "browser"))
          (redirect (track-entry-url entry))
          (api-output entry :message "Feedback submitted." :target (entry-url entry))))))

(define-api feedback/entry/edit (entry &optional comment description status assigned-to severity relates-to order) (:access (perm feedback entry edit))
  (db:with-transaction ()
    (let* ((entry (ensure-entry entry)))
      (edit-entry entry :description description :comment comment :status status :order (cond ((string= "top" order) :top)
                                                                                              ((string= "bottom" order) :bottom)
                                                                                              ((stringp order) (parse-integer order)))
                        :assigned-to assigned-to :severity severity :relates-to relates-to)
      (if (string= "true" (post/get "browser"))
          (redirect (track-entry-url entry))
          (api-output entry :message "Entry updated." :target (entry-url entry))))))

(define-api feedback/entry/delete (entry) (:access (perm feedback entry delete))
  (let ((entry (ensure-entry entry)))
    (delete-entry entry)
    (output NIL "Entry deleted." "feedback/~a/" (dm:field (ensure-project entry) "name"))))

(define-api feedback/entry/subscribe (entry type[]) (:access (perm feedback entry subscribe))
  (let ((entry (ensure-entry entry))
        (project (ensure-project entry)))
    (subscribe-to entry type[])
    (output NIL "Subscription updated." "feedback/~a/entry/~a" (dm:field project "name") (dm:id entry))))

(define-api feedback/entry/usubscribe (entry type[]) (:access (perm feedback entry subscribe))
  (let ((entry (ensure-entry entry))
        (project (ensure-project entry)))
    (unsubscribe-from entry type[])
    (output NIL "Subscription updated." "feedback/~a/entry/~a" (dm:field project "name") (dm:id entry))))

(define-api feedback/entry/set-subscription (entry type[]) (:access (perm feedback entry subscribe))
  (let ((entry (ensure-entry entry))
        (project (ensure-project entry)))
    (set-subscription entry type[])
    (output NIL "Subscription updated." "feedback/~a/entry/~a" (dm:field project "name") (dm:id entry))))

(define-api feedback/note/new (entry text) (:access (perm feedback note new))
  (db:with-transaction ()
    (let* ((entry (ensure-entry entry))
           (note (make-note entry text)))
      (if (string= "true" (post/get "browser"))
          (redirect (note-url note))
          (api-output note :message "Note created." :target (note-url note))))))

(define-api feedback/note/edit (note text) (:access (perm feedback note edit))
  (db:with-transaction ()
    (let ((note (edit-note note :text text)))
      (if (string= "true" (post/get "browser"))
          (redirect (note-url note))
          (api-output note :message "Note updated." :target (note-url note))))))

(define-api feedback/note/delete (note) (:access (perm feedback note delete))
  (let ((entry (ensure-note note)))
    (delete-note note)
    (output entry "Note edited." "feedback/~a/entry/~a" (dm:field (ensure-project note) "name") (dm:id entry))))

(define-api feedback/snapshot/new (project user-id session-id session-duration snapshot-duration &optional version trace) (:access (perm feedback entry new))
  (db:with-transaction ()
    (let* ((project (or (find-project project)
                        (ensure-project project)))
           (snapshot (make-snapshot project :version version
                                            :user-id user-id
                                            :session-id session-id
                                            :session-duration (parse-integer session-duration)
                                            :snapshot-duration (parse-integer snapshot-duration)
                                            :version version)))
      (when trace
        (let ((path (make-pathname :name "trace" :type "dat" :defaults (snapshot-directory snapshot))))
          (ensure-directories-exist path)
          (uiop:copy-file (first trace) path)))
      (output snapshot "Snapshot submitted." "feedback/~a/snapshot/~a" (dm:field project "name") (dm:id snapshot)))))

(define-api feedback/snapshot/delete (snapshot) (:access (perm feedback entry delete))
  (let ((snapshot (ensure-snapshot snapshot)))
    (delete-snapshot snapshot)
    (output NIL "Snapshot deleted." "feedback/~a/" (dm:field (ensure-project snapshot) "name"))))
