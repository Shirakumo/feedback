(in-package #:feedback)

(defun output (object message &optional url-format &rest args)
  (let ((target (if url-format
                    (uri-to-url (apply #'format NIL url-format args)
                                :representation :external :query `(("message" . ,message)))
                    (ecase (dm:collection object)
                      (note (if (string= "true" (post/get "in-detail"))
                                (note-url object :query `(("message" . ,message)))
                                (track-note-url object :query `(("message" . ,message)))))
                      (entry (if (string= "true" (post/get "in-detail"))
                                 (entry-url object :query `(("message" . ,message)))
                                 (track-entry-url object :query `(("message" . ,message)))))
                      (project (project-url object :query `(("message" . ,message))))
                      (track (track-url object :query `(("message" . ,message))))
                      (trace (trace-url object :query `(("message" . ,message))))
                      (snapshot (snapshot-url object :query `(("message" . ,message))))
                      (attachment (entry-url (dm:field object "entry") :query `(("message" . ,message))))))))
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
          "Project created"))

(define-api feedback/project/edit (project &optional name trace-data-type description attachment-name[] attachment-type[] member[]) (:access (perm feedback project edit))
  (output (edit-project project :name name
                                :description description
                                :trace-data-type trace-data-type
                                :attachments (loop for name in attachment-name[]
                                                   for type in attachment-type[]
                                                   collect (list name type))
                                :members member[])
          "Project edited"))

(define-api feedback/project/delete (project) (:access (perm feedback project delete))
  (delete-project (ensure-project project))
  (output NIL "Project deleted" "feedback/"))

(define-api feedback/track/list (project &optional query skip amount) (:access (perm feedback track list))
  (api-output (list-tracks (ensure-project project)
                           :query (or* query)
                           :skip (parse-integer (or* skip "0"))
                           :amount (parse-integer (or* amount "100")))))

(define-api feedback/track/new (project name &optional description protection) (:access (perm feedback track new))
  (let ((project (ensure-project project)))
    (output (make-track project name :description description :protection protection)
            "Track created")))

(define-api feedback/track/edit (track &optional name description protection) (:access (perm feedback track new))
  (let ((track (ensure-track track)))
    (output (edit-track track :name name
                              :description description
                              :protection protection)
            "Track edited")))

(define-api feedback/track/delete (track) (:access (perm feedback track delete))
  (delete-track (ensure-track track))
  (output NIL "Track deleted" "feedback/"))

(define-api feedback/entry/list (&optional project track query skip amount) (:access (perm feedback entry list))
  (api-output (list-entries (cond (project (ensure-project project))
                                  (track (ensure-track track))
                                  (T (error 'api-argument-missing :argument "project")))
                            :query (or* query)
                            :skip (parse-integer (or* skip "0"))
                            :amount (parse-integer (or* amount "100")))))

(define-api feedback/entry/new (project user-id &optional track status version os-type cpu-type gpu-type os-info cpu-info gpu-info description assigned-to severity relates-to) (:access (perm feedback entry new))
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
                                      :severity severity)))
      (loop for type in types
            for file = (post-var (dm:field type "name"))
            do (when file
                 (ensure-directories-exist (attachment-pathname entry type))
                 (uiop:copy-file (first file) (attachment-pathname entry type))))
      (output entry "Feedback submitted"))))

(define-api feedback/entry/edit (entry &optional description status assigned-to severity relates-to order) (:access (perm feedback entry edit))
  (db:with-transaction ()
    (let* ((entry (ensure-entry entry)))
      (edit-entry entry :description description :status status :order (cond ((string= "top" order) :top)
                                                                             ((string= "bottom" order) :bottom)
                                                                             ((stringp order) (parse-integer order)))
                        :assigned-to assigned-to :severity severity :relates-to relates-to)
      (output entry "Entry updated"))))

(define-api feedback/entry/delete (entry) (:access (perm feedback entry delete))
  (let ((entry (ensure-entry entry)))
    (delete-entry entry)
    (output NIL "Entry deleted" "feedback/~a/" (dm:field (ensure-project entry) "name"))))

(define-api feedback/note/new (entry text) (:access (perm feedback note new))
  (let ((note (make-note entry text)))
    (output note "Note created")))

(define-api feedback/note/edit (note text) (:access (perm feedback note edit))
  (let ((note (edit-note note :text text)))
    (output note "Note edited")))

(define-api feedback/note/delete (note) (:access (perm feedback note delete))
  (let ((note (delete-note note)))
    (output note "Note deleted")))

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
      (output snapshot "Snapshot submitted"))))

(define-api feedback/snapshot/delete (snapshot) (:access (perm feedback entry delete))
  (let ((snapshot (ensure-snapshot snapshot)))
    (delete-snapshot snapshot)
    (output NIL "Snapshot deleted" "feedback/~a/" (dm:field (ensure-project snapshot) "name"))))

(define-api feedback/subscription/add (object-type object-id type[]) (:access (perm feedback project subscribe))
  (let ((object (ensure-object object-type object-id)))
    (subscribe-to object type[])
    (output object "Subscription updated" "feedback/subscribe/~a/~a" object-type object-id)))

(define-api feedback/subscription/remove (object-type object-id type[]) (:access (perm feedback project subscribe))
  (let ((object (ensure-object object-type object-id)))
    (unsubscribe-from object type[])
    (output object "Subscription updated" "feedback/subscribe/~a/~a" object-type object-id)))

(define-api feedback/subscription/edit (object-type object-id type[]) (:access (perm feedback project subscribe))
  (let ((object (ensure-object object-type object-id)))
    (set-subscription object type[])
    (output object "Subscription updated" "feedback/subscribe/~a/~a" object-type object-id)))
