(in-package #:feedback)

(defun output (object message &optional url-format &rest args)
  (let ((target (if url-format
                    (uri-to-url (apply #'format NIL url-format args)
                                :representation :external :query `(("message" . ,message)))
                    (case (dm:collection object)
                      (note (if (string= "true" (post/get "in-detail"))
                                (note-url object :query `(("message" . ,message)))
                                (track-note-url object :query `(("message" . ,message)))))
                      (entry (if (string= "true" (post/get "in-detail"))
                                 (entry-url object :query `(("message" . ,message)))
                                 (track-entry-url object :query `(("message" . ,message)))))
                      (T (object-url object :query `(("message" . ,message))))))))
    (if (string= "true" (post/get "browser"))
        (redirect target)
        (api-output object :message message :target target))))

(defun output-changelog (changes)
  (api-output (loop for change in changes
                    for table = (make-hash-table :test 'equal)
                    do (setf (gethash "time" table) (dm:field change "time"))
                       (setf (gethash "author" table) (when (dm:field change "author")
                                                        (user:get (dm:field change "author"))))
                       (setf (gethash "edit-type" table) (string-downcase (id->edit-type (dm:field change "edit-type"))))
                       (setf (gethash "object" table) (changelog-object change))
                       (setf (gethash "parent" table) (changelog-parent change))
                    collect table)))

(define-api feedback/project (name) (:access (perm feedback project list))
  (api-output (ensure-project name)))

(define-api feedback/project/list () (:access (perm feedback project list))
  (api-output (list-projects)))

(define-api feedback/project/new (name &optional trace-data-type description attachment-name[] attachment-type[] tag[] tag-color[]) (:access (perm feedback project new))
  (output (make-project name :description description
                             :trace-data-type trace-data-type
                             :attachments (loop for name in attachment-name[]
                                                for type in attachment-type[]
                                                collect (list name type))
                             :tags (loop for name in tag[]
                                         for color in tag-color[]
                                         collect (list name color)))
          "Project created"))

(define-api feedback/project/edit (project &optional name trace-data-type description attachment-name[] attachment-type[] member[] tag[] tag-color[]) (:access (perm feedback project edit))
  (let ((project (ensure-project project)))
    (check-accessible project :edit)
    (output (edit-project project :name name
                                  :description description
                                  :trace-data-type trace-data-type
                                  :attachments (loop for name in attachment-name[]
                                                     for type in attachment-type[]
                                                     collect (list name type))
                                  :tags (loop for name in tag[]
                                              for color in tag-color[]
                                              collect (list name (when color (parse-color color))))
                                  :members member[])
            "Project edited")))

(define-api feedback/project/delete (project) (:access (perm feedback project delete))
  (let ((project (ensure-project project)))
    (check-accessible project :edit)
    (delete-project project)
    (output NIL "Project deleted" "feedback/")))

(define-api feedback/project/tags (project) (:access (perm feedback project edit))
  (let ((project (ensure-project project)))
    (check-accessible project :view)
    (api-output (list-tags project))))

(define-api feedback/project/import (project csv &optional tags track) (:access (perm feedback project edit))
  (db:with-transaction ()
    (let* ((project (ensure-project project))
           (track (when (or* track) (or (find-track track project) (ensure-track track)))))
      (check-accessible project :edit)
      (import-entry-csv (first csv) project :track track :tags (cond ((string-equal tags "create") :create)
                                                                     ((string-equal tags "error") :error)
                                                                     ((string-equal tags "ignore") :ignore)
                                                                     (T (error 'api-argument-invalid :argument "tags"))))
      (output (or track project) "Entries imported"))))

(define-api feedback/project/changelog (project &optional start end) (:access (perm feedback project edit))
  (let ((object (ensure-project project)))
    (check-accessible object :view)
    (output-changelog (list-changes :object object
                                    :start (when start (parse-time start))
                                    :end (when end (parse-time end))))))

(define-api feedback/track (track) (:access (perm feedback track list))
  (let ((track (ensure-track track)))
    (check-accessible track :view)
    (api-output track)))

(define-api feedback/track/list (project &optional query skip amount) (:access (perm feedback track list))
  (let ((project (ensure-project project)))
    (check-accessible project :view)
    (api-output (list-tracks project
                             :query (or* query)
                             :skip (parse-integer (or* skip "0"))
                             :amount (parse-integer (or* amount "100"))))))

(define-api feedback/track/new (project name &optional description min-report-version permission[]) (:access (perm feedback track new))
  (let ((project (ensure-project project)))
    (check-accessible project :edit)
    (output (make-track project name :description description
                                     :min-report-version (or* min-report-version)
                                     :protection permission[])
            "Track created")))

(define-api feedback/track/edit (track &optional name description min-report-version permission[]) (:access (perm feedback track new))
  (let ((track (ensure-track track)))
    (check-accessible (ensure-project track) :edit)
    (output (edit-track track :name name
                              :description description
                              :min-report-version (or* min-report-version)
                              :protection permission[])
            "Track edited")))

(define-api feedback/track/delete (track) (:access (perm feedback track delete))
  (let* ((track (ensure-track track))
         (project (ensure-project track)))
    (check-accessible project :edit)
    (delete-track track)
    (output project "Track deleted" "feedback/")))

(define-api feedback/track/changelog (track &optional start end) ()
  (let ((object (ensure-track track)))
    (check-accessible object :view)
    (output-changelog (list-changes :object object
                                    :start (when start (parse-time start))
                                    :end (when end (parse-time end))))))

(define-api feedback/entry (entry) ()
  (let ((entry (ensure-entry entry)))
    (check-accessible entry :view)
    (api-output entry)))

(define-api feedback/entry/list (&optional project track query skip amount) ()
  (let ((object (cond (project (ensure-project project))
                      (track (ensure-track track))
                      (T (error 'api-argument-missing :argument "project")))))
    (check-accessible object :view)
    (api-output (list-entries object
                              :query (or* query)
                              :skip (parse-integer (or* skip "0"))
                              :amount (parse-integer (or* amount "100"))))))

(define-api feedback/entry/new (project user-id &optional track status version os-type cpu-type gpu-type os-info cpu-info gpu-info description assigned-to severity relates-to tag[]) ()
  (db:with-transaction ()
    (let* ((project (or (find-project project)
                        (ensure-project project)))
           (track (when track (or (find-track track project) (ensure-track track))))
           (types (list-attachments project)))
      (check-accessible (or track project) :add)
      (when (and track version (dm:field track "min-report-version")
                 (version< version (dm:field track "min-report-version")))
        (error 'api-argument-invalid :argument "version" :message (format NIL "Reports for version ~a, which is below the minimum version ~a, are not accepted."
                                                                          version (dm:field track "min-report-version"))))
      (let ((entry (make-entry project :track track
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
                                       :tags tag[])))
        (loop for type in types
              for file = (post-var (dm:field type "name"))
              for path = (attachment-pathname entry type)
              do (when file
                   (ensure-directories-exist path)
                   (uiop:copy-file (first file) path)))
        (output entry "Feedback submitted")))))

(define-api feedback/entry/edit (entry &optional track user-id description status assigned-to severity relates-to order tag[]) ()
  (db:with-transaction ()
    (let* ((entry (ensure-entry entry))
           (types (list-attachments (dm:field entry "project"))))
      (check-accessible entry :edit)
      (edit-entry entry :description description :status status :order (cond ((string= "top" order) :top)
                                                                             ((string= "bottom" order) :bottom)
                                                                             ((stringp order) (parse-integer order)))
                        :user-id user-id :assigned-to assigned-to :severity severity :relates-to relates-to :track track :tags (if (post/get "tag[]") tag[] :none))
      (loop for type in types
            for file = (post-var (dm:field type "name"))
            for path = (attachment-pathname entry type)
            do (when file
                 (log-change entry :edit)
                 (ensure-directories-exist path)
                 (uiop:copy-file (first file) path)))
      (output entry "Entry updated"))))

(define-api feedback/entry/render (entry[]) ()
  (setf (content-type *response*) "text/html")
  (handler-bind ((plump:invalid-xml-character #'abort)
                 (plump:discouraged-xml-character #'muffle-warning))
    (let ((plump:*tag-dispatchers* plump:*html-tags*))
      (plump:serialize
       (r-clip:process "<c:splice iterate=\"entries\"><c:h>(plump:clone-node (** :entry-content) T)"
                       :entries (loop for name in entry[]
                                      for entry = (ensure-entry name)
                                      do (check-accessible entry :view)
                                      collect entry)
                       :entry-content (plump:parse (template-file "entry.ctml" :feedback)))
       NIL))))

(define-api feedback/entry/tag/new (entry tag) ()
  (let ((entry (ensure-entry entry)))
    (check-accessible entry :edit)
    (let ((tags (list-tags entry))
          (tag (ensure-tag tag (dm:field entry "project"))))
      (unless (find (dm:id tag) tags :key (lambda (x) (dm:field x "tag")) :test #'equal)
        (db:insert 'entry-tag `(("entry" . ,(dm:id entry)) ("tag" . ,(dm:id tag)))))
      (output tag "Tag added"))))

(define-api feedback/entry/tag/delete (entry tag) ()
  (let ((entry (ensure-entry entry)))
    (check-accessible entry :edit)
    (let ((tag (ensure-tag tag (dm:field entry "project"))))
      (db:remove 'entry-tag (db:query (:and (:= 'entry (dm:id entry)) (:= 'tag (dm:id tag)))))
      (output tag "Tag removed"))))

(define-api feedback/entry/delete (entry) (:access (perm feedback entry delete))
  (let ((entry (ensure-entry entry)))
    (check-accessible entry :edit)
    (delete-entry entry)
    (output NIL "Entry deleted" "feedback/~a/" (dm:field (ensure-project entry) "name"))))

(define-api feedback/entry/changelog (entry &optional start end) ()
  (let ((object (ensure-entry entry)))
    (check-accessible object :view)
    (output-changelog (list-changes :object object
                                    :start (when start (parse-time start))
                                    :end (when end (parse-time end))))))

(define-api feedback/note (note) ()
  (let ((note (ensure-note note)))
    (check-accessible note :view)
    (api-output note)))

(define-api feedback/note/list (entry &optional skip amount) ()
  (let ((entry (ensure-entry entry)))
    (check-accessible entry :view)
    (api-output (list-notes entry :skip (parse-integer (or* skip "0"))
                                  :amount (parse-integer (or* amount "100"))))))

(define-api feedback/note/new (entry text) ()
  (let ((entry (ensure-entry entry)))
    (check-accessible entry :edit)
    (output (make-note entry text) "Note created")))

(define-api feedback/note/edit (note text) ()
  (let ((note (ensure-note note)))
    (check-accessible note :edit)
    (output (edit-note note :text text) "Note edited")))

(define-api feedback/note/delete (note) ()
  (let ((note (ensure-note note)))
    (check-accessible note :edit)
    (output (delete-note note) "Note deleted")))

(define-api feedback/event (event) ()
  (let ((event (ensure-event event)))
    (check-accessible event :view)
    (api-output event)))

(define-api feedback/event/list (timeline &optional start end) ()
  (let ((timeline (ensure-timeline timeline)))
    (check-accessible timeline :view)
    (api-output (list-events timeline :start (when start (parse-time start))
                                      :end (when end (parse-time end))))))

(define-api feedback/event/new (timeline entry start end &optional (layer "0")) ()
  (let ((entry (ensure-entry entry))
        (timeline (ensure-timeline timeline)))
    (check-accessible timeline :edit)
    (output (make-event timeline entry (parse-time start) (parse-time end) :layer (parse-integer layer)) "Event created")))

(define-api feedback/event/edit (event &optional start end layer) ()
  (let ((event (ensure-event event)))
    (check-accessible event :edit)
    (output (edit-event event :start (when start (parse-time start)) :end (when end (parse-time end)) :layer layer) "Event edited")))

(define-api feedback/event/delete (event) ()
  (let ((event (ensure-event event)))
    (check-accessible event :edit)
    (output (delete-event event) "Event deleted")))

(define-api feedback/event/changelog (event &optional start end) ()
  (let ((object (ensure-event event)))
    (check-accessible object :view)
    (output-changelog (list-changes :object object
                                    :start (when start (parse-time start))
                                    :end (when end (parse-time end))))))

(define-api feedback/deadline (deadline) ()
  (let ((deadline (ensure-deadline deadline)))
    (check-accessible deadline :view)
    (api-output deadline)))

(define-api feedback/deadline/list (timeline &optional start end) ()
  (let ((timeline (ensure-timeline timeline)))
    (check-accessible timeline :view)
    (api-output (list-deadlines timeline :start (when start (parse-time start))
                                         :end (when end (parse-time end))))))

(define-api feedback/deadline/new (timeline name time) ()
  (let ((timeline (ensure-timeline timeline)))
    (check-accessible timeline :edit)
    (output (make-deadline timeline name (parse-time time)) "Deadline created")))

(define-api feedback/deadline/edit (deadline &optional name time) ()
  (let ((deadline (ensure-deadline deadline)))
    (check-accessible deadline :edit)
    (output (edit-deadline deadline :name name :time (when time (parse-time time))) "Deadline edited")))

(define-api feedback/deadline/delete (deadline) ()
  (let ((deadline (ensure-deadline deadline)))
    (check-accessible deadline :edit)
    (output (delete-deadline deadline) "Deadline deleted")))

(define-api feedback/snapshot (snapshot) ()
  (let ((snapshot (ensure-snapshot snapshot)))
    (check-accessible snapshot :view)
    (api-output snapshot)))

(define-api feedback/snapshot/new (project user-id session-id session-duration snapshot-duration &optional version trace) ()
  (db:with-transaction ()
    (let* ((project (check-accessible (or (find-project project) (ensure-project project)) :edit))
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

(define-api feedback/snapshot/delete (snapshot) ()
  (let ((snapshot (ensure-snapshot snapshot)))
    (check-accessible snapshot :edit)
    (delete-snapshot snapshot)
    (output NIL "Snapshot deleted" "feedback/~a/" (dm:field (ensure-project snapshot) "name"))))

(define-api feedback/timeline (timeline) ()
  (let ((timeline (ensure-timeline timeline)))
    (check-accessible timeline :view)
    (api-output timeline)))

(define-api feedback/timeline/list (project &optional query skip amount) (:access (perm feedback timeline list))
  (let ((project (ensure-project project)))
    (check-accessible project :view)
    (api-output (list-timelines project
                                :query (or* query)
                                :skip (parse-integer (or* skip "0"))
                                :amount (parse-integer (or* amount "100"))))))

(define-api feedback/timeline/new (project name &optional description start end permission[]) (:access (perm feedback timeline new))
  (let ((project (ensure-project project)))
    (check-accessible project :edit)
    (output (make-timeline project name :description description
                                        :protection permission[]
                                        :start (when start (parse-time start))
                                        :end (when end (parse-time end)))
            "Timeline created")))

(define-api feedback/timeline/edit (timeline &optional name description start end permission[]) (:access (perm feedback timeline new))
  (let ((timeline (ensure-timeline timeline)))
    (check-accessible (ensure-project timeline) :edit)
    (output (edit-timeline timeline :name name
                                    :description description
                                    :protection permission[]
                                    :start (when start (parse-time start))
                                    :end (when end (parse-time end)))
            "Timeline edited")))

(define-api feedback/timeline/delete (timeline) (:access (perm feedback timeline delete))
  (let* ((timeline (ensure-timeline timeline))
         (project (ensure-project timeline)))
    (check-accessible project :edit)
    (delete-timeline timeline)
    (output project "Timeline deleted" "feedback/")))

(define-api feedback/subscription/add (object-type object-id type[]) ()
  (let ((object (ensure-object object-type object-id)))
    (check-accessible (ensure-project object) :edit)
    (subscribe-to object type[])
    (output object "Subscription updated" "feedback/subscribe/~a/~a" object-type object-id)))

(define-api feedback/subscription/remove (object-type object-id type[]) ()
  (let ((object (ensure-object object-type object-id)))
    (check-accessible (ensure-project object) :edit)
    (unsubscribe-from object type[])
    (output object "Subscription updated" "feedback/subscribe/~a/~a" object-type object-id)))

(define-api feedback/subscription/edit (object-type object-id type[]) ()
  (let ((object (ensure-object object-type object-id)))
    (check-accessible (ensure-project object) :edit)
    (set-subscription object type[])
    (output object "Subscription updated" "feedback/subscribe/~a/~a" object-type object-id)))

(define-api feedback/user/changelog (user &optional start end) ()
  (let ((object (user:get user :if-does-not-exist :error)))
    (when (or (eql object (auth:current NIL))
              (user:check (auth:current) (perm feedback)))
      (output-changelog (list-changes :author object
                                      :start (when start (parse-time start))
                                      :end (when end (parse-time end)))))))
