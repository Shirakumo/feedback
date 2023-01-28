(in-package #:feedback)

(define-trigger db:connected ()
  (db:create 'project
             '((name (:varchar 32))
               (description :text)
               (trace-data-type (:integer 1)))
             :indices '(name))
  
  (db:create 'attachment
             '((project (:id project))
               (name (:varchar 32))
               (type (:integer 1)))
             :indices '(project))

  (db:create 'members
             '((project (:id project))
               (user :id))
             :indices '(project user))

  (db:create 'track
             '((project (:id project))
               (name (:varchar 32))
               (description :text)
               (protection (:integer 1)))
             :indices '(project))

  (db:create 'entry
             '((project (:id project))
               (track (:id track))
               (status (:integer 1))
               (version (:varchar 64))
               (user-id (:varchar 64))
               (order (:integer 4))
               (time (:integer 5))
               (os-type (:integer 1))
               (cpu-type (:integer 1))
               (gpu-type (:integer 1))
               (os-info :text)
               (cpu-info :text)
               (gpu-info :text)
               (description :text)
               (assigned-to :id)
               (severity (:integer 1))
               (relates-to (:id entry)))
             :indices '(project track assigned-to severity user-id os-type cpu-type gpu-type))
  
  (db:create 'snapshot
             '((project (:id project))
               (version (:varchar 64))
               (user-id (:varchar 64))
               (session-id (:varchar 64))
               (session-duration (:integer 4))
               (snapshot-duration (:integer 4))
               (time (:integer 5)))
             :indices '(project user-id session-id))

  (db:create 'note
             '((entry (:id entry))
               (author :id)
               (time (:integer 5))
               (text :text))
             :indices '(entry))

  (db:create 'subscriber
             '((object :id)
               (object-type (:integer 1))
               (user :id)
               (type (:integer 2)))))

;; TODO: test notifs
;; TODO: anonymously viewable tracks
;; TODO: manual attachments
;; TODO: update without page refresh
;; TODO: csv import

(defun check-name (name)
  (let ((names '("new" "snapshot" "edit" "entry" "user" "subscribe")))
    (when (or (find name names :test #'string-equal)
              (every #'digit-char-p name))
      (error 'api-argument-invalid :argument "name" :message (format NIL "The name cannot be numeric, or one of: ~{~a~^, ~}" names)))))

(defmacro define-mapping ((a b) &body mappings)
  `(progn
     (defun ,(intern (format NIL "~a~a" (string a) (string 's))) ()
       ',(mapcar #'first mappings))
     (defun ,(intern (format NIL "~a->~a" (string a) (string b))) (thing)
       (cond ,@(loop for (a b) in mappings
                     collect `((equalp thing ,(string a)) ,b)
                     collect `((equalp thing ,(princ-to-string b)) ,b))
             (T (ecase thing
                  ,@(loop for (a b) in mappings
                          collect `((,a ,b) ,b))))))
     (defun ,(intern (format NIL "~a->~a" (string b) (string a))) (thing)
       (ecase thing
         ,@(loop for (a b) in mappings
                 collect (list b `',a)
                 collect (list a `',a))))))

(define-mapping (os-type id)
  (:unknown 0)
  (:windows 1)
  (:linux 2)
  (:macos 3)
  (:freebsd 4)
  (:android 5)
  (:ios 6))

(define-mapping (cpu-type id)
  (:unknown 0)
  (:amd64 1)
  (:i686 2)
  (:arm64 3)
  (:armv7l 4))

(define-mapping (gpu-type id)
  (:unknown 0)
  (:intel 1)
  (:nvidia 2)
  (:amd 3)
  (:vmware 4)
  (:virtualbox 5))

(define-mapping (attachment-type id)
  (:dat 0)
  (:png 1)
  (:jpg 2)
  (:txt 3)
  (:log 4)
  (:zip 5)
  (:sig 6)
  (:csv 7)
  (:json 8)
  (:xml 9))

(define-mapping (status id)
  (:new 0)
  (:triaged 1)
  (:resolved 2)
  (:wontfix 3)
  (:invalid 4)
  (:duplicate 5)
  (:unclear 6)
  (:deleted 7))

(define-mapping (trace-data-type id)
  (:none 0)
  (:2d-points 1)
  (:3d-points 2))

(define-mapping (protection id)
  (:private 0)
  (:registered-read 1)
  (:registered-write 2)
  (:public-read 3)
  (:public-write 4))

(define-mapping (object-type id)
  (project 0)
  (track 1)
  (entry 2)
  (note 3)
  (members 4)
  (snapshot 5)
  (attachment 6)
  (subscriber 7))

(defun os-type-icon (os-type)
  (case os-type
    ((:windows 1) "fa-windows")
    ((:linux 2) "fa-linux")
    ((:macos 3) "fa-apple")
    ((:freebsd 4) "fa-freebsd")
    ((:android 5) "fa-android")
    ((:ios 6) "fa-app-store-ios")
    (T "fa-question-circle")))

(defun status-icon (status)
  (case status
    ((:new 0) "fa-exclamation-circle")
    ((:resolved 2) "fa-check-circle")
    ((:wontfix 3) "fa-ban")
    ((:invalid 4) "fa-xmark")
    ((:duplicate 5) "fa-code-merge")
    ((:unclear 6) "fa-question-circle")
    ((:deleted 7) "fa-trash")
    (T "fa-ellipsis-h")))

(defun attachment-type-content-type (attachment-type)
  (case attachment-type
    ((:png 1) "image/png")
    ((:jpg 2) "image/jpeg")
    ((:txt 3) "text/plain")
    ((:log 4) "text/plain")
    ((:zip 5) "application/zip")
    ((:sig 6) "application/pgp-signature")
    ((:csv 7) "text/csv")
    ((:json 8) "application/json")
    ((:xml 9) "application/xml")
    (T "application/octet-stream")))

(defun attachment-image-p (attachment-type)
  (case attachment-type
    ((:png 1 :jpg 2) T)))

(defun attachment-text-p (attachment-type)
  (case attachment-type
    ((:txt 3 :log 4) T)))

(defun id->subscription-types (int)
  (let ((types ()))
    (when (logbitp 0 int) (push :entry-new types))
    (when (logbitp 1 int) (push :entry-edit types))
    (when (logbitp 2 int) (push :note-new types))
    (when (logbitp 3 int) (push :note-edit types))
    (when (logbitp 4 int) (push :track-new types))
    (when (logbitp 5 int) (push :project-new types))
    (when (logbitp 6 int) (push :snapshot-new types))
    (when (logbitp 7 int) (push :member-new types))
    types))

(defun subscription-types->id (types)
  (etypecase types
    (integer types)
    (symbol (subscription-types->id (list types)))
    (list
     (let ((int 0))
       (when (member :entry-new types :test #'string-equal) (setf (ldb (byte 1 0) int) 1))
       (when (member :entry-edit types :test #'string-equal) (setf (ldb (byte 1 1) int) 1))
       (when (member :note-new types :test #'string-equal) (setf (ldb (byte 1 2) int) 1))
       (when (member :note-edit types :test #'string-equal) (setf (ldb (byte 1 3) int) 1))
       (when (member :track-new types :test #'string-equal) (setf (ldb (byte 1 4) int) 1))
       (when (member :project-new types :test #'string-equal) (setf (ldb (byte 1 5) int) 1))
       (when (member :snapshot-new types :test #'string-equal) (setf (ldb (byte 1 6) int) 1))
       (when (member :member-new types :test #'string-equal) (setf (ldb (byte 1 7) int) 1))
       int))))

(defun project-directory (project)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(princ-to-string (dm:id project))))
   (environment-module-pathname #.*package* :data "attachments/")))

(defun entry-directory (entry)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(princ-to-string (dm:field entry "project"))
                                         ,(princ-to-string (dm:id entry))))
   (environment-module-pathname #.*package* :data "attachments/")))

(defun snapshot-directory (snapshot)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(princ-to-string (dm:field snapshot "project"))
                                         ,(princ-to-string (dm:id snapshot))))
   (environment-module-pathname #.*package* :data "snapshots/")))

(defun attachment-pathname (entry type)
  (make-pathname :name (dm:field type "name")
                 :type (string-downcase (id->attachment-type (dm:field type "type")))
                 :defaults (entry-directory entry)))

(defun ensure-project (project-ish)
  (typecase project-ish
    (dm:data-model
     (ecase (dm:collection project-ish)
       (project project-ish)
       ((attachment track entry note snapshot members)
        (dm:get-one 'project (db:query (:= '_id (dm:field project-ish "project")))))))
    (T
     (or (dm:get-one 'project (db:query (:= '_id (ensure-id project-ish))))
         (error 'request-not-found :message "Could not find the requested project.")))))

(defun find-project (name)
  (dm:get-one 'project (db:query (:= 'name name))))

(defun list-projects (&optional (member (auth:current NIL)))
  (if member
      (mapcar #'ensure-project (dm:get 'members (db:query (:= 'user (user:id member)))))
      (dm:get 'project (db:query :all))))

(defun make-project (name &key description trace-data-type attachments (author (auth:current)))
  (check-name name)
  (db:with-transaction ()
    (when (find-project name)
      (error "Project named ~s already exists" name))
    (let ((model (dm:hull 'project)))
      (setf-dm-fields model name description)
      (setf (dm:field model "trace-data-type") (trace-data-type->id (or trace-data-type :none)))
      (dm:insert model)
      (loop for (name type) in attachments
            for sub = (dm:hull 'attachment)
            when (or* name)
            do (setf-dm-fields sub (model "project") name)
               (setf (dm:field sub "type") (attachment-type->id type))
               (dm:insert sub))
      (db:insert 'members `(("project" . ,(dm:id model)) ("user" . ,(user:id author))))
      (notify model :project-new)
      model)))

(defun edit-project (project &key name description trace-data-type (attachments NIL attachments-p) members)
  (db:with-transaction ()
    (let ((project (ensure-project project)))
      (setf-dm-fields project name description)
      (when trace-data-type
        (setf (dm:field project "trace-data-type") (trace-data-type->id trace-data-type)))
      (dm:save project)
      (when attachments-p
        (let ((existing (list-attachments project)))
          (loop for (name type) in attachments
                for entry = (find name existing :key (lambda (n) (dm:field n "name"))
                                                :test #'string-equal)
                do (cond (entry
                          (setf (dm:field entry "type") (attachment-type->id type))
                          (setf existing (delete entry existing)))
                         ((or* name)
                          (let ((sub (dm:hull 'attachment)))
                            (setf-dm-fields sub (project "project") name)
                            (setf (dm:field sub "type") (attachment-type->id type))
                            (dm:insert sub)))))
          (dolist (attachment existing)
            (dm:delete attachment))))
      (when members
        (db:remove 'members (db:query (:= 'project (dm:id project))))
        ;; FIXME: Notify for new members
        (dolist (member members)
          (db:insert 'members `(("project" . ,(dm:id project)) ("user" . ,(user:id member))))))
      project)))

(defun delete-project (project)
  (db:with-transaction ()
    (let ((project (ensure-project project)))
      (mapc #'delete-entry (list-entries project))
      (mapc #'delete-snapshot (list-snapshots project))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id project)) (:= 'object-type (object-type->id 'project)))))
      (db:remove 'members (db:query (:= 'project (dm:id project))))
      (dm:delete project)
      (delete-directory (project-directory project)))))

(defun find-track (name project)
  (dm:get-one 'track (db:query (:and (:= 'name name) (:= 'project (ensure-id project))))))

(defun ensure-track (track-ish)
  (etypecase track-ish
    (dm:data-model
     (ecase (dm:collection track-ish)
       (entry
        (dm:get-one 'track (db:query (:= '_id (dm:field track-ish "track")))))
       (track track-ish)))
    (T
     (or (dm:get-one 'track (db:query (:= '_id (ensure-id track-ish))))
         (error 'request-not-found :message "Could not find the requested track.")))))

(defun list-tracks (project &key (skip 0) (amount 50) query)
  (dm:get 'track (if query
                     (db:query (:and (:= 'project (ensure-id project))
                                     (:MATCHES* 'name (cl-ppcre:quote-meta-chars query))))
                     (db:query (:= 'project (ensure-id project))))
          :skip skip :amount amount :sort '(("name" :desc))))

(defun make-track (project name &key description protection)
  (check-name name)
  (let ((project (ensure-project project))
        (track (dm:hull 'track)))
    (setf-dm-fields track project name description)
    (setf (dm:field track "protection") (protection->id (or protection :private)))
    (prog1 (dm:insert track)
      (notify track :track-new))))

(defun edit-track (track &key name description protection)
  (db:with-transaction ()
    (let ((track (ensure-track track)))
      (setf-dm-fields track name description)
      (when protection
        (setf (dm:field track "protection") (protection->id protection)))
      (dm:save track))))

(defun delete-track (track &key (delete-entries T))
  (db:with-transaction ()
    (let ((track (ensure-track track)))
      (if delete-entries
          (db:remove 'entry (db:query (:= 'track (dm:id track))))
          (db:update 'entry (db:query (:= 'track (dm:id track))) `(("track" . NIL))))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id track)) (:= 'object-type (object-type->id 'track)))))
      (db:remove 'track (db:query (:= '_id (dm:id track)))))))

(defun list-members (project)
  (db:iterate 'members (db:query (:= 'project (ensure-id project)))
    (lambda (row) (user:get (gethash "user" row)))
    :accumulate T :fields '("user")))

(defun list-attachments (thing)
  (cond ((or (typep thing 'db:id) (eql 'project (dm:collection thing)))
         (dm:get 'attachment (db:query (:= 'project (ensure-id thing))) :sort '(("name" :asc))))
        ((eql 'entry (dm:collection thing))
         (let ((types (list-attachments (dm:field thing "project"))))
           (loop for type in types
                 when (probe-file (attachment-pathname thing type))
                 collect type)))
        (T
         (error "Don't know wtf to do with~%  ~a" thing))))

(defun ensure-attachment (project name)
  (or (dm:get-one 'attachment (db:query (:and (:= 'project (ensure-id project))
                                              (:= 'name name))))
      (error 'request-not-found :message "Could not find the requested attachment.")))

(defun ensure-entry (entry-ish)
  (etypecase entry-ish
    (dm:data-model
     (ecase (dm:collection entry-ish)
       (entry entry-ish)
       (note
        (dm:get-one 'entry (db:query (:= '_id (dm:field entry-ish "entry")))))))
    (T
     (or (dm:get-one 'entry (db:query (:= '_id (ensure-id entry-ish))))
         (error 'request-not-found :message "Could not find the requested entry.")))))

(defun list-entries (&optional parent &key (skip 0) (amount 50) query)
  (macrolet ((query (query)
               `(cond ((null query)
                       (db:query ,query))
                      ((id-code-p query)
                       (db:query (:and ,query
                                       (:or (:= '_id (parse-id-code query))
                                            (:matches* 'description (cl-ppcre:quote-meta-chars query))))))
                      (T
                       (db:query (:and ,query
                                       (:matches* 'description (cl-ppcre:quote-meta-chars query))))))))
    (dm:get 'entry (etypecase parent
                     (user:user
                      (query (:and (:= 'assigned-to (user:id parent))
                                      (:<= 'status 1))))
                     (dm:data-model
                      (ecase (dm:collection parent)
                        (project (query (:= 'project (dm:id parent))))
                        (track (query (:= 'track (dm:id parent))))))
                     (db:id
                      (query (:= 'project parent)))
                     (null
                      (query :all)))
            :skip skip :amount amount :sort '(("order" :desc) ("time" :desc)))))

(defun make-entry (project &key (time (get-universal-time))
                                (user-id (user:username (auth:current)))
                                track status
                                os-type os-info
                                cpu-type cpu-info
                                gpu-type gpu-info
                                version description
                                assigned-to severity
                                relates-to)
  (db:with-transaction ()
    (let ((project (ensure-project project))
          (track (when track (ensure-track track)))
          (relates-to (when relates-to (ensure-entry relates-to)))
          (model (dm:hull 'entry))
          (severity (or severity 0))
          (status (or status 0)))
      (setf-dm-fields model project track version time user-id os-info cpu-info gpu-info description severity relates-to)
      (let ((highest (dm:get-one 'entry (if track (db:query (:= 'track (dm:id track))) (db:query (:= 'project (dm:id project)))) :sort `(("order" :desc)))))
        (setf (dm:field model "order") (if highest (1+ (dm:field highest "order")) 0)))
      (setf (dm:field model "assigned-to") (when assigned-to (user:id assigned-to)))
      (setf (dm:field model "status") (status->id (or status :new)))
      (setf (dm:field model "os-type") (os-type->id (or os-type :unknown)))
      (setf (dm:field model "cpu-type") (cpu-type->id (or cpu-type :unknown)))
      (setf (dm:field model "gpu-type") (gpu-type->id (or gpu-type :unknown)))
      (prog1 (dm:insert model)
        (notify model :entry-new project)))))

(defun edit-entry (entry &key track user-id description status (assigned-to NIL assigned-p) (relates-to NIL relates-to-p) severity order)
  (db:with-transaction ()
    (let ((entry (ensure-entry entry)))
      (setf-dm-fields entry track user-id description severity relates-to)
      (when relates-to-p
        (setf (dm:field entry "assigned-to") (when relates-to (ensure-entry relates-to))))
      (when assigned-p
        (setf (dm:field entry "assigned-to") (when assigned-to (user:id assigned-to))))
      (when status
        (setf (dm:field entry "status") (status->id status)))
      (when order
        (dm:save entry)
        (let* ((prev (dm:field entry "order"))
               (track (dm:field entry "track"))
               (order (case order
                        (:top (dm:field (dm:get-one 'entry (if track (db:query (:= 'track track)) (db:query (:= 'project (dm:field entry "project")))) :sort `(("order" :desc))) "order"))
                        (:bottom (dm:field (dm:get-one 'entry (if track (db:query (:and (:= 'track track) (:<= 'status 1))) (db:query (:and (:= 'project (dm:field entry "project")) (:<= 'status 1)))) :sort `(("order" :asc))) "order"))
                        (T order))))
          ;; FIXME: this sucks. But we can't do better with the standard interface.
          (cond ((< prev order)
                 (loop for record in (db:select 'entry (db:query (:and (:< prev 'order) (:<= 'order order))))
                       do (db:update 'entry (db:query (:= '_id (gethash "_id" record))) `(("order" . ,(1- (gethash "order" record)))))))
                ((< order prev)
                 (loop for record in (db:select 'entry (db:query (:and (:<= order 'order) (:< 'order prev))))
                       do (db:update 'entry (db:query (:= '_id (gethash "_id" record))) `(("order" . ,(1+ (gethash "order" record))))))))
          (setf (dm:field entry "order") order)))
      (prog1 (dm:save entry)
        (notify entry :entry-edit)))))

(defun delete-entry (entry &key (delete-related T))
  (db:with-transaction ()
    (let ((entry (ensure-entry entry)))
      (delete-directory (entry-directory entry))
      (if delete-related
          (db:remove 'entry (db:query (:= 'relates-to (dm:id entry))))
          (db:update 'entry (db:query (:= 'relates-to (dm:id entry))) `(("relates-to" . NIL))))
      (db:remove 'note (db:query (:= 'entry (dm:id entry))))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id entry)) (:= 'object-type (object-type->id 'entry)))))
      (dm:delete entry))))

(defun list-related (entry)
  (dm:get 'entry (db:query (:= 'relates-to (dm:id entry)))))

(defun list-notes (entry &key (skip 0) (amount 100))
  (dm:get 'note (db:query (:= 'entry (dm:id entry)))
          :skip skip :amount amount :sort '(("time" :asc))))

(defun make-note (entry text &key (author (auth:current)) (time (get-universal-time)))
  (let ((entry (ensure-entry entry))
        (model (dm:hull 'note)))
    (setf-dm-fields model entry text time)
    (setf (dm:field model "author") (user:id author))
    (prog1 (dm:insert model)
      (notify model :note-new))))

(defun ensure-note (note-ish)
  (etypecase note-ish
    (dm:data-model
     (ecase (dm:collection note-ish)
       (note note-ish)))
    (T
     (or (dm:get-one 'note (db:query (:= '_id (ensure-id note-ish))))
         (error 'request-not-found :message "Could not find the requested note.")))))

(defun edit-note (note &key text)
  (let ((note (ensure-note note)))
    (setf-dm-fields note text)
    (prog1 (dm:save note)
      (notify note :note-edit))))

(defun delete-note (note)
  (db:with-transaction ()
    (db:remove 'subscriber (db:query (:and (:= 'object (dm:id note)) (:= 'object-type (object-type->id 'note)))))
    (dm:delete note)))

(defun ensure-snapshot (snapshot-ish)
  (etypecase snapshot-ish
    (dm:data-model
     (ecase (dm:collection snapshot-ish)
       (snapshot snapshot-ish)))
    (T
     (or (dm:get-one 'snapshot (db:query (:= '_id (ensure-id snapshot-ish))))
         (error 'request-not-found :message "Could not find the requested snapshot.")))))

(defun list-snapshots (project &key user-id session-id (skip 0) (amount 50))
  (dm:get 'snapshot (cond (user-id (db:query (:and (:= 'project (ensure-id project))
                                                   (:= 'user-id user-id))))
                          (session-id (db:query (:and (:= 'project (ensure-id project))
                                                      (:= 'session-id session-id))))
                          (T (db:query (:= 'project (ensure-id project)))))
          :skip skip :amount amount :sort '(("time" :desc))))

(defun make-snapshot (project &key (time (get-universal-time))
                                   user-id session-id session-duration
                                   snapshot-duration version)
  (let ((project (ensure-project project))
        (model (dm:hull 'snapshot)))
    (setf-dm-fields model project time user-id session-id session-duration snapshot-duration version)
    (prog1 (dm:insert model)
      (notify model :snapshot-new))))

(defun delete-snapshot (snapshot)
  (db:with-transaction ()
    (let ((snapshot (ensure-snapshot snapshot)))
      (delete-directory (snapshot-directory snapshot))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id snapshot)) (:= 'object-type (object-type->id 'snapshot)))))
      (dm:delete snapshot))))

(defun ensure-object (type id)
  (ecase (id->object-type (object-type->id type))
    (project (ensure-project id))
    (track (ensure-track id))
    (entry (ensure-entry id))
    (note (ensure-note id))
    (snapshot (ensure-snapshot id))))

(defun notify (object type &optional (project (ensure-project object)) (author (auth:current)))
  (let* ((type-template
           (ecase type
             (:entry-new (@template "mail-entry-new.ctml"))
             (:entry-edit (@template "mail-entry-edit.ctml"))
             (:note-new (@template "mail-note-new.ctml"))
             (:note-edit (@template "mail-note-edit.ctml"))
             (:track-new (@template "mail-track-new.ctml"))
             (:project-new (@template "mail-project-new.ctml"))
             (:snapshot-new (@template "mail-snapshot-new.ctml"))
             (:member-new (@template "mail-member-new.ctml"))))
         (rendered
           (let ((*package* #.*package*))
             (clip:process type-template :project project :object object)))
         (subject (lquery:$1 rendered "title" (text)))
         (body (lquery:$1 rendered "body" (text)))
         (mask (subscription-types->id type)))
    (db:iterate 'subscriber (cond ((and (find type '(:entry-new :entry-edit))
                                        (dm:field object "track"))
                                   (db:query (:and (:or (:and (:= 'object (dm:field object "project")) (:= 'object-type (object-type->id 'project)))
                                                        (:and (:= 'object (dm:field object "track")) (:= 'object-type (object-type->id 'track)))
                                                        (:and (:= 'object (dm:id object)) (:= 'object-type (object-type->id 'entry))))
                                                   (:/= 'user (user:id author)))))
                                  (T (db:query (:and (:and (:= 'object (dm:id project)) (:= 'object-type (object-type->id 'project)))
                                                     (:/= 'user (user:id author))))))
                (lambda (record)
                  (when (< 0 (logand mask (gethash "type" record)))
                    (let ((email (user:field "email" (gethash "user" record))))
                      (when email (mail:send email subject body))))))))

(defun set-subscription (object types &optional (user (auth:current)))
  (let ((existing (dm:get-one 'subscriber (db:query (:and (:= 'object (dm:id object))
                                                          (:= 'object-type (object-type->id (dm:collection object)))
                                                          (:= 'user (user:id user))))))
        (types (subscription-types->id types)))
    (cond (existing
           (cond ((= 0 (dm:field existing "type"))
                  (dm:delete existing))
                 (T
                  (setf (dm:field existing "type") types)
                  (dm:save existing))))
          ((/= 0 types)
           (db:insert 'subscriber `(("object" . ,(dm:id object))
                                    ("object-type" . ,(object-type->id (dm:collection object)))
                                    ("user" . ,(user:id user))
                                    ("type" . ,(subscription-types->id types))))))))

(defun subscribe-to (object types &optional (user (auth:current)))
  (let ((existing (dm:get-one 'subscriber (db:query (:and (:= 'object (dm:id object))
                                                          (:= 'object-type (object-type->id (dm:collection object)))
                                                          (:= 'user (user:id user)))))))
    (cond (existing
           (setf (dm:field existing "type") (logior (dm:field existing "type") (subscription-types->id types)))
           (dm:save existing))
          (T
           (db:insert 'subscriber `(("object" . ,(dm:id object))
                                    ("object-type" . ,(object-type->id (dm:collection object)))
                                    ("user" . ,(user:id user))
                                    ("type" . ,(subscription-types->id types))))))))

(defun unsubscribe-from (object types &optional (user (auth:current)))
  (let ((existing (dm:get-one 'subscriber (db:query (:and (:= 'object (dm:id object))
                                                          (:= 'object-type (object-type->id (dm:collection object)))
                                                          (:= 'user (user:id user)))))))
    (when existing
      (setf (dm:field existing "type") (logand (dm:field existing "type") (lognot (subscription-types->id types))))
      (if (= 0 (dm:field existing "type"))
          (dm:delete existing)
          (dm:save existing)))))

(defun list-subscriptions (object)
  (if (typep object 'user:user)
      (dm:get 'subscriber (db:query (:= 'user (user:id object))))
      (dm:get 'subscriber (db:query (:and (:= 'object (dm:id object))
                                          (:= 'object-type (object-type->id (dm:collection object))))))))

(defun subscription-object (subscription)
  (ensure-object (dm:field subscription "object-type") (dm:field subscription "object")))
