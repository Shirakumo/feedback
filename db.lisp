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

  (db:create 'tag
             '((project (:id project))
               (name (:varchar 32))
               (color (:integer 3))))

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

  (db:create 'entry-tag
             '((entry (:id entry))
               (tag (:id tag)))
             :indices '(entry))

  (db:create 'dependency
             '((target (:id entry))
               (source (:id entry)))
             :indices '(target source))
  
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
               (type (:integer 2))))

  (db:create 'timeline
             '((project (:id project))
               (name (:varchar 32))
               (description :text)
               (protection (:integer 1))
               (start (:integer 5))
               (end (:integer 5)))
             :indices '(project))

  (db:create 'event
             '((timeline (:id timeline))
               (entry (:id entry))
               (start (:integer 5))
               (end (:integer 5))
               (layer (:integer 2)))
             :indices '(timeline))

  (db:create 'deadline
             '((timeline (:id timeline))
               (name (:varchar 32))
               (time (:integer 5)))
             :indices '(timeline))

  (db:create 'changelog
             '((time (:integer 5))
               (author :id)
               (object :id)
               (object-type (:integer 2))
               (parent :id)
               (parent-type (:integer 2))
               (edit-type (:integer 2)))
             :indices '(time object object-type parent parent-type)))

(defun check-name (name)
  (let ((names '("new" "snapshot" "edit" "entry" "user" "subscribe" "import" "timeline" "tl")))
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
  (:bsd 4)
  (:android 5)
  (:ios 6))

(define-mapping (cpu-type id)
  (:unknown 0)
  (:amd64 1)
  (:i686 2)
  (:arm64 3)
  (:armv7l 4)
  (:riscv 5))

(define-mapping (gpu-type id)
  (:unknown 0)
  (:intel 1)
  (:nvidia 2)
  (:amd 3)
  (:vmware 4)
  (:virtualbox 5)
  (:virgl 6))

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
  (:xml 9)
  (:yaml 10)
  (:toml 11)
  (:mp4 12)
  (:ogv 13)
  (:mp3 14)
  (:wav 15))

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

(define-mapping (object-type id)
  (project 0)
  (track 1)
  (entry 2)
  (note 3)
  (members 4)
  (snapshot 5)
  (attachment 6)
  (subscriber 7)
  (timeline 8)
  (event 9)
  (deadline 10)
  (dependency 11)
  (changelog 12)
  (tag 13))

(define-mapping (edit-type id)
  (:make 0)
  (:edit 1)
  (:move 2)
  (:delete 3))

(defun edit-type-label (edit-type)
  (case edit-type
    ((:make 0) "created")
    ((:edit 1) "edited")
    ((:move 2) "moved")
    ((:delete 3) "deleted")
    (T "changed")))

(defun os-type-icon (os-type)
  (case os-type
    ((:windows 1) "fa-windows")
    ((:linux 2) "fa-linux")
    ((:macos 3) "fa-apple")
    ((:bsd 4) "fa-freebsd")
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

(defun attachment-type-icon (attachment-type)
  (case attachment-type
    ((:png 1 :jpg 2) "fa-file-image")
    ((:txt 3 :log 4) "fa-file-lines")
    ((:zip 5) "fa-file-zipper")
    ((:sig 6) "fa-file-signature")
    ((:csv 7) "fa-file-csv")
    ((:json 8 :xml 9 :yaml 10 :toml 11) "fa-file-code")
    ((:mp4 12 :ogv 13) "fa-file-video")
    ((:mp3 14 :wav 15) "fa-file-audio")
    (T "fa-file")))

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
    ((:yaml 10) "text/vnd.yaml")
    ((:toml 11) "application/toml")
    ((:mp4 12) "video/mp4")
    ((:ogv 13) "video/ogv")
    ((:mp3 14) "audio/mp3")
    ((:wav 15) "audio/wav")
    (T "application/octet-stream")))

(defun attachment-image-p (attachment-type)
  (case attachment-type
    ((:png 1 :jpg 2) T)))

(defun attachment-text-p (attachment-type)
  (case attachment-type
    ((:txt 3 :log 4) T)))

(defun attachment-code-p (attachment-type)
  (case attachment-type
    ((:csv 7 :json 8 :xml 9 :yaml 10 :toml 11) T)))

(defun attachment-video-p (attachment-type)
  (case attachment-type
    ((:mp4 12 :ogv 13) T)))

(defun attachment-audio-p (attachment-type)
  (case attachment-type
    ((:mp3 14 :wav 15) T)))

(defun attachment-type-kind (attachment-type)
  (case attachment-type
    ((:png 1 :jpg 2) :image)
    ((:txt 3 :log 4) :text)
    ((:csv 7 :json 8 :xml 9 :yaml 10 :toml 11) :code)
    ((:mp4 12 :ogv 13) :video)
    ((:mp3 14 :wav 15) :audio)
    (T :data)))

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
    (when (logbitp 8 int) (push :timeline-new types))
    (when (logbitp 9 int) (push :timeline-edit types))
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
       (when (member :timeline-new types :test #'string-equal) (setf (ldb (byte 1 8) int) 1))
       (when (member :timeline-edit types :test #'string-equal) (setf (ldb (byte 1 9) int) 1))
       int))))

(defun id->protection (int)
  (let ((types ()))
    (when int
      (when (logbitp 0 int) (push :registered-view types))
      (when (logbitp 1 int) (push :registered-add types))
      (when (logbitp 2 int) (push :registered-edit types))
      (when (logbitp 3 int) (push :anonymous-view types))
      (when (logbitp 4 int) (push :anonymous-add types))
      (when (logbitp 5 int) (push :anonymous-edit types)))
    types))

(defun protection->id (types)
  (etypecase types
    (integer types)
    (symbol (protection->id (list types)))
    (list
     (let ((int 0))
       (when (member :registered-view types :test #'string-equal) (setf (ldb (byte 1 0) int) 1))
       (when (member :registered-add types :test #'string-equal) (setf (ldb (byte 1 1) int) 1))
       (when (member :registered-edit types :test #'string-equal) (setf (ldb (byte 1 2) int) 1))
       (when (member :anonymous-view types :test #'string-equal) (setf (ldb (byte 1 3) int) 1))
       (when (member :anonymous-add types :test #'string-equal) (setf (ldb (byte 1 4) int) 1))
       (when (member :anonymous-edit types :test #'string-equal) (setf (ldb (byte 1 5) int) 1))
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

(define-ensure parent (object)
  (ecase (dm:collection object)
    (project)
    ((attachment track tag snapshot members timeline)
     (ensure-project object))
    ((note entry-tag dependency)
     (ensure-entry object))
    ((event deadline)
     (ensure-timeline object))
    ((log subscriber)
     (ensure-object (dm:field object "object") (dm:field object "object-type")))
    (entry
     (if (dm:field object "track")
         (ensure-track object)
         (ensure-project object)))))

(defun log-change (object action &key (time (get-universal-time)) (author (author)))
  (let ((parent (parent object)))
    (db:insert 'changelog
               `(("time" . ,time)
                 ("author" . ,(user:id author))
                 ("object" . ,(dm:id object))
                 ("object-type" . ,(object-type->id (dm:collection object)))
                 ("parent" . ,(when parent (dm:id parent)))
                 ("parent-type" . ,(when parent (object-type->id (dm:collection parent))))
                 ("edit-type" . ,(edit-type->id action))))))

(defun changelog-object (object)
  (ensure-object (dm:field object "object-type") (dm:field object "object")))

(defun changelog-parent (object)
  (when (dm:field object "parent")
    (ensure-object (dm:field object "parent-type") (dm:field object "parent"))))

(defun list-changes (&key start end object author (skip 0) amount)
  (let ((start (or start 0)) (end (or end most-positive-fixnum)))
    (dm:get 'changelog 
            (cond (object
                   (db:query (:and (:>= 'time start) (:< 'time end)
                                   (:or (:and (:= 'object (dm:id object))
                                              (:= 'object-type (object-type->id (dm:collection object))))
                                        (:and (:= 'parent (dm:id object))
                                              (:= 'parent-type (object-type->id (dm:collection object))))))))
                  (author
                   (db:query (:and (:>= 'time start) (:< 'time end)
                                   (:= 'author (user:id author)))))
                  (T
                   (db:query (:and (:>= 'time start) (:< 'time end)))))
            :amount amount :skip skip :sort '(("time" :desc)))))

(define-ensure ensure-project (project-ish)
  (typecase project-ish
    (dm:data-model
     (ecase (dm:collection project-ish)
       (project project-ish)
       (note
        (ensure-project (ensure-entry project-ish)))
       ((event deadline)
        (ensure-project (ensure-timeline project-ish)))
       ((attachment track entry snapshot members timeline tag)
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

(defun make-project (name &key description trace-data-type attachments tags (author (auth:current)))
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
      (loop for (name color) in tags
            for sub = (dm:hull 'tag)
            when (or* name)
            do (setf-dm-fields sub (model "project") name color)
               (dm:insert sub))
      (db:insert 'members `(("project" . ,(dm:id model)) ("user" . ,(user:id author))))
      (log-change model :make :author author)
      (notify model :project-new model author)
      model)))

(defun edit-project (project &key name description trace-data-type (attachments NIL attachments-p) members (tags NIL tags-p))
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
                          (log-change entry :edit)
                          (setf existing (delete entry existing)))
                         ((or* name)
                          (let ((sub (dm:hull 'attachment)))
                            (setf-dm-fields sub project name)
                            (setf (dm:field sub "type") (attachment-type->id type))
                            (dm:insert sub)
                            (log-change sub :make)))))
          (dolist (attachment existing)
            (log-change attachment :delete)
            (dm:delete attachment))))
      (when members
        (db:remove 'members (db:query (:= 'project (dm:id project))))
        ;; FIXME: Notify for new members
        (dolist (member members)
          (db:insert 'members `(("project" . ,(dm:id project)) ("user" . ,(user:id member))))))
      (when tags-p
        (let ((existing (list-tags project)))
          (loop for (name color) in tags
                for entry = (find name existing :key (lambda (n) (dm:field n "name"))
                                                :test #'string-equal)
                do (cond (entry
                          (setf (dm:field entry "color") color)
                          (dm:save entry)
                          (log-change entry :edit)
                          (setf existing (delete entry existing)))
                         ((or* name)
                          (let ((sub (dm:hull 'tag)))
                            (setf-dm-fields sub project name color)
                            (dm:insert sub)
                            (log-change sub :make)))))
          (dolist (tag existing)
            (db:remove 'entry-tag (db:query (:= 'tag (dm:id tag))))
            (log-change tag :delete)
            (dm:delete tag))))
      (log-change project :edit)
      project)))

(defun delete-project (project)
  (db:with-transaction ()
    (let ((project (ensure-project project)))
      (mapc #'delete-entry (list-entries project))
      (mapc #'delete-snapshot (list-snapshots project))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id project)) (:= 'object-type (object-type->id 'project)))))
      (db:remove 'members (db:query (:= 'project (dm:id project))))
      (db:remove 'tag (db:query (:= 'project (dm:id project))))
      (log-change project :delete)
      (dm:delete project)
      (delete-directory (project-directory project)))))

(defun find-track (name project)
  (dm:get-one 'track (db:query (:and (:= 'name name) (:= 'project (ensure-id project))))))

(define-ensure ensure-track (track-ish)
  (etypecase track-ish
    (dm:data-model
     (ecase (dm:collection track-ish)
       (entry
        (dm:get-one 'track (db:query (:= '_id (dm:field track-ish "track")))))
       (track track-ish)))
    (T
     (or (dm:get-one 'track (db:query (:= '_id (ensure-id track-ish))))
         (error 'request-not-found :message "Could not find the requested track.")))))

(defun list-tags (object)
  (when (dm:id object)
    (ecase (dm:collection object)
      (project (dm:get 'tag (db:query (:= 'project (dm:id object))) :sort '(("name" :asc))))
      (entry (dm:get (rdb:join (tag _id) (entry-tag tag)) (db:query (:= 'entry (dm:id object))) :sort '(("name" :asc)))))))

(defun find-tag (name project)
  (dm:get-one 'tag (db:query (:and (:= 'name name) (:= 'project (ensure-id project))))))

(define-ensure ensure-tag (tag-ish &optional project)
  (etypecase tag-ish
    (dm:data-model
     (ecase (dm:collection tag-ish)
       (tag tag-ish)))
    (db:id
     (or (dm:get-one 'tag (db:query (:= '_id tag-ish)))
         (error 'request-not-found :message "Could not find the requested tag.")))
    (string
     (or (find-tag tag-ish project)
         (error 'request-not-found :message "Could not find the requested tag.")))))

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
    (when (find-track name project)
      (error "Track named ~s already exists" name))
    (setf-dm-fields track project name description)
    (setf (dm:field track "protection") (protection->id protection))
    (prog1 (dm:insert track)
      (log-change track :make)
      (notify track :track-new project))))

(defun edit-track (track &key name description (protection NIL protection-p))
  (db:with-transaction ()
    (let ((track (ensure-track track)))
      (setf-dm-fields track name description)
      (when protection-p
        (setf (dm:field track "protection") (protection->id protection)))
      (log-change track :edit)
      (dm:save track))))

(defun delete-track (track &key (delete-entries T))
  (db:with-transaction ()
    (let ((track (ensure-track track)))
      (if delete-entries
          (db:remove 'entry (db:query (:= 'track (dm:id track))))
          (db:update 'entry (db:query (:= 'track (dm:id track))) `(("track" . NIL))))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id track)) (:= 'object-type (object-type->id 'track)))))
      (db:remove 'track (db:query (:= '_id (dm:id track))))
      (log-change track :delete))))

(defun list-members (project)
  (db:iterate 'members (db:query (:= 'project (ensure-id project)))
    (lambda (row) (user:get (gethash "user" row)))
    :accumulate T :fields '("user")))

(defun list-attachments (thing)
  (dm:get 'attachment (db:query (:= 'project (if (typep thing 'db:id) thing (dm:id (ensure-project thing))))) :sort '(("name" :asc))))

(define-ensure ensure-attachment (project name)
  (or (dm:get-one 'attachment (db:query (:and (:= 'project (ensure-id project))
                                              (:= 'name name))))
      (error 'request-not-found :message "Could not find the requested attachment.")))

(define-ensure ensure-entry (entry-ish)
  (etypecase entry-ish
    (dm:data-model
     (ecase (dm:collection entry-ish)
       (entry entry-ish)
       ((note entry-tag)
        (dm:get-one 'entry (db:query (:= '_id (dm:field entry-ish "entry")))))
       (dependency
        (dm:get-one 'entry (db:query (:= '_id (dm:field entry-ish "source")))))))
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

(defun list-untracked-entries (project &key (skip 0) (amount 50) query)
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
    (dm:get 'entry (query (:and (:= 'project (dm:id project)) (:null 'track)))
            :skip skip :amount amount :sort '(("order" :desc) ("time" :desc)))))

(defmacro %entry-query (parent-form &rest query)
  (let ((parent 'parent))
    `(let ((,parent ,parent-form))
       (flet ((track (id) (db:query (:and (:= 'track id) ,@query)))
              (project (id) (db:query (:and (:= 'project id) ,@query))))
         (ecase (dm:collection ,parent)
           (entry (if (dm:field ,parent "track")
                      (track (dm:field ,parent "track"))
                      (project (dm:field ,parent "project"))))
           (track (track (dm:id ,parent)))
           (project (project (dm:id ,parent))))))))

(defun make-entry (project &key (time (get-universal-time))
                                (user-id (user:username (author)))
                                track status
                                os-type os-info
                                cpu-type cpu-info
                                gpu-type gpu-info
                                version description
                                assigned-to severity
                                relates-to tags)
  (db:with-transaction ()
    (let ((project (ensure-project project))
          (track (when track (ensure-track track)))
          (relates-to (when relates-to (ensure-entry relates-to)))
          (model (dm:hull 'entry))
          (severity (or severity 0))
          (status (or status 0)))
      (let ((highest (dm:get-one 'entry (%entry-query (or track project)) :sort `(("order" :desc)))))
        (cond ((null highest)
               (setf (dm:field model "order") 0))
              ((and (equalp (dm:field highest "description") description)
                    (equalp (dm:field highest "user-id") user-id))
               (error 'api-argument-invalid :argument "description" :message "Duplicate entry"))
              (T
               (setf (dm:field model "order") (1+ (dm:field highest "order"))))))
      (let ((duplicate (dm:get-one 'entry (%entry-query (or track project) (:= 'description description) (:/= 'status (status->id :duplicate))))))
        (when duplicate
          (setf status :duplicate)
          (setf relates-to (dm:id duplicate))))
      (setf-dm-fields model project track version time user-id os-info cpu-info gpu-info description severity relates-to)
      (setf (dm:field model "assigned-to") (when assigned-to (user:id assigned-to)))
      (setf (dm:field model "status") (status->id (or status :new)))
      (setf (dm:field model "os-type") (os-type->id (or os-type :unknown)))
      (setf (dm:field model "cpu-type") (cpu-type->id (or cpu-type :unknown)))
      (setf (dm:field model "gpu-type") (gpu-type->id (or gpu-type :unknown)))
      ;; Reorder to bottom
      (unless (find (id->status (dm:field model "status")) '(:new :triaged :unclear))
        (let ((bottom (dm:get-one 'entry (%entry-query (or track project) (:<= 'status 1)) :sort `(("order" :asc)))))
          (when bottom
            (setf bottom (dm:field bottom "order"))
            (loop for record in (db:select 'entry (%entry-query (or track project) (:and (:<= bottom 'order))))
                  do (db:update 'entry (db:query (:= '_id (gethash "_id" record))) `(("order" . ,(1+ (gethash "order" record))))))
            (setf (dm:field model "order") bottom))))
      (prog1 (dm:insert model)
        (dolist (tag tags)
          (db:insert 'entry-tag `(("entry" . ,(dm:id model)) ("tag" . ,(dm:id (ensure-tag tag project))))))
        (unless (eql status :duplicate)
          (notify model :entry-new project))
        (log-change model :make)))))

(defun edit-entry (entry &key track user-id description status (assigned-to NIL assigned-p) (relates-to NIL relates-to-p) severity order (tags NIL tags-p))
  (db:with-transaction ()
    (let ((entry (ensure-entry entry)))
      (when (changes-p entry user-id description status assigned-to relates-to severity)
        (log-change entry :edit))
      (when (changes-p entry track)
        (log-change entry :move))
      (setf-dm-fields entry track user-id description severity relates-to)
      (when relates-to-p
        (setf (dm:field entry "assigned-to") (when relates-to (ensure-entry relates-to))))
      (when assigned-p
        (setf (dm:field entry "assigned-to") (when assigned-to (user:id assigned-to))))
      (when status
        (setf (dm:field entry "status") (status->id status)))
      (when (and tags-p (listp tags))
        ;; FIXME: track tags changes
        (db:remove 'entry-tag (db:query (:= 'entry (dm:id entry))))
        (dolist (tag tags)
          (db:insert 'entry-tag `(("entry" . ,(dm:id entry)) ("tag" . ,(dm:id (ensure-tag tag (dm:field entry "project"))))))))
      (when order
        (dm:save entry)
        (let* ((prev (dm:field entry "order"))
               (order (case order
                        (:top (dm:field (dm:get-one 'entry (%entry-query entry) :sort `(("order" :desc))) "order"))
                        (:bottom (dm:field (dm:get-one 'entry (%entry-query entry (:<= 'status 1)) :sort `(("order" :asc))) "order"))
                        (T order))))
          ;; FIXME: this sucks. But we can't do better with the standard interface.
          (cond ((< prev order)
                 (loop for record in (db:select 'entry (%entry-query entry (:and (:< prev 'order) (:<= 'order order))))
                       do (db:update 'entry (db:query (:= '_id (gethash "_id" record))) `(("order" . ,(1- (gethash "order" record)))))))
                ((< order prev)
                 (loop for record in (db:select 'entry (%entry-query entry (:and (:<= order 'order) (:< 'order prev))))
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
      (db:remove 'entry-tag (db:query (:= 'entry (dm:id entry))))
      (db:remove 'dependency (db:query (:or (:= 'target (dm:id entry)) (:= 'source (dm:id entry)))))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id entry)) (:= 'object-type (object-type->id 'entry)))))
      (log-change entry :delete)
      (dm:delete entry))))

(defun list-related (entry)
  (dm:get 'entry (db:query (:= 'relates-to (dm:id entry)))))

(defun list-notes (entry &key (skip 0) (amount 100))
  (dm:get 'note (db:query (:= 'entry (dm:id entry)))
          :skip skip :amount amount :sort '(("time" :asc))))

(defun make-note (entry text &key (author (author)) (time (get-universal-time)))
  (let ((entry (ensure-entry entry))
        (model (dm:hull 'note)))
    (setf-dm-fields model entry text time)
    (setf (dm:field model "author") (user:id author))
    (prog1 (dm:insert model)
      (log-change model :make)
      (notify model :note-new))))

(define-ensure ensure-note (note-ish)
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
      (log-change note :edit)
      (notify note :note-edit))))

(defun delete-note (note)
  (db:with-transaction ()
    (db:remove 'subscriber (db:query (:and (:= 'object (dm:id note)) (:= 'object-type (object-type->id 'note)))))
    (log-change note :delete)
    (dm:delete note)))

(defun list-depends-on (entry)
  (dm:get (rdb:join (entry _id) (dependency target)) (db:query (:= 'source (ensure-id entry)))
          :sort '(("order" :desc) ("time" :desc))))

(defun list-depended-by (entry)
  (dm:get (rdb:join (entry _id) (dependency source)) (db:query (:= 'target (ensure-id entry)))
          :sort '(("order" :desc) ("time" :desc))))

(defun add-dependency (target source)
  (db:with-transaction ()
    (when (= 0 (db:count 'dependency (db:query (:and (:= 'source (ensure-id source)) (:= 'target (ensure-id target))))))
      (let ((dependency (dm:hull 'dependency)))
        (setf-dm-fields dependency source target)
        (dm:insert dependency)
        (log-change dependency :make)))))

(defun remove-dependency (target source)
  (let ((dependency (dm:get-one 'dependency (db:query (:and (:= 'source (ensure-id source)) (:= 'target (ensure-id target)))))))
    (log-change dependency :delete)
    (dm:delete dependency)))

(define-ensure ensure-snapshot (snapshot-ish)
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
      (log-change model :make)
      (notify model :snapshot-new project))))

(defun delete-snapshot (snapshot)
  (db:with-transaction ()
    (let ((snapshot (ensure-snapshot snapshot)))
      (delete-directory (snapshot-directory snapshot))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id snapshot)) (:= 'object-type (object-type->id 'snapshot)))))
      (log-change snapshot :delete)
      (dm:delete snapshot))))

(define-ensure ensure-object ((type id))
  (ecase (id->object-type (object-type->id type))
    (project (ensure-project id))
    (track (ensure-track id))
    (entry (ensure-entry id))
    (note (ensure-note id))
    (members (dm:get-one 'members (db:query (:= '_id id))))
    (snapshot (ensure-snapshot id))
    (attachment (dm:get-one 'attachment (db:query (:= '_id id))))
    (subscriber (dm:get-one 'subscriber (db:query (:= '_id id))))
    (timeline (ensure-timeline id))
    (event (ensure-event id))
    (deadline (ensure-deadline id))
    (dependency (dm:get-one 'dependency (db:query (:= '_id id))))
    (changelog (dm:get-one 'changelog (db:query (:= '_id id))))
    (tag (ensure-tag id))))

(defun notify (object type &optional (project (ensure-project object)) (author (author)))
  (let* ((type-template
           (ecase type
             (:entry-new (@template "mail-entry-new.ctml"))
             (:entry-edit (@template "mail-entry-edit.ctml"))
             (:note-new (@template "mail-note-new.ctml"))
             (:note-edit (@template "mail-note-edit.ctml"))
             (:track-new (@template "mail-track-new.ctml"))
             (:project-new (@template "mail-project-new.ctml"))
             (:snapshot-new (@template "mail-snapshot-new.ctml"))
             (:member-new (@template "mail-member-new.ctml"))
             (:timeline-new (@template "mail-timeline-new.ctml"))
             (:timeline-edit (@template "mail-timeline-edit.ctml"))))
         (rendered
           (let ((*package* #.*package*))
             (clip:process type-template :project project :object object :author author)))
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

(defun make-subscription (object types &optional (user (auth:current)))
  (let ((subscriber (dm:hull 'subscriber)))
    (setf (dm:field subscriber "object") (dm:id object))
    (setf (dm:field subscriber "object-type") (object-type->id (dm:collection object)))
    (setf (dm:field subscriber "user") (user:id user))
    (setf (dm:field subscriber "type") (subscription-types->id types))
    (dm:insert subscriber)
    (log-change subscriber :make :author user)))

(defun set-subscription (object types &optional (user (auth:current)))
  (let ((existing (dm:get-one 'subscriber (db:query (:and (:= 'object (dm:id object))
                                                          (:= 'object-type (object-type->id (dm:collection object)))
                                                          (:= 'user (user:id user))))))
        (types (subscription-types->id types)))
    (cond (existing
           (cond ((= 0 (dm:field existing "type"))
                  (log-change existing :delete)
                  (dm:delete existing))
                 (T
                  (setf (dm:field existing "type") types)
                  (log-change existing :edit)
                  (dm:save existing))))
          ((/= 0 types)
           (make-subscription object types user)))))

(defun subscribe-to (object types &optional (user (auth:current)))
  (let ((existing (dm:get-one 'subscriber (db:query (:and (:= 'object (dm:id object))
                                                          (:= 'object-type (object-type->id (dm:collection object)))
                                                          (:= 'user (user:id user)))))))
    (cond (existing
           (setf (dm:field existing "type") (logior (dm:field existing "type") (subscription-types->id types)))
           (log-change existing :edit)
           (dm:save existing))
          (T
           (make-subscription object types user)))))

(defun unsubscribe-from (object types &optional (user (auth:current)))
  (let ((existing (dm:get-one 'subscriber (db:query (:and (:= 'object (dm:id object))
                                                          (:= 'object-type (object-type->id (dm:collection object)))
                                                          (:= 'user (user:id user)))))))
    (when existing
      (setf (dm:field existing "type") (logand (dm:field existing "type") (lognot (subscription-types->id types))))
      (cond ((= 0 (dm:field existing "type"))
             (log-change existing :delete)
             (dm:delete existing))
            (T
             (log-change existing :edit)
             (dm:save existing))))))

(defun list-subscriptions (scope &optional (user (auth:current)))
  (let ((subs (dm:get 'subscriber (db:query (:= 'user (user:id user)))
                      :sort '(("object-type" :asc) ("object" :asc)))))
    (when scope
      (macrolet ((filter (&rest cases)
                   `(loop for sub in subs
                          for object = (dm:field sub "object")
                          when (case (id->object-type (dm:field sub "object-type"))
                                 ,@cases)
                          collect sub)))
        (ecase (dm:collection scope)
          (entry
           (filter (:entry (equal object (dm:id scope)))))
          (track
           (filter (:track (equal object (dm:id scope)))
                   (:entry (= 1 (db:count 'entry (db:query (:and (:= 'track (dm:id scope)) (:= '_id object))))))))
          (timeline
           (filter (:timeline (equal object (dm:id scope)))))
          (project
           (filter (:project (equal object (dm:id scope)))
                   (:track (= 1 (db:count 'track (db:query (:and (:= 'project (dm:id scope)) (:= '_id object))))))
                   (:timeline (= 1 (db:count 'timeline (db:query (:and (:= 'project (dm:id scope)) (:= '_id object))))))
                   (:entry (= 1 (db:count 'entry (db:query (:and (:= 'project (dm:id scope)) (:= '_id object)))))))))))))

(defun subscription-object (subscription)
  (ensure-object (dm:field subscription "object-type") (dm:field subscription "object")))

(defun find-timeline (name project)
  (dm:get-one 'timeline (db:query (:and (:= 'name name) (:= 'project (ensure-id project))))))

(defun list-timelines (project &key (skip 0) (amount 50) query)
  (dm:get 'timeline (if query
                        (db:query (:and (:= 'project (ensure-id project))
                                        (:MATCHES* 'name (cl-ppcre:quote-meta-chars query))))
                        (db:query (:= 'project (ensure-id project))))
          :skip skip :amount amount :sort '(("name" :desc))))

(define-ensure ensure-timeline (timeline-ish)
  (etypecase timeline-ish
    (dm:data-model
     (ecase (dm:collection timeline-ish)
       (timeline timeline-ish)
       ((event deadline)
        (dm:get-one 'timeline (db:query (:= '_id (dm:field timeline-ish "timeline")))))))
    (T
     (or (dm:get-one 'timeline (db:query (:= '_id (ensure-id timeline-ish))))
         (error 'request-not-found :message "Could not find the requested timeline.")))))

(defun make-timeline (project name &key description protection start end)
  (check-name name)
  (let ((project (ensure-project project))
        (timeline (dm:hull 'timeline)))
    (when (find-timeline name project)
      (error "Timeline named ~s already exists" name))
    (setf-dm-fields timeline project name description start end)
    (setf (dm:field timeline "protection") (protection->id protection))
    (prog1 (dm:insert timeline)
      (log-change timeline :make)
      (notify timeline :timeline-new project))))

(defun edit-timeline (timeline &key name description (protection NIL protection-p) start end)
  (db:with-transaction ()
    (let ((timeline (ensure-timeline timeline)))
      (setf-dm-fields timeline name description start end)
      (when protection-p
        (setf (dm:field timeline "protection") (protection->id protection)))
      (log-change timeline :edit)
      (dm:save timeline))))

(defun delete-timeline (timeline)
  (db:with-transaction ()
    (let ((timeline (ensure-timeline timeline)))
      (db:remove 'event (db:query (:= 'timeline (dm:id timeline))))
      (db:remove 'deadline (db:query (:= 'timeline (dm:id timeline))))
      (db:remove 'subscriber (db:query (:and (:= 'object (dm:id timeline)) (:= 'object-type (object-type->id 'timeline)))))
      (log-change timeline :delete)
      (db:remove 'timeline (db:query (:= '_id (dm:id timeline)))))))

(defvar *default-timeline-range* (* 60 60 24 30 4)) ; four months
(defun timeline-range (timeline)
  (let* ((timeline (ensure-timeline timeline))
         (start (dm:field timeline "start"))
         (end (dm:field timeline "end"))
         (layers 1))
    (let ((res (first (rdb:sql "SELECT MIN(\"start\"), MAX(\"end\"), MAX(\"layer\") FROM \"FEEDBACK/EVENT\" WHERE \"timeline\"=?;"
                               (dm:id timeline)))))
      (when res
        (unless start (setf start (gethash "MIN(start)" res)))
        (unless end (setf end (gethash "MIN(end)" res)))
        (setf layers (1+ (gethash "MAX(layer)" res 0)))))
    (unless start
      (setf start (if end
                      (- end *default-timeline-range*)
                      (get-universal-time))))
    (unless end
      (setf end (+ start *default-timeline-range*)))
    (list :start start :end end :layers layers)))

(defun list-events (timeline &key start end)
  (dm:get (rdb:join (entry _id) (event entry))
          (db:query (:and (:= 'timeline (ensure-id timeline))
                          (:> 'end (or start 0))
                          (:< 'start (or end most-positive-fixnum))))
          :sort '(("start" :asc))))

(define-ensure ensure-event (event-ish)
  (etypecase event-ish
    (dm:data-model
     (ecase (dm:collection event-ish)
       (event event-ish)))
    (T
     (or (dm:get-one 'event (db:query (:= '_id (ensure-id event-ish))))
         (error 'request-not-found :message "Could not find the requested event.")))))

(defun find-event (timeline entry)
  (dm:get-one 'event (db:query (:and (:= 'timeline (ensure-id timeline))
                                     (:= 'entry (ensure-id entry))))))

(defun make-event (timeline entry start end &key (layer 0))
  (let ((timeline (ensure-timeline timeline))
        (event (dm:hull 'event)))
    (when (find-event timeline entry)
      (error "Event for entry ~s already exists" (ensure-id entry)))
    (setf-dm-fields event entry start end layer)
    (prog1 (dm:insert event)
      (log-change event :make)
      (notify timeline :timeline-edit))))

(defun edit-event (event &key name start end layer)
  (db:with-transaction ()
    (let ((event (ensure-event event)))
      (setf-dm-fields event name start end layer)
      (prog1 (dm:save event)
        (log-change event :edit)
        (notify (ensure-timeline event) :timeline-edit)))))

(defun delete-event (event)
  (db:with-transaction ()
    (let ((event (ensure-event event)))
      (prog1 (dm:delete event)
        (log-change event :delete)
        (notify (ensure-timeline event) :timeline-edit)))))

(defun list-deadlines (timeline &key start end)
  (dm:get 'deadline (db:query (:and (:= 'timeline (ensure-id timeline))
                                    (:> 'time (or start 0))
                                    (:< 'time (or end most-positive-fixnum))))
          :sort '(("time" :asc))))

(define-ensure ensure-deadline (deadline-ish)
  (etypecase deadline-ish
    (dm:data-model
     (ecase (dm:collection deadline-ish)
       (deadline deadline-ish)))
    (T
     (or (dm:get-one 'deadline (db:query (:= '_id (ensure-id deadline-ish))))
         (error 'request-not-found :message "Could not find the requested deadline.")))))

(defun make-deadline (timeline name time)
  (let ((timeline (ensure-timeline timeline))
        (deadline (dm:hull 'deadline)))
    (setf-dm-fields deadline name time)
    (prog1 (dm:insert deadline)
      (log-change deadline :make)
      (notify timeline :timeline-edit))))

(defun edit-deadline (deadline &key name time)
  (db:with-transaction ()
    (let ((deadline (ensure-deadline deadline)))
      (setf-dm-fields deadline name time)
      (prog1 (dm:save deadline)
        (log-change deadline :edit)
        (notify deadline :timeline-edit)))))

(defun delete-deadline (deadline)
  (db:with-transaction ()
    (let ((deadline (ensure-deadline deadline)))
      (prog1 (dm:delete deadline)
        (log-change deadline :delete)
        (notify deadline :timeline-edit)))))

(defun accessible-p (object action &optional (user (author)))
  (flet ((project-accessible-p (project)
           (< 0 (db:count 'members (db:query (:and (:= 'project project) (:= 'user (user:id user))))))))
    (ecase (dm:collection object)
      (project
       (project-accessible-p (dm:id object)))
      ((track timeline)
       (let ((bit (ecase action (:view 0) (:add 1) (:edit 2))))
         (when (or (null user) (eq user (user:get "anonymous")))
           (setf bit (+ bit 3)))
         (or (logbitp bit (dm:field object "protection"))
             (project-accessible-p (dm:field object "project")))))
      (entry
       (if (dm:field object "track")
           (accessible-p (ensure-track object) action user)
           (project-accessible-p (dm:field object "project"))))
      ((event deadline)
       (accessible-p (ensure-timeline object) action user))
      (note
       (let ((entry (ensure-entry object)))
         (ecase action
           (:view (accessible-p entry action user))
           (:edit (or (and (= (dm:field object "author") (user:id user))
                           (not (eq (user:get "anonymous") user)))
                      (project-accessible-p (dm:field entry "project")))))))
      (T
       (project-accessible-p (dm:field object "project"))))))

(defun check-accessible (object action &optional (user (author)))
  (if (accessible-p object action user)
      object
      (error 'request-denied)))
