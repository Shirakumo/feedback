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

  (db:create 'member
             '((project (:id project))
               (user :id))
             :indices '(project user))

  (db:create 'track
             '((project (:id project))
               (name (:varchar 32))
               (description :text))
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
               (comment :text)
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
             :indices '(entry)))

(defmacro define-mapping ((a b) &body mappings)
  `(progn
     (defun ,(intern (format NIL "~a~a" (string a) (string 's))) ()
       ',(mapcar #'first mappings))
     (defun ,(intern (format NIL "~a->~a" (string a) (string b))) (thing)
       (cond ,@(loop for (a b) in mappings
                     collect `((equalp thing ,(string a)) ,b))
             (T (ecase thing
                  ,@(loop for (a b) in mappings
                          collect `((,a ,b) ,b))))))
     (defun ,(intern (format NIL "~a->~a" (string b) (string a))) (thing)
       (ecase thing
         ,@(mapcar #'reverse mappings)
         ,@(loop for (a b) in mappings
                 collect (list a a))))))

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
    ((:duplicate 5) "fa-copy")
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
       ((attachment entry note snapshot member)
        (dm:get-one 'project (db:query (:= '_id (dm:field project-ish "project")))))))
    (T
     (or (dm:get-one 'project (db:query (:= '_id (ensure-id project-ish))))
         (error 'request-not-found :message "Could not find the requested project.")))))

(defun find-project (name)
  (dm:get-one 'project (db:query (:= 'name name))))

(defun list-projects (&optional (member (auth:current NIL)))
  (if member
      (mapcar #'ensure-project (dm:get 'member (db:query (:= 'user (user:id member)))))
      (dm:get 'project (db:query :all))))

(defun make-project (name &key description trace-data-type attachments (author (auth:current)))
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
      (db:insert 'member `(("project" . ,(dm:id model)) ("user" . ,(user:id author))))
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
        (db:remove 'member (db:query (:= 'project (dm:id project))))
        (dolist (member members)
          (db:insert 'member `(("project" . ,(dm:id project)) ("user" . ,(user:id member))))))
      project)))

(defun delete-project (project)
  (db:with-transaction ()
    (let ((project (ensure-project project)))
      (mapc #'delete-entry (list-entries project))
      (mapc #'delete-snapshot (list-snapshots project))
      (dm:delete project)
      (delete-directory (project-directory project)))))

(defun list-members (project)
  (db:iterate 'member (db:query (:= 'project (ensure-id project)))
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
       (entry entry-ish)))
    (T
     (or (dm:get-one 'entry (db:query (:= '_id (ensure-id entry-ish))))
         (error 'request-not-found :message "Could not find the requested entry.")))))

(defun list-entries (&optional project &key (skip 0) (amount 50))
  (dm:get 'entry (if project
                     (db:query (:= 'project (ensure-id project)))
                     (db:query :all))
          :skip skip :amount amount :sort '(("time" :desc))))

(defun make-entry (project &key (time (get-universal-time)) user-id
                                os-type os-info
                                cpu-type cpu-info
                                gpu-type gpu-info
                                version description
                                assigned-to severity
                                relates-to)
  (let ((project (ensure-project project))
        (model (dm:hull 'entry)))
    (setf-dm-fields model project version time user-id os-info cpu-info gpu-info description severity relates-to)
    (setf (dm:field model "assigned-to") (when assigned-to (user:id assigned-to)))
    (setf (dm:field model "status") (status->id :new))
    (setf (dm:field model "os-type") (os-type->id (or os-type :unknown)))
    (setf (dm:field model "cpu-type") (cpu-type->id (or cpu-type :unknown)))
    (setf (dm:field model "gpu-type") (gpu-type->id (or gpu-type :unknown)))
    (dm:insert model)))

(defun edit-entry (entry &key user-id description comment status (assigned-to NIL assigned-p) severity relates-to)
  (let ((entry (ensure-entry entry)))
    (setf-dm-fields entry user-id description comment severity relates-to)
    (when assigned-p
      (setf (dm:field entry "assigned-to") (when assigned-to (user:id assigned-to))))
    (setf (dm:field entry "status") (status->id status))
    (dm:save entry)
    entry))

(defun delete-entry (entry)
  (db:with-transaction ()
    (let ((entry (ensure-entry entry)))
      (delete-directory (entry-directory entry))
      (db:remove 'note (db:query (:= 'entry (dm:id entry))))
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
    (dm:insert model)))

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
    (dm:save note)
    note))

(defun delete-note (note)
  (db:with-transaction ()
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
    (dm:insert model)))

(defun delete-snapshot (snapshot)
  (db:with-transaction ()
    (let ((snapshot (ensure-snapshot snapshot)))
      (delete-directory (snapshot-directory snapshot))
      (dm:delete snapshot))))
