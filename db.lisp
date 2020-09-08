(in-package #:feedback)

(define-trigger db:connected ()
  (db:create 'project
             '((name (:varchar 32))
               (description :text)))
  
  (db:create 'attachment
             '((project (:id project))
               (name (:varchar 32))
               (type (:integer 1))))

  (db:create 'entry
             '((project (:id project))
               (user-id (:varchar 64))
               (time (:integer 5))
               (os-type (:integer 1))
               (cpu-type (:integer 1))
               (gpu-type (:integer 1))
               (os-info :text)
               (cpu-info :text)
               (gpu-info :text)
               (description :text))))

(defmacro define-mapping ((a b) &body mappings)
  `(progn
     (defun ,(intern (format NIL "~a->~a" (string a) (string b))) (thing)
       (cond ,@(loop for (a b) in mappings
                     collect `((string-equal thing ,(string a)) ,b))))
     (defun ,(intern (format NIL "~a->~a" (string b) (string a))) (thing)
       (ecase thing
         ,@(mapcar #'reverse mappings)))))

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

(defun project-directory (project)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(princ-to-string (dm:field entry "project"))))
   (environment-module-pathname #.*package* :data "attachments/")))

(defun entry-directory (entry)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(princ-to-string (dm:field entry "project"))
                                         ,(princ-to-string (dm:id entry))))
   (environment-module-pathname #.*package* :data "attachments/")))

(defun attachment-pathname (entry type)
  (make-pathname :name (dm:field type "name")
                 :type (string-downcase (id->attachment-type (dm:field type "type")))
                 :defaults (entry-directory entry)))

(defun ensure-project (project-ish)
  (typecase project-ish
    (dm:data-model
     (ecase (dm:collection project-ish)
       (project project-ish)
       ((attachment entry)
        (dm:get-one 'project (db:query (:= '_id (dm:field project-ish "project")))))))
    (T
     (dm:get-one 'project (db:query (:= '_id (ensure-id project-ish)))))))

(defun find-project (name)
  (dm:get-one 'project (db:query (:= 'name name))))

(defun list-projects ()
  (dm:get 'project))

(defun make-project (name &key description attachments)
  (db:with-transaction ()
    (when (find-project name)
      (error "Project named ~s already exists" name))
    (let ((model (dm:hull 'project)))
      (setf-dm-fields model name description)
      (dm:insert model)
      (loop for (name type) in attachments
            for sub = (dm:hull 'attachment)
            do (setf-dm-fields sub (model "project") name)
               (setf (dm:field sub "type") (attachment-type->id type))
               (dm:insert sub))
      model)))

(defun edit-project (project &key name description (attachments NIL attachments-p))
  (db:with-transaction ()
    (let ((project (ensure-project project)))
      (setf-dm-fields project name description)
      (when attachments-p
        (let ((existing (list-attachments project)))
          (loop for (name type) in attachments
                for entry = (find name existing :key (lambda (n) (dm:field n "name"))
                                                :test #'string-equal)
                do (cond (entry
                          (setf (dm:field sub "type") (attachment-type->id type))
                          (setf existing (delete entry existing)))
                         (T
                          (let ((sub (dm:hull 'attachment)))
                            (setf-dm-fields sub (model "project") name)
                            (setf (dm:field sub "type") (attachment-type->id type))
                            (dm:insert sub)))))
          (dolist (attachment existing)
            (dm:delete attachment))))
      project)))

(defun delete-project (project)
  (db:with-transaction ()
    (let ((project (ensure-project project)))
      (mapc #'delete-entry (list-entries project))
      (dm:delete project)
      (delete-directory (project-directory project)))))

(defun list-attachments (project)
  (dm:get 'attachment (db:query (:= 'project (ensure-id project)))))

(defun ensure-entry (entry-ish)
  (etypecase entry-ish
    (dm:data-model
     (ecase (dm:collection entry-ish)
       (entry entry-ish)))
    (T
     (dm:get-one 'project (db:query (:= '_id (ensure-id entry-ish)))))))

(defun list-entries (project)
  (dm:get 'entry (db:query (:= 'project (ensure-id project)))))

(defun make-entry (project &key (time (get-universal-time)) user-id
                                os-type os-info
                                cpu-type cpu-info
                                gpu-type gpu-info
                                description)
  (let ((model (dm:hull 'entry)))
    (setf-dm-fields model project time user-id os-type os-info cpu-type cpu-info gpu-type gpu-info description)
    (dm:insert model)))

(defun delete-entry (entry)
  (db:with-transaction ()
    (let ((entry (ensure-entry entry)))
      (delete-directory (entry-directory entry))
      (dm:delete entry))))
