(in-package #:feedback)

(defun output (object message url-format &rest args)
  (let ((target (url> (apply #'format NIL url-format args)
                      :query `(("message" . ,message)))))
    (if (string= "true" (post/get "browser"))
        (redirect target)
        (api-output object :message message :target target))))

(define-api feedback/project/list () (:access (perm feedback project list))
  (api-output (list-projects)))

(define-api feedback/project/new (name &optional description attachment-name[] attachment-type[]) (:access (perm feedback project new))
  (output (make-project name :description description
                             :attachments (loop for name in attachment-name[]
                                                for type in attachment-type[]
                                                collect (list name type)))
          "Project created." (format NIL "feedback/~a" name)))

(define-api feedback/project/edit (project &optional name description attachment-name[] attachment-type[]) (:access (perm feedback project edit))
  (output (edit-project project :name name
                                :description description
                                :attachments (loop for name in attachment-name[]
                                                   for type in attachment-type[]
                                                   collect (list name type)))
          "Project edited." (format NIL "feedback/~a" name)))

(define-api feedback/project/delete (project) (:access (perm feedback project delete))
  (delete-project (ensure-project project))
  (output NIL "Project deleted." "feedback/"))

(define-api feedback/entry/list (project) (:access (perm feedback entry list))
  (api-output (list-entries (ensure-project project))))

(define-api feedback/entry/new (project user-id os-type os-info cpu-type cpu-info gpu-type gpu-info &optional description) (:access (perm feedback entry new))
  (db:with-transaction ()
    (let* ((project (ensure-project project))
           (types (list-attachments project))
           (entry (make-entry project :user-id user-id
                                      :os-type os-type
                                      :os-info os-info
                                      :cpu-type cpu-type
                                      :cpu-info cpu-info
                                      :gpu-type gpu-type
                                      :gpu-info gpu-info
                                      :description description)))
      (loop for type in types
            for file = (post-var (dm:field type "name"))
            do (when file
                 (uiop:copy-file file (attachment-pathname entry type))))
      (output entry "Feedback submitted." (format NIL "feedback/~a/~a" (dm:field project "name") (dm:id entry))))))

(define-api feedback/entry/delete (entry) (:access (perm feedback entry delete))
  (let ((entry (ensure-entry entry)))
    (delete-entry entry)
    (output NIL "Entry deleted." (format NIL "feedback/~a" (dm:field (ensure-project entry) "name")))))
