(in-package #:feedback)

(defun normalize-field (field)
  (with-stringcase field
    (("project" "package") :project)
    (("status" "state") :status)
    (("version") :version)
    (("user-id" "reporter" "author" "reported by") :user-id)
    (("order" "#") :order)
    (("time" "date") :time)
    (("os-type" "os" "operating system" "system") :os-type)
    (("cpu-type" "cpu" "processor") :cpu-type)
    (("gpu-type" "gpu" "graphics") :gpu-type)
    (("title" "description" "text") :description)
    (("comment" "note") :notes)
    (("assigned") :assigned-to)
    (("category" "tag" "track") :tags)
    (("severity" "criticality" "importance") :severity)))

(defgeneric translate-field (type value project))

(defmethod translate-field ((type (eql :project)) value project)
  project)

(defmethod translate-field ((type (eql :status)) value project)
  (with-stringcase value
    (("triaged") 1)
    (("resolved" "fixed" "done") 2)
    (("wontfix" "wont fix" "wont-fix") 3)
    (("invalid") 4)
    (("duplicate" "dupe" "duplicated") 5)
    (("unclear" "unknown" "clarify" "?" "n/a") 6)
    (("deleted" "removed") 7)
    (T 0)))

(defmethod translate-field ((type (eql :version)) value project)
  (shorten value :limit 64))

(defmethod translate-field ((type (eql :user-id)) value project)
  (loop for entry in (cl-ppcre:split "(\\n\\r?)+" value)
        collect (shorten entry :limit 64)))

(defmethod translate-field ((type (eql :order)) value project)
  (parse-integer value))

(defmethod translate-field ((type (eql :time)) value project)
  (let ((ts (local-time:parse-timestring value :fail-on-error NIL)))
    (cond (ts
           (local-time:timestamp-to-universal ts))
          ((every #'digit-char-p value)
           (unix-to-universal-time (parse-integer value)))
          (T
           (get-universal-time)))))

(defmethod translate-field ((type (eql :os-type)) value project)
  (with-stringcase value
    (("win" "windows" "ms") :windows)
    (("lin" "linux") :linux)
    (("mac" "osx" "os x" "darwin") :macos)
    (("bsd" "freebsd") :bsd)
    (("android") :android)
    (("ios" "i os") :ios)
    (T :unknown)))

(defmethod translate-field ((type (eql :cpu-type)) value project)
  (with-stringcase value
    (("amd64" "x86_64" "amd 64" "x86 64") :amd64)
    (("i386" "i686") :i686)
    (("arm64") :arm64)
    (("arm" "armv7l" "armv7") :armv7l)
    (T :unknown)))

(defmethod translate-field ((type (eql :gpu-type)) value project)
  (with-stringcase value
    (("intel") :intel)
    (("nvidia") :nvidia)
    (("amd" "ati") :amd)
    (("vmware" "vmw") :vmware)
    (("virtualbox" "vbox") :virtualbox)
    (T :unknown)))

(defmethod translate-field ((type (eql :description)) value project)
  (let ((value (string-trim '(#\Linefeed #\Return #\Space #\Tab) value)))
    (when (string/= "" value) value)))

(defmethod translate-field ((type (eql :notes)) value project)
  (let ((value (string-trim '(#\Linefeed #\Return #\Space #\Tab) value)))
    (when (string/= "" value) value)))

(defmethod translate-field ((type (eql :tags)) value project)
  (dm:get-one 'tag (db:query (:and (:= 'name (string-downcase value))
                                   (:= 'project (dm:id project))))))

(defmethod translate-field ((type (eql :assigned-to)) value project)
  (user:get value))

(defmethod translate-field ((type (eql :severity)) value project)
  (with-stringcase value
    (("irrelevant") 1)
    (("nice to have") 10)
    (("low") 30)
    (("medium" "moderate") 50)
    (("high" "severe") 80)
    (("critical") 100)
    (T 0)))

(defun parse-entry (values keys project)
  (let ((table (make-hash-table :test 'eql)))
    (loop for raw in values
          for key in keys
          for value = (translate-field key raw project)
          do (if (listp value)
                 (loop for v in value do (push v (gethash key table)))
                 (push value (gethash key table))))
    (loop for k being the hash-keys of table using (hash-value v)
          collect k
          collect (case k
                    (:description
                     (format NIL "~{~a~^~%~%~}" v))
                    ((:notes :tags)
                     v)
                    (:user-id
                     v)
                    (T
                     (typecase (first v)
                       (dm:data-model (dm:id (first v)))
                       (T (first v))))))))

(defun realize-entries (data &optional (project (getf data :project)))
  (let ((notes (getf data :notes))
        (user-id (first (getf data :user-id))))
    (remf data :notes)
    (remf data :project)
    (let ((entry (apply #'make-entry project :user-id user-id data))
          (anon (user:get "anonymous")))
      (dolist (text notes)
        (make-note entry text :author anon))
      (dolist (user-id (rest (getf data :user-id)))
        (apply #'make-entry project :user-id user-id :status :duplicate :relates-to (dm:id entry) data))
      entry)))

(defun parse-entry-csv (csv project)
  (etypecase csv
    (pathname
     (with-open-file (stream csv)
       (parse-entry-csv stream project)))
    ((or string stream)
     (destructuring-bind (header . entries) (cl-csv:read-csv csv)
       (let ((fields (mapcar #'normalize-field header)))
         (loop for data in entries
               for parsed = (parse-entry data fields project)
               when (getf parsed :description)
               collect parsed))))))

(defun import-entry-csv (csv project &rest entry-args)
  (db:with-transaction ()
    (dolist (entry (parse-entry-csv csv project))
      (with-simple-restart (continue "Ignore the failed entry")
        (realize-entries (append entry-args entry) project)))))

;; TODO: allow creating tags on demand
