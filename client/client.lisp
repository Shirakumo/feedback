(in-package #:org.shirakumo.feedback.client)

(defclass client (north:client)
  ((api-base :reader api-base))
  (:default-initargs
   :api-base NIL
   :request-token-uri NIL
   :authorize-uri NIL
   :access-token-uri NIL))

(defmethod initialize-instance :after ((client client) &key api-base)
  (setf api-base (string-right-trim "/" api-base))
  (setf (slot-value client 'api-base) api-base)
  (unless (north:request-token-uri client)
    (setf (north:request-token-uri client) (format NIL "~a/oauth/request-token" api-base)))
  (unless (north:authorize-uri client)
    (setf (north:authorize-uri client) (format NIL "~a/oauth/authorize" api-base)))
  (unless (north:access-token-uri client)
    (setf (north:access-token-uri client) (format NIL "~a/oauth/access-token" api-base)))
  (unless (north:verify-uri client)
    (setf (north:verify-uri client) (format NIL "~a/oauth/verify" api-base))))

(defun decode-radiance-payload (data)
  (let ((json (com.inuoe.jzon:parse
               (etypecase data
                 (string data)
                 (vector (babel:octets-to-string data))
                 (stream data)))))
    (when (/= 200 (gethash "status" json))
      (error "Request failed: ~s" (gethash "message" json)))
    (gethash "data" json)))

(defmethod post ((client client) endpoint parameters)
  (decode-radiance-payload
   (north:make-signed-request client (format NIL "~a/~a" (api-base client) endpoint)
                              :post :params parameters)))

(defmethod post-file ((client client) endpoint data parameters)
  (decode-radiance-payload
   (north:make-signed-data-request client (format NIL "~a/~a" (api-base client) endpoint)
                                   data :params parameters)))

(defun login (key secret &key (api-base "https://feedback.tymoon.eu/api/"))
  (let* ((client (make-instance 'client :api-base api-base :key key :secret secret))
         (url (north:initiate-authentication client)))
    (format *query-io* "~&> ~a~%Token: " url)
    (finish-output *query-io*)
    (north:complete-authentication client (read-line *query-io*))
    (values client (north:token client) (north:token-secret client))))

(defun gather-system-info ()
  (multiple-value-bind (os-type os-info) (determine-os)
    (multiple-value-bind (cpu-type cpu-info) (determine-cpu)
      (multiple-value-bind (gpu-type gpu-info) (determine-gpu)
        `(("os-type" . ,(string os-type))
          ("os-info" . ,os-info)
          ("cpu-type" . ,(string cpu-type))
          ("cpu-info" . ,cpu-info)
          ("gpu-type" . ,(string gpu-type))
          ("gpu-info" . ,gpu-info))))))

(defun submit (project user-id &key track version description attachments client key secret token token-secret parameters (api-base "https://feedback.tymoon.eu/api/"))
  (let ((parameters (append `(("project" . ,project)
                              ("track" . ,track)
                              ("notify" . "true")
                              ("user-id" . ,(string user-id))
                              ("version" . ,(princ-to-string (or version "")))
                              ("description" . ,(or description "")))
                            (gather-system-info)
                            parameters))
        (client (or client (make-instance 'client :key key :secret secret :token token :token-secret token-secret :api-base api-base))))
    (post-file client "feedback/entry/new" attachments parameters)))

(defun submit-snapshot (project user-id session-id session-duration snapshot-duration &key version trace client key secret token token-secret parameters (api-base "https://feedback.tymoon.eu/api/"))
  (let ((parameters (append `(("project" . ,project)
                              ("user-id" . ,(princ-to-string user-id))
                              ("session-id" . ,(princ-to-string session-id))
                              ("session-duration" . ,(princ-to-string (round session-duration)))
                              ("snapshot-duration" . ,(princ-to-string (round snapshot-duration)))
                              ("version" . ,(princ-to-string (or version ""))))
                            parameters))
        (client (or client (make-instance 'client :key key :secret secret :token token :token-secret token-secret :api-base api-base))))
    (post-file client "feedback/snapshot/new" (when trace `(("trace" . ,trace))) parameters)))
