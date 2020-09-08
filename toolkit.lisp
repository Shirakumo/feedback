(in-package #:feedback)

(defun ensure-id (id)
  (etypecase id
    (db:id id)
    (dm:data-model (dm:id id))
    (string (db:ensure-id id))))

(defmacro setf-dm-fields (model &rest vars)
  (let ((modelg (gensym "MODEL")))
    `(let ((,modelg ,model))
       ,@(loop for var in vars
               collect (destructuring-bind (var &optional (field (string-downcase var))) (radiance::enlist var)
                         `(typecase ,var
                            (null)
                            (dm:data-model
                             (setf (dm:field ,modelg ,field) (dm:id ,var)))
                            (T
                             (setf (dm:field ,modelg ,field) ,var)))))
       ,modelg)))

(defun delete-directory (directory)
  (uiop:delete-directory-tree directory :validate (constantly T) :if-does-not-exist :ignore))
