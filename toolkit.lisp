(in-package #:feedback)

(defun ensure-id (id)
  (cond ((id-code-p id) (parse-id-code id))
        ((typep id 'db:id) id)
        ((typep id 'dm:data-model) (dm:id id))
        ((stringp id) (db:ensure-id id))))

(defmacro setf-dm-fields (model &rest vars)
  (let ((modelg (gensym "MODEL")))
    `(let ((,modelg ,model))
       ,@(loop for var in vars
               collect (destructuring-bind (var &optional (field (string-downcase var))) (radiance::enlist var)
                         `(typecase ,var
                            (null)
                            (dm:data-model
                             (setf (dm:field ,modelg ,field) (dm:id ,var)))
                            (user:user
                             (setf (dm:field ,modelg ,field) (user:id ,var)))
                            (T
                             (setf (dm:field ,modelg ,field) ,var)))))
       ,modelg)))

(defun delete-directory (directory)
  (uiop:delete-directory-tree directory :validate (constantly T) :if-does-not-exist :ignore))

(defun shorten (text &key (limit 50))
  (if (<= (length text) limit)
      text
      (format NIL "~a..." (subseq text 0 (- limit 3)))))

(defun id-code (entry-ish)
  (etypecase entry-ish
    (integer
     (format NIL "$~(~4,'0x~)" entry-ish))
    (string
     entry-ish)
    (dm:data-model
     (id-code (dm:id entry-ish)))))

(defun id-code-p (entry-ish)
  (and (stringp entry-ish)
       (<= 5 (length entry-ish))
       (char= #\$ (char entry-ish 0))
       (loop for char across entry-ish
             always (or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)))))

(defun parse-id-code (entry-ish)
  (parse-integer entry-ish :radix 16))

(defun short-text (text &optional (limit 32))
  (if (<= (length text) limit)
      text
      (format NIL "~a..." (subseq text (- limit 3)))))
