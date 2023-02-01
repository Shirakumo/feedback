(in-package #:feedback)

(defvar *author* NIL)

(defun author ()
  (or *author* (auth:current "anonymous")))

(defun ensure-id (id)
  (cond ((id-code-p id) (db:ensure-id (parse-id-code id)))
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
       (loop for i from 1 below (length entry-ish)
             for char = (char entry-ish i)
             always (or (char<= #\0 char #\9)
                        (char<= #\a char #\f)
                        (char<= #\A char #\F)))))

(defun parse-id-code (entry-ish)
  (parse-integer entry-ish :start 1 :radix 16))

(defun short-text (text &optional (limit 32))
  (if (<= (length text) limit)
      text
      (format NIL "~a..." (subseq text 0 (- limit 3)))))

(defun render-description (text)
  (if text
      (cl-markless:output (cl-markless:parse text T)
                          :target (plump:make-root)
                          :format 'cl-markless-plump:plump)
      ""))

(defun render-comment (text)
  (if text
      (cl-markless:output (cl-markless:parse text T)
                          :target (plump:make-root)
                          :format 'cl-markless-plump:plump)
      ""))

(defun one-of (value &rest options)
  (find value options :test (lambda (a b) (search b a :test #'char-equal))))

(defmacro with-stringcase (value &body body)
  (let ((v (gensym "V")))
    `(let ((,v ,value))
       (cond ,@(loop for (vals . forms) in body
                     collect `(,(cond ((eql T vals) T)
                                      ((listp vals) `(one-of ,v ,@vals))
                                      (T `(one-of ,v ,vals)))
                               ,@forms))))))

(defun parse-color (color)
  (cond ((string= "" color) #xFFFFFF)
        ((char= #\# (char color 0)) (parse-integer color :start 1 :radix 16))
        (T (or (parse-integer color :junk-allowed T)
               (with-stringcase color
                 ("red"   #xFF0000)
                 ("green" #x00FF00)
                 ("blue"  #x0000FF)
                 ("white" #xFFFFFF)
                 ("black" #x000000)
                 (T       #xFFFFFF))))))

(defun print-color (color)
  (format NIL "#~6,'0X" color))
