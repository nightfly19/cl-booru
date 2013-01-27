(in-package :cl-booru)

(defclass <image> ()
  ((number     :col-type integer :initarg :number     :accessor <image>-number)
   (category   :col-type integer :initarg :category   :accessor <image>-category)
   (source-url :col-type string  :initarg :source-url :accessor <image>-source-url)
   (md5        :col-type string  :initarg :md5        :accessor <image>-md5)
   (ext        :col-type string  :initarg :ext        :accessor <image>-ext)
   (account    :col-type integer :initarg :account    :accessor <image>-account)
   (occured    :col-type simple-date:timestamp
                                 :initarg :occured    :accessor <image>-occured)
   (status     :col-type boolean :initarg :status     :accessor <image>-status)
   (width      :col-type integer :initarg :width      :accessor <image>-width)
   (height     :col-type integer :initarg :height     :accessor <image>-height)
   (file-size  :col-type integer :initarg :file-size  :accessor <image>-file-size)
   (active     :col-type boolean :initarg :active     :accessor <image>-active)
   (title      :col-type string  :initarg :title      :accessor <image>-title))
  (:metaclass postmodern:dao-class)
  (:table-name images)
  (:keys number))

(defclass <image-tag>()
  ((image   :col-type integer :initarg :image   :accessor <image-tag>-image)
   (tag     :col-type integer :initarg :tag     :accessor <image-tag>-tag)
   (active  :col-type boolean :initarg :active  :accessor <image-tag>-active))
  (:metaclass postmodern:dao-class)
  (:table-name image-tags)
  (:keys image tag))

(defgeneric <image> (image-key))
(defgeneric <image>-views  (image))
(defgeneric <image>-rating (image))
(defgeneric <image>-comments (image))
(defgeneric <image>-tag (image tag))
(defgeneric <image>-untag (image tag))
(defgeneric <image>-tags (image))
(defgeneric <image>-tag-names (image))
(defgeneric <image>-tags-as-string (image))
(defgeneric <image>-log-view (image session))

(defgeneric <image-tag> (image-key tag-key))

(defgeneric images-in-set (set))

(defmethod  <image> ((number integer))
  (postmodern:get-dao '<image> number))

(defmethod  <account> ((image <image>))
  (<account> (<image>-account image)))

(defmethod <image-tag> ((image-number integer) (tag-number integer))
  (postmodern:get-dao '<image-tag> image-number tag-number) )

(defmethod <image-tag> ((image <image>) (tag <tag>))
  (<image-tag> (<image>-number image) (<tag>-number tag)) )

(defmethod <image>-views ((image-number integer))
  (postmodern:query
    (:select (:count '*)
             :from 'image-views
             :where (:= 'image image-number)) :single))

(defmethod <image>-views ((image <image>))
  (<image>-views (<image>-number image)) )

(defmethod <image>-rating ((image-number integer))
  (let ((score (postmodern:query
                 (:select (:sum 'rating)
                          :from 'image-ratings
                          :where (:= 'image image-number)) :single)))
    (if (not (eql :null score))
      score 0)))

(defmethod <image>-rating ((image <image>))
  (<image>-rating (<image>-number image)) )

(defmethod <image>-comments ((image-number integer))
  (postmodern:query-dao
    '<comment>
    (s-sql:sql-compile `(:select '*
                                 :from 'comments
                                 :where (:and
                                          (:= comments.active t)
                                          (:= 'comments.image ,image-number))))))

(defmethod <image>-comments ((image <image>))
  (<image>-comments (<image>-number image)))

(defmethod <image>-tags ((image-number integer))
  (postmodern:query-dao
    '<tag>
    (s-sql:sql-compile `(:select *
                                 :from tags
                                 :where
                                   (:in tags.number
                                        (:select image-tags.tag
                                                 :from image-tags
                                                 :where (:and
                                                          (:= image-tags.active t)
                                                          (:= image-tags.image ,image-number))))))))

(defmethod <image>-tags ((image <image>))
  (<image>-tags (<image>-number image)))

(defmethod <image>-tag-names ((image <image>))
  (mapcar (lambda (tag)
            (<tag>-name tag))
          (sort (<image>-tags image) (lambda (tag1 tag2)
                                       (> (<tag>-count tag1) (<tag>-count tag2)) )) ) )

(defmethod <image>-tag-names ((image-number integer))
  (<image>-tag-names (<image> image-number)))

(defmethod <image>-tags-as-string ((image <image>))
  (let ((tag-string ""))
    (mapc (lambda (tag)
            (setf tag-string (format nil "~A ~A" tag-string tag)))
          (<image>-tag-names image))
    (string-trim '(#\Space) tag-string)))

(defmethod <image>-tags-as-string ((image-number integer))
  (<image>-tags-as-string (<image> image-number)))

(defmethod <image>-log-view ((image-number integer) (session <session>))
  (handler-case (postmodern:query
                  (:insert-into 'image-views
                                :set
                                'user-session (<session>-number session)
                                'image image-number))
    (error () nil)) )

(defmethod <image>-log-view ((image <image>) (session <session>))
  (<image>-log-view (<image>-number image) session) )

(defun image-by-checksum (checksum)
  (car (postmodern:query-dao
         '<account>
         (s-sql:sql-compile `(:select * :from images :where (:= md5 ,checksum))) )) )

(defmethod <image>-tag ((image <image>) (tag <tag>))
  (let ((image-tag (make-instance '<image-tag>
                                  :image (<image>-number image)
                                  :tag (<tag>-number tag)
                                  :active t ))
        (current-tag (postmodern:get-dao '<image-tag> (<image>-number image) (<tag>-number tag))) )
    (postmodern:save-dao image-tag)
    (when (not (or current-tag (<image-tag>-active current-tag)))
      (incf (<tag>-count tag))
      (postmodern:update-dao tag)) ) )

(defmethod <image>-tag ((image-number integer) (tag-number integer))
  (<image>-tag (<image> image-number) (<tag> tag-number)) )

(defmethod <image>-tag ((image <image>) (tag-name string))
  (<image>-tag image (ensure-tag-exists tag-name)) )

(defmethod <image>-tag ((image integer) (tag-name string))
  (<image>-tag (<image> image) (ensure-tag-exists tag-name)) )

(defmethod <image>-untag ((image <image>) (tag <tag>))
  (let ((image-tag (make-instance '<image-tag>
                                  :image (<image>-number image)
                                  :tag (<tag>-number tag)
                                  :active nil ))
        (current-tag (postmodern:get-dao '<image-tag> (<image>-number image) (<tag>-number tag))) )
    (postmodern:save-dao image-tag)
    (when (or current-tag (<image-tag>-active current-tag))
      (decf (<tag>-count tag))) ))

(defmethod <image>-untag ((image-number integer) (tag-number integer))
  (<image>-untag (<image> image-number) (<tag> tag-number)) )

(defmethod <image>-untag ((image <image>) (tag-name string))
  (let ((tag (<tag> tag-name)))
    (when tag (<image>-untag image tag)) ) )

(defmethod images-in-set (image-set)
  (mapcar (lambda (image) (<image> image)) image-set))
