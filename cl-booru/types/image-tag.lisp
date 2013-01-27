(in-package :cl-booru)

(defclass <image-tag>()
  ((image   :col-type integer :initarg :image   :accessor <image-tag>-image)
   (tag     :col-type integer :initarg :tag     :accessor <image-tag>-tag)
   (active  :col-type boolean :initarg :active  :accessor <image-tag>-active))
  (:metaclass postmodern:dao-class)
  (:table-name image-tags)
  (:keys image tag))

(defgeneric <image-tag> (image-key tag-key))

(defmethod <image-tag> ((image-number integer) (tag-number integer))
  (postmodern:get-dao '<image-tag> image-number tag-number) )

(defmethod <image-tag> ((image <image>) (tag <tag>))
  (<image-tag> (<image>-number image) (<tag>-number tag)) )
