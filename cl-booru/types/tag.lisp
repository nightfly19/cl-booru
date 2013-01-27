(in-package :cl-booru)
(defclass <tag>()
  ((number      :col-type integer :initarg :number      :accessor <tag>-number)
   (name        :col-type string  :initarg :name        :accessor <tag>-name)
   (description :col-type string  :initarg :description :accessor <tag>-description)
   (count       :col-type integer :initarg :count       :accessor <tag>-count))
  (:metaclass postmodern:dao-class)
  (:table-name tags)
  (:keys number))

(defgeneric <tag> (tag-key))
(defgeneric ensure-tag-exists (tag-name))

(defmethod  <tag> ((number integer))
  (postmodern:get-dao '<tag> number))

(defmethod  <tag> ((tag-name string))
  (car (postmodern:query-dao '<tag>
                             (s-sql:sql-compile
                               `(:select * :from tags :where (:= name ,tag-name)) ) )) )

(defmethod ensure-tag-exists ((tag-name string))
  (let ((tag (<tag> tag-name)))
    (if tag
      tag
      (progn
        (setf tag (postmodern:make-dao '<tag> :name tag-name))
        (setf (<tag>-name tag) tag-name)
        (postmodern:save-dao tag) ) ) ) )
