(in-package :cl-booru)


(defclass <category>()
  ((number      :col-type integer :initarg :number      :accessor <category>-number)
   (name        :col-type string  :initarg :name        :accessor <category>-name)
   (description :col-type string  :initarg :description :accessor <category>-description) )
  (:metaclass postmodern:dao-class)
  (:table-name image-categories)
  (:keys number) )

(defgeneric <category> (account-key))

(defmethod  <category> ((category-number integer))
  (postmodern:get-dao '<category> category-number) )

(defmethod <category>-number ((category-list list))
  (mapcar (lambda (category) (<category>-number category))
          category-list ) )
