(in-package :cl-booru)

(defclass <comment>()
  ((number  :col-type integer :initarg :number  :accessor <comment>-number)
   (image   :col-type integer :initarg :image   :accessor <comment>-image)
   (account :col-type integer :initarg :account :accessor <comment>-account)
   (comment :col-type string  :initarg :comment :accessor <comment>-comment)
   (occured :col-type integer :initarg :occured :accessor <comment>-occured)
   (active  :col-type boolean :initarg :active  :accessor <comment>-active)
   (rating  :initform 0       :initarg :rating  :reader   <comment>-rating))
  (:metaclass postmodern:dao-class)
  (:table-name comments)
  (:keys number))

(defgeneric <comment> (comment-key))

(defmethod  <comment> (number)
  (postmodern:get-dao '<comment> number))

(defmethod <comment>-rating ((comment-number integer))
  (let ((score (postmodern:query
                 (:select (:sum 'rating)
                          :from 'comment-ratings
                          :where (:= 'comment comment-number)) :single)))
    (if (not (eql :null score))
      score 0)))

(defmethod <comment>-rating ((comment <comment>))
  (<comment>-rating (<comment>-number comment)))

