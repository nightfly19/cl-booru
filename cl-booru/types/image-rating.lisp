(in-package :cl-booru)

(defclass <image-rating>()
  ((account :col-type integer :initarg :account :accessor <image-rating>-account)
   (image   :col-type integer :initarg :image   :accessor <image-rating>-image)
   (rating  :col-type integer :initarg :rating  :accessor <image-rating>-rating))
  (:metaclass postmodern:dao-class)
  (:table-name image-ratings)
  (:keys account image))

(defgeneric <image-rating> (image-key account-key))
(defgeneric vote-on-image (image account vote))

(defmethod <image-rating> ((image-number integer) (account-number integer))
  (postmodern:get-dao '<image-rating> image-number account-number) )

(defmethod <image-rating> ((image <image>) (account <account>))
  (<image-rating> (<image>-number image) (<account>-number account)) )

(defmethod vote-on-image ((image-number integer) (account-number integer) vote)
  (let* ((vote-value (if vote 1 -1))
         (image-rating
           (make-instance '<image-rating> 
                          :account account-number
                          :image image-number
                          :rating vote-value) ) )
    (postmodern:save-dao image-rating) ) )
