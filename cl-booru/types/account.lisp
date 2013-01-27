(in-package :cl-booru)

(defclass <account>()
  ((number   :col-type integer :initarg :number   :accessor <account>-number)
   (username :col-type string  :initarg :username :accessor <account>-username)
   (password :col-type t       :initarg :password :accessor <account>-password)
   (email    :col-type string  :initarg :email    :accessor <account>-email)
   (status   :col-type integer :initarg :status   :accessor <account>-status)
   (verified :col-type boolean :initarg :verified :accessor <account>-verified))
  (:metaclass postmodern:dao-class)
  (:table-name accounts)
  (:keys number))

(defgeneric <account> (account-key))
(defgeneric <account>-preferences (account-key))

(defgeneric account-authenticate (user-name password))
(defgeneric account-create (user-name password))


(defmethod  <account> ((number integer))
  (postmodern:get-dao '<account> number) )

(defmethod  <account> ((username string))
  (car (postmodern:query-dao '<account>
                             (s-sql:sql-compile
                               `(:select * :from accounts :where (:= username ,username))))))

(defun hash-password (password)
  (md5-hex-digest
    (md5:md5sum-sequence
      (concatenate 'string +salt+ password) ) ) )

(defmethod account-authenticate ((username string) (password string))
  (car (postmodern:query-dao '<account>
                             (s-sql:sql-compile
                               `(:select *
                                  :from accounts
                                  :where (:and
                                           (:= username ,username)
                                           (:= password ,(hash-password password))))))))

(defmethod account-create (username password)
  (let ((new-account (postmodern:make-dao '<account>)))
    (setf (<account>-username new-account) username)
    (setf (<account>-password new-account) (hash-password password))
    (postmodern:save-dao new-account)
    new-account ) )
