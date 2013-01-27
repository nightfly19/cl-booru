(in-package :cl-booru)

(defclass <session> ()
  ((number     :col-type integer :initarg :number     :accessor <session>-number)
   (account    :col-type integer :initarg :account    :accessor <session>-account)
   (occured    :col-type simple-date:timestamp
                                 :initarg :occured    :accessor <session>-occured)
   (refreshed  :col-type integer :initarg :refreshed  :accessor <session>-refreshed)
   (active     :col-type boolean :initarg :active     :accessor <session>-active)
   (hash-id    :col-type string  :initarg :string     :accessor <session>-hash-id)
   (ip         :col-type t       :initarg :ip         :accessor <session>-ip) )
  (:metaclass postmodern:dao-class)
  (:table-name user-sessions)
  (:keys number))

(defgeneric <session> (session-key))
(defgeneric new-session (session-key))
(defgeneric <session>-change-user (session username))
(defgeneric render-session-info (session page))

(defmethod <session> ((session-number integer))
  (postmodern:get-dao '<session> session-number))

(defmethod <session> ((request <request>))
  (if (gethash :|session| (<request>-cookies request))
    (progn
      (let ((session
              (car (postmodern:query-dao
                     '<session>
                     (s-sql:sql-compile 
                       `(:select *
                                 :from 'user-sessions
                                 :where (:and
                                          (:= 'active t)
                                          (:= 'ip ,(env-value "REMOTE_ADDR" (<request>-environment request)))
                                          (:= 'hash_id ,(gethash :|session| (<request>-cookies request))) ) ) ) ) ) ))
        (if session session (new-session request))))
    (new-session request)))

(defun hash-session-id (id)
  (md5-hex-digest
    (md5:md5sum-sequence
      (concatenate 'string 
                   +session-salt+
                   (write-to-string id) ) ) ) )

(defmethod new-session ((ip string))
  (let ((session (postmodern:make-dao '<session>)))
    (setf (<session>-account session) 0)
    (setf (<session>-ip session) ip)
    (setf (<session>-hash-id session) (hash-session-id (<session>-number session)))
    (postmodern:update-dao session)
    session) )

(defmethod new-session ((request <request>))
  (let ((session (new-session (env-value "REMOTE_ADDR" (<request>-environment request)))))
    (<request>-add-header request
                          (set-cookie-header :|session| (<session>-hash-id session)) )
    session ) )

(defmethod  <account> ((session <session>))
  (<account> (<session>-account session)))

(defmethod render-session-info ((session <session>) (page <page>))
  (let ((account (<account> (<session>-account session))))
    (setf (<page>-content-field page :session-info)
          (if (not (eq 0 (<account>-number account)))
            (html
              (:a :href "/user/preferences"
                  (:span :class "user-name"
                         (cl-who:esc (<account>-username account))))
              "&ndash;"
              (:a :href "/action/user/logout"
                  "Logout"))
            (html
              (:a :href "/user/register"
                  "Register")
              "&ndash;"
              (:a :href "/user/login"
                  "Login"))))))

(defmethod <session>-change-user ((session <session>) (account-number integer))
  (setf (<session>-account session) account-number)
  (postmodern:save-dao session) )

(defmethod <session>-change-user ((session <session>) (account <account>))
  (<session>-change-user session (<account>-number account)) )
