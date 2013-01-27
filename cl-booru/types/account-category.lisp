(in-package :cl-booru)

(defclass <account-category>()
  ((account  :col-type integer :initarg :account  :accessor <account-category>-account)
   (category :col-type integer :initarg :category :accessor <account-category>-category)
   (pref     :col-type integer :initarg :pref     :accessor <account-category>-pref))
  (:metaclass postmodern:dao-class)
  (:table-name account-category-prefs)
  (:keys account category))

(defgeneric <account-category> (account-key1 account-key2))
(defgeneric categories-to-exclude (preferences))
(defgeneric categories-to-mask (preferences))

(defmethod  <account-category> ((account integer) (category integer))
  (postmodern:get-dao '<account> account category))

(defmethod <account>-preferences ((account-number integer))
  (postmodern:query-dao
    '<account-category>
    (s-sql:sql-compile `(:select *
                                 :from account-category-prefs
                                 :where
                                 (:= account ,account-number) ) ) ) )

(defmethod <account>-preferences ((account <account>))
  (<account>-preferences (<account>-number account)) )

(defmethod  <category> ((account-category  <account-category>))
  (<category> (<account-category>-category account-category)) )

(defmethod categories-to-exclude ((preferences null))
  nil)

(defmethod categories-to-exclude ((preferences cons))
  (let (to-exclude)
    (mapc (lambda(preference)
            (when (eql 2 (<account-category>-pref preference))
              (push (<account-category>-category preference) to-exclude))) preferences)
    to-exclude))

(defmethod categories-to-mask ((preferences null))
  nil)

(defmethod categories-to-mask ((preferences cons))
  (let (to-exclude)
    (mapc (lambda(preference)
            (when (eql 1 (<account-category>-pref preference))
              (push (<account-category>-category preference) to-exclude))) preferences)
    to-exclude))
