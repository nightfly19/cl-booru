(in-package :webby)

(defclass <page> ()
  ((title
    :initarg :title
    :initform ""
    :accessor <page>-title )
   (extra-head
     :initarg :extra-head
     :initform ""
     :accessor <page>-extra-head )
   (content
     :initarg :content
     :initform (make-hash-table)
     :accessor <page>-content ) ) )

(defgeneric <page>-content-field (page field))
(defgeneric <page>-apply-template (page template))
(defgeneric <page>-redirect-to (page new-page))

(defun <page> () (make-instance '<page>))

(defmethod <page>-content-field ((page <page>) field)
  (gethash field (<page>-content page)) )

(defsetf <page>-content-field (page field) (value)
         `(setf (gethash ,field (<page>-content ,page)) ,value))

(defmethod <page>-apply-template ((page <page>) template)
  (funcall template page))

(defmethod <page>-redirect-to ((page <page>) new-page)
  (setf (<page>-content-field page :extra-header)
        (format nil "<meta http-equiv=\"refresh\" content=\"1;url=~A\" />"
                new-page)))
