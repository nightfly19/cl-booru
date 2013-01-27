(in-package :cl-booru)

(defmethod webmethod ((directory (eql :|/docs|)) (basename (eql :|about|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (render-session-info session page)
    (setf (<page>-title page) (format nil "About ~A" +site-title+))
    (setf (<page>-content-field page :body)
          (html
            (:div :class "document"
                  (:h3 (cl-who:esc (<page>-title page)))
                  (:p
                    "This is a booru"))))
    (<request>-respond request (<page>-apply-template page *template*)) ))

(defmethod webmethod ((directory (eql :|/docs|)) (basename (eql :|contact|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (render-session-info session page)
    (setf (<page>-title page) (format nil "Contact ~A" +site-title+))
    (setf (<page>-content-field page :body)
          (html
            (:div :class "document"
                  (:h3 (cl-who:esc (<page>-title page)))
                  "contact details")))
    (<request>-respond request (<page>-apply-template page *template*)) ))

(defmethod webmethod ((directory (eql :|/docs|)) (basename (eql :|terms-of-service|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (render-session-info session page)
    (setf (<page>-title page) "Terms of Service")
    (setf (<page>-content-field page :body)
          (html
            (:div :class "document"
                  (:h3 (cl-who:esc (<page>-title page)))
                  "Terms of service")))))
 
    (<request>-respond request (<page>-apply-template page *template*)) ))
