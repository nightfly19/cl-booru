(in-package :cl-booru)

(defun webapp-error-respond (error-object request)
  (let ((page (<page>)) (session (<session> request)))
    (render-session-info session page)
    (setf (<page>-title page) (slot-value error-object 'webby::title))
    (setf (<request>-status request) (slot-value  error-object 'webby::code))
    (setf (<page>-content-field page :title)
          (slot-value error-object 'webby::title) )
    (setf (<page>-content-field page :body)
          (html
            (:h3 (cl-who:esc (slot-value error-object 'webby::title))) ) )
    (format *standard-output* "Error: ~A~%" (slot-value error-object 'webby::text) )
    (<request>-respond request (<page>-apply-template page *template*)) ))

(defun error-respond (error-object request)
  (let ((page (<page>)))
    error-object;Keep the loader from bitching
    (setf (<page>-title page) "Error")
    (setf (<request>-status request) "500 Internal Server Error")
    (setf (<page>-content-field page :title) "Server Error")
    (setf (<page>-content-field page :body)
          (html
            (:h3 (cl-who:esc "Server Error"))
            "Something has gone wrong..."))
    (format *standard-output* "Error: ~A~%" error-object)
    (<request>-respond request (<page>-apply-template page *template*)) ))
