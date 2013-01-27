(in-package :cl-booru)

;;Home page
(defmethod webmethod ((directory (eql :|/posts|)) (basename (eql :|upload|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
           (:form
            :id "upload-form"
            :method "POST"
            :enctype "multipart/form-data"
            :action "/action/posts/upload"
            (:h3 "Upload File")
            (:label :for  "file"   "File:")
            (:input :type "file"   :name "file")
            (:label :for  "title"  "Tile:")
            (:input :id   "title"  :name "title" :type "text")
            (:label :for  "source" "Source: (url)")
            (:input :id   "source" :name "source" :type "text")
            (:label :for  "tags" "Tags:")
            (:textarea :id "tags" :name "tags")
            (:input :type "submit" :name "upload-submit" :value "Upload"))))
    (<request>-respond request (<page>-apply-template page *template*))))

(defmethod webmethod ((directory (eql :|/posts|)) (basename (eql :|upload|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (setf (<request>-status request) "503 Service Unavailable")    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
           (:h3 "Uploading of Files Currently Disabled")
           "Sorry for any inconveniance, but file uploading is disabled at this time.") )

    (<request>-respond request (<page>-apply-template page *template*))))

