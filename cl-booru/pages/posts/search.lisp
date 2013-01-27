(in-package :cl-booru)

(defmethod webmethod ((directory (eql :||)) (basename (eql :|search|)) request)
  (webmethod :|/posts/list/page| :|1| request))

(defmethod webmethod ((directory (eql :|/search|)) (basename (eql :||)) request)
  (webmethod :|/posts/list/page| :|1| request))

(defmethod webmethod ((directory (eql :|/posts/search/tags|)) basename request)
  (let ((page (<page>)) (session (<session> request)) query page-number images user-preferences)
    (handler-case
      (setf page-number (+ -1 (parse-integer
                                (<target>-parameter (<request>-target request) :|page|) )))
      (error () (setf page-number 0)))
    (when (< page-number 0)
      (setf page-number 0))
    (setf query (it.bese.arnesi::unescape-as-uri (string basename)))
    (setf user-preferences
          (<account>-preferences (<session>-account (<session> request))))
    (setf images (image-search query
                               (* +results-per-page+ page-number)
                               +results-per-page+
                               (categories-to-exclude user-preferences)))
    ;;Page content
    (render-session-info session page)
    (setf (<page>-title page) (format nil "~A: ~A" +site-title+ query))
    (setf (<page>-content-field page :tag-search-query) query)
    (setf (<page>-content-field page :body)
          (html
            (:div
              (cl-who:str (render-page-chooser
                            (+ page-number 1)
                            (num-results-pages images)
                            (concatenate 'string "/posts/search/tags/" query "?page=")))
              (cl-who:str (render-page-of-images images (categories-to-mask user-preferences)))
              (cl-who:str (render-page-chooser
                            (+ page-number 1)
                            (num-results-pages images)
                            (concatenate 'string "/posts/search/tags/" query "?page="))))))
    (<request>-respond request (<page>-apply-template page *template*)) ) )
