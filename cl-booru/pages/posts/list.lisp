(in-package :cl-booru)

(defmethod webmethod ((directory (eql :|/posts|)) (basename (eql :|list|)) request)
  (webmethod :|/posts/list/page| :|1| request))

(defmethod webmethod ((directory (eql :|/posts/list/page|)) basename request)
  (let ((page (<page>)) (session (<session> request)) page-number images user-preferences)
    (render-session-info session page)
    (setf (<page>-title page) +site-title+)
    ;;Input checking
    (handler-case
      (setf page-number (- (parse-integer (string basename))1))
      (parse-error () (error 'page-not-found-error)))
    (when (> 0 page-number)
      (error 'page-not-found-error) )
    (setf user-preferences
          (<account>-preferences (<session>-account (<session> request))))
    (setf images (posts-in-range
                   (* +results-per-page+ page-number)
                   +results-per-page+
                   (categories-to-exclude  user-preferences)))
    (setf (<page>-content-field page :body)
          (html
            (:div
              (cl-who:str (render-page-chooser
                            (+ page-number 1)
                            (num-results-pages images)
                            "/posts/list/page/"))
              (cl-who:str (render-page-of-images images (categories-to-mask user-preferences)))
              (cl-who:str (render-page-chooser
                            (+ page-number 1)
                            (num-results-pages images)
                            "/posts/list/page/")) ) ) )
    (<request>-respond request (<page>-apply-template page *template*)) ) )


