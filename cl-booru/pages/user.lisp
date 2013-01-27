(in-package :cl-booru)

(defmethod webmethod ((directory (eql :|/user|)) (basename (eql :|login|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (when (not (eq 0 (<session>-account session)))
      (error 'webapp-error :text "You are already logged in"))
    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
            (:form
              :id "login-form"
              :method "POST"
              :action "/action/user/login"
              (:h3 "Sign In")
              (:label
                :for "username"
                "Username:")
              (:input
                :id "username"
                :name "username")
              (:label
                :for "password"
                "Password:")
              (:input
                :id "password"
                :name "password"
                :type "password")
              (:input
                :name "signup-submit"
                :type "submit"
                :value "Login"))))
    (<request>-respond request (<page>-apply-template page *template*)) ))

(defmethod webmethod ((directory (eql :|/user|)) (basename (eql :|register|)) request)
  (let ((page (<page>)) (session (<session> request)))
    (when (not (eq 0 (<session>-account session)))
      (error 'restricted-action-error :text "At least logout before making another account"))
    (setf (<page>-content-field page :body)
          (html
            (:form
              :id "register-form"
              :method "POST"
              :action "/action/user/register"
              (:h3 "Register")
              (:label
                :for "username"
                "Username:")
              (:input
                :id "username"
                :name "username")
              (:label
                :for "password"
                "Password:"
                (:span :class "lesser-text"
                       "(At least 8 chars)"))
              (:input
                :id "password"
                :name "password"
                :type "password")
              (:label
                :for "confirm-password"
                "Confirm Password:")
              (:input
                :id "confirm-password"
                :name "confirm-password"
                :type "password")
              (:label
                :for "email-address"
                "Email Address:"
                (:span :class "lesser-text"
                       "(Optional)"))
              (:input
                :id "email-address"
                :name "email-address")
              (:input
                :name "signup-submit"
                :type "submit"
                :value "Register"))))
    (<request>-respond request (<page>-apply-template page *template*)) ) )

(defmethod webmethod ((directory (eql :|/user|)) (basename (eql :|preferences|)) request)
  (let* ((page (<page>))
         (session (<session> request))
         (account (<account> (<session>-account session)))
         (user-prefs (<account>-preferences account)))
    (when (not (eq 0 (<account>-number account)))
      (error 'webapp-error :text "Can't change preferencs for the anonymous user"))
    (setf (<page>-content-field page :body)
          ;(format nil "~A" user-prefs)
          (html
            (:form
              :id "preferences-change-form"
              :method "POST"
              :action "/action/user/preferences"
              (:h3 "Change Preferences")
              (:h4 "Filtering Preferences")
              ;"Which categories of images you want to see when searching or browsing posts. \"Show\" the image is visible everywhere. \"Mask\" the image will show up in lists but must be clicked to be visible. \"Hide\" The image will not show up in search results, though direct links to that type of image will still show it."
              (:div :id "category-preference-options"
                    (mapc (lambda (category)
                            (let ((field-name
                                    (format nil "~A-image" (string-downcase (category-name category)))))
                              (cl-who:htm
                                (:div :class "category-preference"
                                      (:h5
                                        (:label :for field-name (cl-who:fmt "~A Posts" (category-name category))))
                                      (:select
                                        :name field-name
                                        (mapc (lambda (option)
                                                (cl-who:htm
                                                  (:option
                                                    :value (car option)
                                                    :selected
                                                    (when (eql (car option)
                                                               (handler-case
                                                                 (gethash :pref (gethash category user-prefs))
                                                                 (error () nil) ) )
                                                      t)
                                                    (cl-who:str (cdr option)))))
                                              '((0 . "Show") (1 ."Mask") (2 . "Hide"))))))))
                          '(0 1 2 3)))
              (:input
                :name "preferences-submit"
                :type "submit"
                :value "Save Preferences"))))

    (<request>-respond request (<page>-apply-template page *template*)) ))
