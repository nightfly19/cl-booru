(in-package :cl-booru)

(defmethod webmethod ((directory (eql :|/action/user|)) (basename (eql :|logout|)) request)
  (let* ((page (<page>))
         (session (<session> request))
         (account (<session>-account session)))
    (when (eq 0 account)
      (error 'webapp-error :text "You are not logged in"))
    (setf (<session>-active session) nil)
    (postmodern:update-dao session)
    (setf session (<session> request))
    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
            (:h3 "Logout Sucessful")
            "You have been successfully logged out"))
    (<page>-redirect-to page (<request>-referer request))
    (<request>-respond request (<page>-apply-template page *template*)) ) )

(defmethod webmethod ((directory (eql :|/action/user|)) (basename (eql :|login|)) request)
  (let
    ((page (<page>))
     (session (<session> request))
     (post-data (<request>-post-as-hash request))
     account)
    ;;Wrap the bits that could fail
    (when (not (eq 0 (<session>-account session)))
      (error 'webapp-error :text "You are already logged in"))
    (setf account (account-authenticate
                    (gethash :|username| post-data)
                    (gethash :|password| post-data)))
    (when (not account)
      (error 'webapp-error :text "Could not verify credentials"))
    (<session>-change-user session account)
    ;(setf session (session <request>))
    (render-session-info session page)
    (<page>-redirect-to page "/")
    (setf (<page>-content-field page :body)
          (html
            (:h3 "Login Sucessful")
            "Your account, "
            (:span :class "user-name"
                   (cl-who:esc (<account>-username account)))
            "has been successfully logged in"))
    (<request>-respond request (<page>-apply-template page *template*)) ) )

(defmethod webmethod ((directory (eql :|/action/user|)) (basename (eql :|register|)) request)
  (let
    ((page (<page>))
     (session (<session> request))
     (post-data (<request>-post-as-hash request)))
    (when (or
            (not (gethash :|username| post-data))
            (string-equal "" (gethash :|username| post-data)))
      (error 'webapp-error :text "No username supplied"))
    (when (string-equal ""
                        (string-trim 
                          '(#\Space #\Tab #\Newline)
                          (gethash :|username| post-data)))
      (error 'webapp-error :text "Please choose something besides a silly white-space only username"))

    (when (<account> (gethash :|username| post-data))
      (error 'webapp-error :text "User already exists"))
    (when (> 8 (length (gethash :|password| post-data)))
      (error 'webapp-error :text "Password is too short"))
    (when (not (string-equal 
                 (gethash :|confirm-password| post-data) 
                 (gethash :|password| post-data)))
      (error 'webapp-error :text "Passwords do not match"))
    (when (not (account-create
                 (gethash :|username| post-data)
                 (gethash :|password| post-data)))
      (error 'webapp-error :text "There was a problem creating your account. Please wait a bit or try again."))
    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
            (:h3 "Account created!")
            "Your account, "
            (:span :class "new-user-name"
                   (cl-who:esc (gethash :|username| post-data)))
            "has been successfully created!"
            "Please login to enjoy all the wonders that are now awarded to you"))
    (<request>-respond request (<page>-apply-template page *template*)) ) )

(defmethod webmethod ((directory (eql :|/action/user|)) (basename (eql :|preferences|)) request)
  (let
    ((page (<page>))
     (session (<session> request))
     (post-data (<request>-post-as-hash request)))
    (when (eql 0 (<session>-account session))
      (error 'webapp-error :text "Cannot change preferences for anonymous user"))
    (let (safe-image questionable-image explicit-image unrated-image)
      (handler-case (progn
                      (setf safe-image (parse-integer (gethash :|safe-image| post-data)))
                      (setf questionable-image 
                            (parse-integer (gethash :|questionable-image| post-data)))
                      (setf explicit-image (parse-integer (gethash :|explicit-image| post-data)))
                      (setf unrated-image (parse-integer (gethash :|unrated-image| post-data)))
                      (mapc (lambda (setting-value)
                              (when (or (< 3 setting-value) (> 0 setting-value))
                                (error "fail")))
                            (list safe-image questionable-image explicit-image unrated-image)))
        (error () (error 'webapp-error :text "Invalid preference values"))))
    ;(set-user-category-preference user-number 0 safe-image)
    ;(set-user-category-preference user-number 1 questionable-image)
    ;(set-user-category-preference user-number 2 explicit-image)
    ;(set-user-category-preference user-number 3 unrated-image))
    (<page>-redirect-to page "/")
    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
            (:h3 "Preferences Saved!")
            "The changes you made to your preferences have been saved."))
    (<request>-respond request (<page>-apply-template page *template*)) ) )
