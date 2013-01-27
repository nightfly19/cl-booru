(in-package :cl-booru)

(defgeneric render-page-chooser (current-page number-of-pages link-prefix &optional page-slots))
(defgeneric render-page-of-images (images &optional cats-to-mask))

(defun num-results-pages (images)
  (ceiling (/ (<result-set>-total images) +results-per-page+)) )

(defun image-link-target (image-number)
  (concatenate 'string "/posts/view/" (write-to-string image-number)))

(defun image-thumb-target (image)
  (format nil "/images/thumb/~A.jpg" (<image>-md5 image)) )

(defun image-tags-names (image)
  (sort (mapcar (lambda(tag)
                  (gethash :tag-name tag))
                (gethash :tags image)) #'string-lessp))

;;REALLY needs to be cleaned up
(defmethod render-page-chooser (current-page
                             number-of-pages
                             link-prefix
                             &optional (page-slots 8))
  (let ((at-start (eq current-page 1)) (at-end (eq current-page number-of-pages)) list-start list-end)
    (when (> page-slots number-of-pages)
      (setf page-slots number-of-pages))
    (when at-start
      (decf page-slots 2))
    (when at-end
      (decf page-slots 2))
    ;(when (at-end)
    ;  (decf page-slots 2))
    (if (< current-page (/ number-of-pages 2))
      (progn
        (setf list-start current-page)
        (setf list-end (+ current-page (- page-slots 1))))
      (progn
        (setf list-end current-page)
        (setf list-start (- current-page (+ page-slots 1)))))
    (when (< list-start 1)
      (setf list-start 1))
    (html
      (:ul
        :class "pager"
        ;; Prev arrows
        (when (not at-start)
          (progn
            (htm
              (:a
                :href (concatenate 'string link-prefix (write-to-string 1))
                :title (write-to-string 1)
                (:li
                  :class "pager-page-inactive"
                  "<<") ) )
            (htm
              (:a
                :href (concatenate 'string link-prefix (write-to-string (- current-page 1)))
                :title (write-to-string (- current-page 1))
                (:li
                  :class "pager-page-inactive"
                  "<") ) )))
        ;; Numbers
        (loop
          for current-number
          from list-start to list-end
          do (progn
               (if (eq current-number current-page)
                 (htm
                   (:li
                     :class "pager-page-active"
                     (str (write-to-string current-number))) )
                 (htm
                   (:a :href (concatenate 'string link-prefix (write-to-string current-number))
                       :title (write-to-string current-number)
                       (:li
                         :class "pager-page-inactive"
                         (str (write-to-string current-number))))))))
        ;; Next arrows
        (when (not at-end)
          (progn
            (htm
              (:a 
                :href (concatenate 'string link-prefix (write-to-string (+ current-page 1))) 
                :title (write-to-string (+ current-page 1))
                (:li
                  :class "pager-page-inactive"
                  ">") ) )
            (htm
              (:a 
                :href (concatenate 'string link-prefix (write-to-string number-of-pages)) 
                :title (write-to-string number-of-pages)
                (:li
                  :class "pager-page-inactive"
                  ">>") ) ) ) ) ) ) ) )

(defun category-name (category)
  (case category
    (0 "Safe")
    (1 "Questionable")
    (2 "Explicit")
    (3 "Unrated")))

(defmethod render-page-of-images (images &optional cats-to-mask)
  cats-to-mask
  (html
    (mapc (lambda (image)
            (htm
              (:div :class "image-thumb"
                    :data-number (<image>-number image)
                    :data-tags (json::encode-json-to-string (<image>-tag-names image))
                    (:a :href (image-link-target (<image>-number image))
                        :title (esc (<image>-tags-as-string image))
                        (:img :src (image-thumb-target image))))))
                              ;(if (find category cats-to-mask)
                                ;(format nil "/resources/images/~A.png" 
                                ;        (string-downcase
                                ;          (category-name category)))
                                ;(image-thumb-target image-number images)))))))
                                (<result-set>-images images))))

(defun render-image-url (image)
  (concatenate 'string "/images/full/"
               (<image>-md5 image)
               (when (<image>-ext image)
                 (concatenate 'string "."
                              (<image>-ext image)))))

(defun vote-link (target &key (type "up") (prefix "/action/posts/vote") id)
  (html-ni
    (:a
      :id id
      :href (concatenate 'string prefix type "/" target)
      (str type))))

(defun vote-link-image (image &optional (type "up"))
  (let ((num-string (write-to-string (<image>-number image))))
    (vote-link num-string :type type :id (format nil "image-vote-~A" type) :prefix "/action/posts/vote/")))

(defun vote-link-comment (comment &optional (type "up"))
  (let ((num-string (write-to-string comment)))
    (vote-link num-string :type type :prefix "/action/comments/vote/")))

(defun format-time (time)
    (format nil "~D-~D-~D ~D:~D:~D"
                (nth 0 time)
                (nth 1 time)
                (nth 2 time)
                (nth 3 time)
                (nth 4 time)
                (nth 5 time)))

(defun sidebar-view-about (image request)
  (let ((account (<account> (<image>-account image))))
  (html
    (:h4 "About")
    (:ul :class "image-about-list"
         (:li
           "Number: " (str (<image>-number image)))
         (:li
           "Posted: "
           ;(str "Meow"))
           (str (format-time
                         (multiple-value-list (simple-date:decode-timestamp (<image>-occured image))))))
         (:li
           "By: "
           (:a :href (concatenate 'string "/users/profile/"
                                  (it.bese.arnesi::escape-as-uri
                                    (<account>-username account)))
               (str (<account>-username account))))
         (when (not (eql :null (<image>-width image)))
           (htm
             (:li
               "Size: "
               (str (concatenate 'string
                                        (write-to-string (<image>-width image))
                                        "x"
                                        (write-to-string (<image>-height image)))))))
         (:li
           "Source: "
           (:a :href (<image>-source-url image)
               (esc (<image>-source-url image))))
         (:li
           "Rating: " 
           (str
             (case (<image>-category image)
               (0 "Safe")
               (1 "Questionable")
               (2 "Explicit")
               (3 "Unrated"))))
         (:li
           "Views: " (:span :id "image-views" (fmt "~D" (<image>-views image))))
         (:li
           "Score: " (:span :id "image-score" (str (<image>-rating image)))
           (when (not (eql 0 (<session>-account (<session> request))))
             (str
               (concatenate 'string
                            " (vote "
                            (vote-link-image image "up")
                            "/"
                            (vote-link-image image "down")
                            ")"))))))))

(defun sidebar-view-tags (tags)
  (html
    (:h4 "Tags")
    (:ul
      (mapc (lambda (tag)
              (htm
                (:li
                  (:a :href
                      (concatenate 'string "/posts/search/tags/"
                                   (it.bese.arnesi::escape-as-uri (<tag>-name tag)))
                      (esc (<tag>-name tag)))
                  (str (<tag>-count tag))
                  )))
            (sort tags (lambda (tag1 tag2)
                         (> (<tag>-count tag1) (<tag>-count tag2)) )) ) ) ) )

(defun sidebar-view-body (image request)
  (html
    (:div :id "image-sidebar"
          (:div :id "image-sidebar-tags"
                (str (sidebar-view-tags (<image>-tags image)))
                (str (sidebar-view-about image request))))))

(defun posts-edit-form (image)
  (html
    (:form :id "post-edit-form" :method "post" :action "/action/posts/edit"
           :style "display:none"
           (:h3 "Edit Post")
           (:input :type "hidden"
                   :name "post-number"
                   :id "post-number"
                   :value
                   (write-to-string (<image>-number image)))
           (:label :for "post-category" "Rating:")
           (:input :type "radio" :name "post-category" :value "2"
                   :checked (eql 2 (<image>-category image)))
           "Explicit"
           (:input :type "radio" :name "post-category" :value "1"
                   :checked (eql 1 (<image>-category image)))
           "Questionable"
           (:input :type "radio" :name "post-category" :value "0"
                   :checked (eql 0 (<image>-category image)))
           "Safe"
           (:label :for "post-title" "Title:")
           (:input :id "post-title" :name "post-title" :type "text"
                   :value (<image>-title image))
           (:label :for "post-source" "Source Url:")
           (:input :id "post-source" :name "post-source"  :type "text"
                   :value (<image>-source-url image))
           (:label :for "post-tags-box" "Tags:")
           (str
             (html-ni
               (:textarea
                 :id "post-tags-box" :name "post-tags-box"
                 (str
                   (<image>-tags-as-string image) ) ) ) )
           (:input :type "submit" :id "post-edit-submit" 
                   :name "post-edit-submit" :value "Save" ) ) ) )

(defun posts-comment-form (image)
  (html
    (:form :id "post-comment-form" :method "post" :action "/action/posts/comment"
           :style "display:none"
           (:input :type "hidden"
                   :name "post-number"
                   :value 
                   (write-to-string (<image>-number image)))
           (:h3 "Comment on Post")
           (:textarea :id "post-comment-box" :name "post-comment-box")
           (:div :id "post-comment-anonymously-checkbox"
                 (:input :type "checkbox" :id "post-comment-anonymously"
                         :name "post-comment-anonymously" :value "true")
                 (:label :for  "post-comment-anonymously" "Post Anonymously") )
           (:input :type "submit" :id "post-comment-submit" 
                   :name "post-comment-submit" :value "Comment" )
           ) ) )

(defun posts-view-comment (comment)
  (let ((account (<account> (<comment>-account comment))))
    (html
      (:div :class "post-comment-frame"
            (:a :class "comment-username"
                :href (concatenate 'string "/user/profile/"
                                   (it.bese.arnesi::escape-as-uri (<account>-username account)))
                (str (<account>-username account)))
            (:span :class "comment-number"
                   (fmt "(~A)" (<comment>-number comment)))
            (:div :class "comment-header-blurb"
                  "Posted "
                  (str
                    (format-time (multiple-value-list
                                   (simple-date:decode-timestamp (<comment>-occured comment))))))
            ;" Score "
            ;(str (gethash :rating comment))
            ;(str
            ;(concatenate 'string
            ;" (vote "
            ;(vote-link-comment (gethash :number comment) "up")
            ;"/"
            ;(vote-link-comment (gethash :number comment) "down")
            ;")")))
            (esc (<comment>-comment comment))))))

(defun posts-view-comments (image)
  (html
    (mapc (lambda (comment) (str (posts-view-comment comment)))
          (sort (<image>-comments image)
                (lambda (comment1 comment2)
                  (< (<comment>-number comment1) (<comment>-number comment2)))))))

(defun posts-view-body (image)
  (html
    (:noscript
      (:style
        :type "text/css"
        "#post-comment-form{display:block !important;}" 
        "#post-edit-form{display:block !important;}" ) )
    (:div :class "full-image-view"
          (:a :href (render-image-url image) :target "_blank"
              (:img :class "full-image" :src (render-image-url image)))
          (:div :class "post-meta"
                (:div :class "action-buttons"
                      :style "display:none"
                      (:a :href "#post-edit-form"
                          :title "Edit Post"
                          :id "post-edit-button" (:h3 "Edit"))
                      (:a :href "#post-comment-form"
                          :title "Comment on Post"
                          :id "post-comment-button" (:h3 "Comment") ) )
                (str (posts-edit-form image))
                (str (posts-comment-form image) )
                (str (posts-view-comments image)) ) ) ) )

(defun image-description (image)
  (let ((title (<image>-title image))
        (tags  (<image>-tags-as-string image)))
    (if (not (string-equal "" title))
      (format nil "~A: ~A" title tags)
      (format nil "~A: ~A" +site-title+ tags))))

(defun web-vote-on-image (raw-image action request)
  (let ((page (<page>))
        (session (<session> request))
        image-number image account-number vote-value)
    (handler-case (setf image-number (parse-integer (string raw-image)))
      (error () (error 'page-not-found-error)))
    (setf image (<image> image-number))
    (setf account-number (<session>-account session))
    (setf vote-value (if (eql action :|up|) t nil))
    (when (not image)
      (error 'webapp-error :text "Invalid image number" :title "Voting error"))
    (handler-case (vote-on-image image-number account-number vote-value)
      (error () (error 'webapp-error :text "Voting failed")))
    (<page>-redirect-to page (<request>-referer request))
    (setf (<page>-title page) "Vote Successful")
    (setf (<page>-content-field page :body)
          (html
            (:h3 (<page>-title page))
            "Vote Successful!"))
    (<request>-respond request (<page>-apply-template page *template*)) ) )
