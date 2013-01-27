(in-package :cl-booru)

(defmethod webmethod ((directory (eql :|/action/posts|)) (basename (eql :|upload|)) request)
  (let
      ((page (<page>))
       (post-data (<request>-post-as-string request))
       boundary part-form
       title source tags file filename digest ext image-number
       actual-tags)
    (handler-case
        (setf boundary
              (assoc "boundary"
                     (rfc2388:header-parameters
                      (rfc2388:parse-header
                       (env-value "HTTP_CONTENT_TYPE"
                                  (<request>-environment request)) :value))
                     :test #'string-equal))
      (error () (error 'webapp-error :text "Uploading file failed")))
    (when (> (length post-data) sb-fastcgi::*read-buffer-size*)
      (error () (error 'webapp-error :text "Attempting to upload to large of a file")))
    (setf part-form (twiddle-multipart (rfc2388:parse-mime post-data (cdr boundary) :write-content-to-file nil)))
    (setf title (cdr (assoc :content (cdr (assoc :|title| part-form)))))
    (setf source (cdr (assoc :content (cdr (assoc :|source| part-form)))))
    (setf tags (cdr (assoc :content (cdr (assoc :|tags| part-form)))))
    (setf filename (cdr (assoc "filename"
                               (cdr (assoc :form-data
                                           (cdr (assoc :|file| part-form)))) :test #'string-equal)))
    (setf file (cdr (assoc :content (cdr (assoc :|file| part-form)))))
    (setf digest (md5-hex-digest (md5:md5sum-sequence file)))
    (setf ext (is-filename-image filename))
    (when (image-by-checksum digest)
      (error 'webapp-error :text "Image is a repost"))
    (when (not ext)
      (error 'webapp-error :text "Image in not named as a gif png or jpeg"))
    (with-open-file (stream (image-full-file-path digest ext)
                            :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (map nil (lambda (s-char)
                 (write-byte (char-code s-char) stream))
           file))
    (when (not (image-make-thumb digest ext))
      (error 'webapp-error :text "Not a valid image"))
                                        ;(handler-case
                                        ;  (let ((new-image (postmodern:make-dao '<image>)))
                                        ;    (setf (<image>
                                        ;    (setf image-number (add-new-image
                                        ;                         digest ext title source
                                        ;                         (<session>-account (<request>-session request)) )))
                                        ;  (error () (error 'webapp-error :text "Could not add new image")) )
    (setf actual-tags
          (sort (cl-ppcre:all-matches-as-strings "\\S+" tags) #'string-lessp))
    (mapc (lambda(new-tag)
            (<image>-tag image-number new-tag)) actual-tags)
    (<page>-redirect-to page (format nil "/posts/view/~D" image-number))
    (setf (<page>-content-field page :body)
          (html
           (:h3 "Uploaded")
           "Image successfully uploaded!"))
    (<request>-respond request (<page>-apply-template page *template*)) ) )

(defmethod webmethod ((directory (eql :|/action/posts|)) (basename (eql :|edit|)) request)
  (let
      ((page (<page>))
       (post-data (<request>-post-as-hash request))
       changed desired-tags current-tags image)

    ;;Input checking
    (if (null (gethash :|post-number| post-data))
        (error 'webapp-error :text "Invalid post number")
        (handler-case (setf (gethash :|post-number| post-data)
                            (parse-integer (gethash :|post-number| post-data)))
          (error () (error 'webapp-error :text "Invalid post number"))))
    (handler-case (setf (gethash :|post-category| post-data)
                        (parse-integer (gethash :|post-category| post-data)))
      (error () (error 'webapp-error :text "Invalid Category")))
    (when (or (> 0 (gethash :|post-category| post-data))
              (< 3 (gethash :|post-category| post-data)))
      (error 'webapp-error :text "Invalid Category"))
    (when (null (gethash :|post-title| post-data))
      (error 'webapp-error :text "No title supplied"))
    (when (null (gethash :|post-source| post-data))
      (error 'webapp-error :text "No source supplied"))
    (when (null (gethash :|post-tags-box| post-data))
      (error 'webapp-error :text "No tag data source supplied"))
    (setf desired-tags
          (sort (cl-ppcre:all-matches-as-strings
                 "\\S+"
                 (gethash :|post-tags-box| post-data)) #'string-lessp))
    (setf image (<image> (gethash :|post-number| post-data)))
    (setf current-tags
          (image-tags-names image))

    (handler-case
        (setf changed (postmodern:query
                       (s-sql:sql-compile
                        (concatenate 'list
                                     (list :update 'images :set
                                           'title (gethash :|post-title| post-data)
                                           'source_url (gethash :|post-source| post-data))
                                     (when (gethash :|post-category| post-data)
                                       (list 'category (gethash :|post-category| post-data)))
                                     (list :where
                                           (list := 'number (gethash :|post-number| post-data))
                                           :returning 'number))) :single))
      (error () (error 'webapp-error :text "Something went wrong while updating the post")))

    (when (null changed)
      (error 'webapp-error :text "Could not find post to edit"))
    (let (new-tags del-tags common-tags)
      (setf common-tags (intersection current-tags desired-tags :test #'string-equal))
      (setf new-tags (set-difference desired-tags current-tags :test #'string-equal))
      (setf del-tags (set-difference current-tags desired-tags :test #'string-equal))
      (handler-case (progn
                      (mapc (lambda(new-tag)
                              (<image>-tag
                               (gethash :|post-number| post-data)
                               new-tag))
                            new-tags)
                      (mapc (lambda(del-tag)
                              (<image>-untag
                               (<image> (parse-integer (gethash :|post-number| post-data)))
                               del-tag))
                            del-tags))
        (error () 'web-error :text "Something went wrong while editing the post"))
      (<page>-redirect-to page (<request>-referer request))
      (setf (<page>-content-field page :body)
            (html
             (:h3 "Post Edited!")
             "Your edits have been applied!"))
      (<request>-respond request (<page>-apply-template page *template*)) ) ))

(defmethod webmethod ((directory (eql :|/action/posts|)) (basename (eql :|comment|)) request)
  (let* ((page (<page>))
         (post-data (<request>-post-as-hash request))
         (session (<session> request))
         (account (<session>-account session)))
    (when (null (gethash :|post-number| post-data))
      (error 'webapp-error :text "No post number supplied"))
    (handler-case (setf (gethash :|post-number| post-data)
                        (parse-integer (gethash :|post-number| post-data)))
      (error () (error 'webapp-error :text "Invalid post number")))
    (when (null (gethash :|post-comment-box| post-data))
      (error 'webapp-error :text "No comment supplied"))
    (when (> 1 (length (gethash :|post-comment-box| post-data)))
      (error 'webapp-error :text "No comment supplied"))
    (when (gethash :|post-comment-anonymously| post-data)
      (setf account 0))
    (handler-case (postmodern:query
                   (:insert-into 'comments
                                 :set
                                 'image (gethash :|post-number| post-data)
                                 'account account
                                 'comment (gethash :|post-comment-box| post-data)))
      (error () (error 'webapp-error :text "There was a problem commenting")))

    (<page>-redirect-to page (<request>-referer request))
    (setf (<page>-content-field page :body)
          (html
           (:h3 "Comment Posted")
           "Your Comment has been posted!"))
    (<request>-respond request (<page>-apply-template page *template*)) ) )

(defmethod webmethod ((directory (eql :|/action/posts/vote/up|)) basename request)
  (web-vote-on-image basename :|up| request))

(defmethod webmethod ((directory (eql :|/action/posts/vote/down|)) basename request)
  (web-vote-on-image basename :|down| request))

(defmethod webmethod ((directory (eql :|/action/posts|)) (basename (eql :|edit|)) request)
  (let ((page (<page>))
        (session (<session> request)))
    (setf (<request>-status request) "503 Service Unavailable")
    (render-session-info session page)
    (setf (<page>-content-field page :body)
          (html
           (:h3 "Uploading of Files Currently Disabled")
           "Sorry for any inconveniance, but editing posts is disabled at this time.") )

    (<request>-respond request (<page>-apply-template page *template*))))
