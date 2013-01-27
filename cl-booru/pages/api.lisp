(in-package :cl-booru)

(defmethod webmethod ((directory (eql :|/api|)) (basename (eql :|vote-on-post|)) request)
  ;(when (<request>-target request)
  ;  (return-from api-action (call-next-method)) )
  (let (image-number image account-number vote vote-value vote-result)
    (setf vote
          (<target>-parameter (<request>-target request) :|vote|))
    (when (not (or (string-equal vote "up") (string-equal vote "down")))
      (error () (error 'error)) )
    (handler-case (setf image-number (parse-integer
                                       (<target>-parameter (<request>-target request) :|image|)))
      (error () (error 'error)) )
    (setf image (<image> image-number))
    (setf account-number (<session>-account (<session> request)))
    (setf vote-value (if (string-equal vote "up") t nil))
    (when (not image)
      (error () (error 'error)) )
    (handler-case (setf vote-result (vote-on-image image-number account-number vote-value))
      (error () (error 'error)) )
    (<request>-respond-json request
                       (json::encode-json-to-string (<image>-rating image-number)) ) ) )
