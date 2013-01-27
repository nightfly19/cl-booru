(in-package :webby)

;;;;
;;  Utility things
;;;;

(defun make-keyword (name)
  (when (stringp name)
    (intern name :KEYWORD) ) )

(defun twiddle-multipart (multi-part)
  (mapcar (lambda (part)
            (let ((content-dis (nth 2 (caadr part)))
                  (name))
              (setf name (cdr (assoc "name" content-dis :test #'string-equal)))
              (cons (make-keyword name)
                    (list
                      (cons :form-data content-dis)
                      (cons :content (car part)) ) ) ) ) multi-part) )

(defun env-value (key env)
  (let ((pair (assoc key env :test #'equalp)))
    (cdr pair) ) )

(defun md5-hex-digest (md5)
  (let (output)
    (map 'vector (lambda (m-byte)
                   (push
                     (write-to-string (ash (logand m-byte #xF0) -4) :base 16)
                     output)
                   (push (write-to-string (logand m-byte #x0F) :base 16) output) )
         md5)
    (string-downcase
      (apply #'concatenate 'string
             (reverse output) ) ) ) )

(defun md5-hex-digest-string (input-string)
  (md5-hex-digest
    (md5:md5sum-sequence input-string) ) )

(defun is-filename-image (filename)
  (let ((match (multiple-value-list (cl-ppcre:scan-to-strings ".+\.(png|gif|jpe?g)$" filename))))
    (if (car match)
      (svref (cadr match) 0)
      nil)))

(defun set-cookie-header (cookie value)
  (cons "Set-Cookie" (format nil "~A=~A; Path=/"
                             (it.bese.arnesi::escape-as-uri
                               (string cookie) )
                             (it.bese.arnesi::escape-as-uri
                               (string value) ) )) )

(defmacro html (&rest body)
  `(cl-who:with-html-output-to-string
     (*standard-output* nil :prologue nil :indent 0)
     ,@body))

(defmacro html-ni (&rest body)
  `(cl-who:with-html-output-to-string
     (*standard-output* nil :prologue nil :indent nil)
     ,@body))
