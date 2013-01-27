(in-package :webby)

(defclass <request> ()
  ((environment
     :initarg :environment
     :initform nil
     :accessor <request>-environment )
   (cookies
     :initarg :cookies
     :initform nil
     :accessor <request>-cookies)
   (status
     :initarg :status
     :initform "200 OK"
     :accessor <request>-status)
   (headers
     :initarg :headers
     :initform '(("Content-Type" . "text/html"))
     :accessor <request>-headers)
   (target
     :initarg :target
     :initform (make-instance '<target>)
     :accessor <request>-target)
   (callback
     :initarg :callback
     :accessor <request>-callback ) ) )

(defgeneric <request>-add-header (request header))
(defgeneric <request>-post-raw (request))
(defgeneric <request>-post-as-string (request))
(defgeneric <request>-post-as-hash (request))
(defgeneric <request>-referer (request))
(defgeneric <request>-parse-cookies (request))
(defgeneric make-<request> (environment callback))
(defgeneric <request>-respond (request output))
(defgeneric <request>-respond-json (request output))

(defmethod <request>-add-header ((request <request>) header)
  (setf (<request>-headers request)
        (reverse
          (cons header (reverse (copy-list (<request>-headers request)))) ) ) )

(defmethod <request>-post-raw ((request <request>))
  (funcall (env-value :post-reader (<request>-environment request))))

(defmethod <request>-post-as-string ((request <request>))
  (coerce
    (map 'vector
         (lambda (item)
           (code-char item))
         (funcall
           (env-value
             :post-reader
             (<request>-environment request))))
    'string))

(defmethod <request>-post-as-hash ((request <request>))
  (let ((post-string (<request>-post-as-string request))
        (post-hash (make-hash-table))
        post-parts)
    (setf post-parts (split-sequence:split-sequence
                       #\&
                       post-string
                       :remove-empty-subseqs t) )
    (mapcar
      (lambda (var)
        (let ((pair (split-sequence:split-sequence #\= var)))
          (setf
            (gethash
              (make-keyword
                (it.bese.arnesi::unescape-as-uri
                  (string-trim '(#\Space)
                               (car pair)))) post-hash)
            (it.bese.arnesi::unescape-as-uri
              (string-trim '(#\Space) (cadr pair))))
          pair)) post-parts)
    post-hash))

(defmethod <request>-referer ((request <request>))
  (env-value "HTTP_REFERER" (<request>-environment request)))

(defmethod <request>-parse-cookies ((request <request>))
  (let (raw-cookies temp-cookies (cookies (make-hash-table)))
    (setf raw-cookies (env-value "HTTP_COOKIE" (<request>-environment request)))
    (setf temp-cookies (split-sequence:split-sequence #\; raw-cookies))
    (mapc
      (lambda (cookie)
        (let ((parts (split-sequence:split-sequence #\= cookie)))
          (setf (gethash
                  (make-keyword (string-trim '(#\Space) (car parts)))
                  cookies )
                (cadr parts) ) ) ) temp-cookies )
    (setf (<request>-cookies request) cookies) ) )

(defmethod make-<request> (environment callback)
  (let ((request (make-instance '<request>
                                :environment environment
                                :callback callback ) ) )
    (setf (<request>-target request)
          (parse-target (env-value "REQUEST_URI" (<request>-environment request))) )
    (<request>-parse-cookies request)
    request))

(defmethod <request>-respond ((request <request>) output)
  (funcall (<request>-callback request)
           (<request>-status request)
           (<request>-headers request))
  (if (listp output)
    output
    (list output) ) )

(defmethod <request>-respond-json ((request <request>) json)
  (setf (<request>-headers request)
        '(("Content-Type" . "application/json")) )
  (<request>-respond request json) )
