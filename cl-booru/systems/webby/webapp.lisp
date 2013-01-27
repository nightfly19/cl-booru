(in-package :webby)
(defparameter *address* "0.0.0.0")
(defparameter *port* 9000)
(defparameter *threads* 16)
(defparameter *request-handler* (lambda (&rest the-rest)
                                  (<request>-respond (nth 2 the-rest) "Nadda")))
(defparameter *error-handler* (lambda (&rest the-rest) the-rest nil))
(defparameter *webapp-error-handler*
  (lambda (&rest the-rest)
    (apply *error-handler* the-rest)))
(defparameter *webapp-page-not-found-handler*
  (lambda (&rest the-rest)
    (apply *webapp-error-handler* the-rest)))
(defparameter *db-connection-list* nil)

(define-condition webapp-error (error)
  ((text   :initarg :text  :initform "Something bad happened" :reader text)
   (code   :initarg :code  :initform "400 Bad Request" :reader code)
   (title  :initarg :title :initform "Error" :reader title)) )

(define-condition webapp-page-not-found (webapp-error)
  ((text   :initarg :text  :initform "The page you requested could not be found" :reader text)
   (code   :initarg :code  :initform "404 Not Found" :reader code)
   (title  :initarg :title :initform "404 - Page Not Found" :reader title)) )

(defun inner-request-router (environment callback)
  (let ((request (make-<request> environment callback)))
  (handler-case (let ((target (<request>-target request)))
                  (funcall *request-handler*
                           (<target>-directory target)
                           (<target>-basename target)
                           request) )
    (webapp-page-not-found
      (error-object)
      (funcall *webapp-page-not-found-handler* error-object request))
    (webapp-error
      (error-object)
      (funcall *webapp-error-handler* error-object request))
    (error
      (error-object)
      (funcall *error-handler* error-object request)) ) ) )

(defun request-router (environment callback)
  :documentation "Parses the requested URI and call the action method"
  (if *db-connection-list*
    (postmodern:with-connection
      *db-connection-list*
      (inner-request-router environment callback))
    (inner-request-router environment callback)))

(defmacro start-server (
                        request-handler
                        error-handler
                        &key
                        webapp-error-handler
                        page-not-found-handler
                        db-connection-list
                        address port)
  :documentation "Starts the webapp"
  `(progn
     (format *error-output* "Starting webserver~%")
     (cl-fastcgi:socket-server-threaded
       (cl-fastcgi:make-serve-function
         (lambda(environment callback)
           (let ((*request-handler* ,request-handler)
                 (*error-handler* ,error-handler)
                 ,@(when webapp-error-handler
                     `((*webapp-error-handler* ,webapp-error-handler)))
                 ,@(when page-not-found-handler
                     `((*page-not-found-handler* ,page-not-found-handler)))
                 ,@(when db-connection-list
                     `((*db-connection-list* ,db-connection-list)) )
                 ,@(when address
                     `((*address* ,address)) )
                 ,@(when port
                     `((*port* ,port)) ))
             (request-router environment callback) ) ))
       :inet-addr *address*
       :port *port*
       :threads *threads* ) ))

(defmacro start-server-in-thread (&rest the-rest)
  :documentation "Starts the webapp in a new thread"
  `(sb-thread:make-thread
     (lambda () (start-server ,@the-rest))))
