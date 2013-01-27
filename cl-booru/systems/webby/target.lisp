(in-package :webby)

(defclass <target>()
  ((directory
     :initarg :directory
     :initform :||
     :accessor <target>-directory)
   (basename
     :initarg :basename
     :initform :||
     :accessor <target>-basename)
   (parameters
     :initarg :parameters
     :initform nil
     :accessor <target>-parameters)
   (fragment
     :initarg :fragment
     :initform ""
     :accessor <target>-fragment)))

(defgeneric <target>-parameter (target parameter))

(defun parse-target-path (target raw-path)
  (let ((path-parts (cadr (multiple-value-list
                            (cl-ppcre:scan-to-strings "^(.*)/([^/]*)$" raw-path)))))
    (when (eql nil path-parts)
      (setf path-parts (make-array '(1) :initial-contents `( ,raw-path))))
    (case (length path-parts)
      (2
       (setf (<target>-directory target) (make-keyword (aref path-parts 0)))
       (setf (<target>-basename    target) (make-keyword (aref path-parts 1))))
      (1
       (setf (<target>-directory target) :||)
       (setf (<target>-basename    target) (make-keyword (aref path-parts 0))))
      (0
       (setf (<target>-directory target) :||)
       (setf (<target>-basename    target) :||)))))

(defun parse-target-parameters (target raw-parameters)
  (let ((temp-parameters (split-sequence:split-sequence
                           #\& raw-parameters :remove-empty-subseqs t ) )
        final-parameters)
    (mapc (lambda (var)
            (let ((temp (split-sequence:split-sequence
                          #\= var
                          :remove-empty-subseqs t)))
              (when (<= 1 (length temp))
                (push (cons (make-keyword
                              (it.bese.arnesi::unescape-as-uri (car temp)) )
                            (when (stringp (cadr temp))
                              (it.bese.arnesi::unescape-as-uri (cadr temp)) ) )
                      final-parameters ) ) ) ) temp-parameters)
    (setf (<target>-parameters target) final-parameters)))

(defun parse-target (uri)
  (let* ((new-target (make-instance '<target>))
         (server-client (split-sequence:split-sequence
                          #\# uri :remove-empty-subseqs t ) )
         (path-parameters (split-sequence:split-sequence
                            #\? (car server-client) ) ))

    (parse-target-path       new-target (car path-parameters))
    (parse-target-parameters new-target (cadr path-parameters))

    (when (cadr server-client)
      (setf (<target>-fragment new-target)
            (it.bese.arnesi::unescape-as-uri (cadr server-client))))
    new-target ) )

(defmethod <target>-parameter ((target <target>) parameter)
  (let ((result (assoc parameter (<target>-parameters target))))
    (if result
      (cdr result)
      nil) ) )
