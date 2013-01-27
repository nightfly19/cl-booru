(in-package :cl-booru)

;;;;
;;  Utility things
;;;;

(defun make-keyword (name)
  (when (stringp name)
    (intern name :KEYWORD) ) )

(defun object-slot-names (class)
  (mapcar #'sb-pcl:slot-definition-name
          (sb-pcl:class-slots class)))

(defun object-to-hash (instance &key (recursive t))
  (let ((new-hash (make-hash-table)))
    (mapc (lambda (slot-name)
            (setf (gethash (intern (string slot-name) :KEYWORD) new-hash) 
                  (if (and recursive (typep (slot-value instance slot-name) 'standard-object))
                    (object-to-hash (slot-value instance slot-name))
                    (slot-value instance slot-name))))
          (object-slot-names (class-of instance)))
    new-hash))

(defun object-to-json (instance)
  (json::encode-json-to-string
    (object-to-hash instance) ) )

(defun insert-between-elements (input-list new-element)
  :documentation "Inserts the thing x between elements of a copy of list x"
  (let ((new-list (copy-list input-list)))
    (when (cdr new-list)
      (progn
        (setf (cdr new-list) (append (list new-element) (cdr new-list)))
        (insert-between-elements (cddr new-list) new-element) ) )
    new-list) )

(defun image-full-file-path (hash ext)
  (merge-pathnames (concatenate 'string hash "." ext) *site-images-full*))

(defun image-thumb-file-path (hash &optional (ext "jpg"))
  (merge-pathnames (concatenate 'string hash "." ext) *site-images-thumb*))

(defun image-make-thumb (hash ext)
  (if (> 0 (sb-ext:process-exit-code
             (sb-ext:run-program
               "/usr/bin/convert"
               (list "-scale"
                     (format nil "~Dx~D"
                             *image-thumb-size*
                             *image-thumb-size*)
                     "-background" "white"
                     "-flatten"
                     "-format" "jpg"
                     "-quality" "85"
                     (format nil "~A[0]" (namestring (image-full-file-path hash ext)))
                     (namestring (image-thumb-file-path hash ext))))))
    nil
    t))

(defun twiddle-multipart (multi-part)
  (mapcar (lambda (part)
            (let ((content-dis (nth 2 (caadr part)))
                  (name))
              (setf name (cdr (assoc "name" content-dis :test #'string-equal)))
              (cons (make-keyword name)
                    (list
                      (cons :form-data content-dis)
                      (cons :content (car part)) ) ) ) ) multi-part) )

(defun db-connection-list ()
  (list *db-name* *db-user* *db-password* *db-host* :pooled-p t))

(defun env-value (key env)
  (let ((pair (assoc key env :test #'equalp)))
    (cdr pair) ) )

(defun update-tag-counts ()
  (let ((tags-with-counts
          (postmodern:query 
            (:order-by 
              (:select
                'tags.number
                (:as (:count 'image-tags.tag) 'tag-count)
                :from 'tags
                :inner-join 'image-tags
                :on (:= 'image-tags.tag 'tags.number)
                :group-by 'tags.number)
              'tag-count) :alists)))
    (mapc (lambda (tag)
            (postmodern:query
              (:update 'tags
                       :set 'count (cdr (assoc :tag-count tag))
                       :where (:= 'number (cdr (assoc :number tag)))))
            (cdr (assoc :number tags-with-counts)))
          tags-with-counts)))

(set-pprint-dispatch 'hash-table
                     (lambda (str ht)
                       (format str "{~{~{~S => ~S~}~^, ~}}"
                               (loop for key being the hash-keys of ht
                                     for value being the hash-values of ht
                                     collect (list key value)))))
