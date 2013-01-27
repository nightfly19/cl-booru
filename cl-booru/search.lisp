(in-package :cl-booru)

(defun images-count (&optional excluded-categories)
  (postmodern:query
    (s-sql:sql-compile
      `(:select (:count *)
                :from images
                :where (:and
                         (:= active t)
                         ,@(when excluded-categories
                             `((:not-in category
                                        ,`(:set ,@excluded-categories) )) ) ) ) )
    :single ) )

(defun images-in-range (start-image image-count &optional excluded-categories)
  (postmodern:query-dao
    '<image>
    (s-sql:sql-compile
      `(:limit
         (:order-by
           (:select *
                    :from images
                    :where (:and
                             (:= active t)
                             ,@(when excluded-categories
                                 `((:not-in category
                                            ,`(:set ,@excluded-categories) )) ) ) )
           (:desc images.number) )
         ,image-count ,start-image ) ) ) )

(defun posts-in-range (start-image image-count &optional excluded-categories)
  (let ((results (make-instance '<result-set>)))
    (setf (<result-set>-offset results) start-image)
    (setf (<result-set>-total  results) (images-count excluded-categories))
    (setf (<result-set>-images results) (images-in-range start-image image-count excluded-categories))
    (setf (<result-set>-count results) (length (<result-set>-images results)))
    results ) )

(defun parse-wildcards (word)
  :documentation "Parses the wildcard characters that occur in a string 
  and return a list that represents the query"
  (let ((working-input (copy-seq word)))
    (if (search (string #\*) working-input)
      (progn 
        ;Flatten all wildcards into single instances of the chars
        (cl-ppcre:regex-replace-all "\\*+" working-input "*")
        (let ((parts nil))
          ;Handing wildcards at the beginning
          (when (eq #\* (char working-input 0)) (push "%" parts))
          ;Handle the messy things in the middle
          (let ((inner-parts
                  (split-sequence:split-sequence #\* working-input :remove-empty-subseqs t) ) )
            (if (eq 1 (length inner-parts))
              (setf parts (append parts inner-parts) )
              (setf parts (append parts (insert-between-elements inner-parts "%"))) ) )
          ;Handle wildcards at the end
          (when (eq #\* (char working-input (- (length working-input) 1)))
            (setf parts (append parts (list "%"))) )
          ;;Final output
          (apply #'concatenate 'string parts) ) )
      ;If there are no wildcards simply return the original word
      working-input ) ) )

(defun filter-bad-tags (query)
  :documentation "Filter out tag searches that are all wildcards"
  (let (filtered)
    (mapc (lambda (tag)
            (unless(equal (cadr tag) "%%")
              (push tag filtered) ) ) query )
    filtered ) )

(defun parse-tags (query)
  :documentation "Parses search tags (with wildcards) from string input"
  (let ((tags nil) (parts (split-sequence:split-sequence-if
                            (lambda (x) (member x '(#\Space #\Tab #\Newline)))
                            query :remove-empty-subseqs t ) ) )
    (mapc (lambda (word)
            (if (eq (char word 0) #\-)
              (when (< 1 (length word))
                (push (list :NOT (parse-wildcards (subseq word 1))) tags) )
              (push (list :AND (parse-wildcards word)) tags) ) ) parts )
    ;Return the list of tags
    (filter-bad-tags tags) ) )

(defun search-tags-by-name-query-builder (tag-strings)
  :documentation "Build a sql query from the given tag strings"
  `(:select number
            :from tags
            :where (:or
                     ,@(mapcar (lambda (tag)
                                 `(:ilike name ,tag) )
                               (if (listp tag-strings) tag-strings (list tag-strings) ) ) ) ) )

(defun search-tagged-images-with-tag-by-name-query-builder (tag-strings)
  `(:select
     image :from
     image_tags
     :where (:and
              (:= image_tags.active t)
              (:in
                image_tags.tag
                ,(search-tags-by-name-query-builder tag-strings) ))))

(defun search-images-by-search-string-query-builder (query &optional excluded-cats)
  (let (inclusion exclusion joins exclusion-query (index 0))

    ;;Name the subqueries that will be used to search for tags to be included
    (mapc (lambda (tag-string)
            (cond
              ((eq :AND (car tag-string))
               (progn
                 (push (list
                         (format nil "SUBQ_~A" index)
                         (cadr tag-string)) inclusion)
                 (incf index) ) )
              ((eq :NOT (car tag-string))
               (push (cadr tag-string) exclusion) )
              (t nil) ) )
          (parse-tags query) )

    ;; Build the where clause if there are tags to be excluded
    (setf exclusion-query
          (list (list :where
                      (list :and
                            (if excluded-cats
                              (list :not-in
                                    'images.category
                                    (concatenate 'list (list :set) excluded-cats) )
                              t)
                            (if exclusion
                              (list :not-in
                                    'images.number
                                    (search-tagged-images-with-tag-by-name-query-builder exclusion) ) 
                              t)) ) ))

    ;; Generate the joins used to search for the desired tags
    (mapc (lambda (inclusive-query)
            (push (list
                    :inner-join
                    (list :as
                          (search-tagged-images-with-tag-by-name-query-builder
                            (cadr inclusive-query))
                          (intern (car inclusive-query) :cl-booru) )
                    :on (list :=
                              'images.number
                              (intern
                                (format nil "~A.IMAGE" (car inclusive-query))
                                :cl-booru) ) )
                  joins) )
          inclusion)

    ;;Build the full query
    (list :order-by
          (apply #'concatenate 'list
                 (list :select
                       'images.number :from
                       'images)
                 (apply #'concatenate 'list joins) exclusion-query)
          (list :desc 'images.number)) ) )


(defun search-images-by-search-string (query &optional excluded-cats)
  (postmodern:query
    (s-sql:sql-compile
      (search-images-by-search-string-query-builder query excluded-cats))
    :column))

(defun image-search (query detailed-start number-of-images &optional (excluded-cats nil))
  (let ((results (make-instance '<result-set>))
        result-numbers)
    (setf result-numbers (search-images-by-search-string query excluded-cats))
    (setf (<result-set>-total results) (length result-numbers))
    (setf (<result-set>-offset results) detailed-start)
    (setf (<result-set>-images results)
          (images-in-set
            (subseq result-numbers
                    (if (> detailed-start 0)
                      detailed-start
                      0)
                    (if (< (+ detailed-start number-of-images) (<result-set>-total results))
                      (+ detailed-start number-of-images)
                      (<result-set>-total results) ) ) ) )
    (setf (<result-set>-count results) (length (<result-set>-images results)))
    results) )

