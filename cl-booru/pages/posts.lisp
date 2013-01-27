(in-package :cl-booru)

(defmethod webmethod ((directory (eql :||)) (basename (eql :|posts|)) request)
  (webmethod :|/posts| :|| request))

(defmethod webmethod ((directory (eql :|/posts|)) (basename (eql :||)) request)
  (webmethod :|/posts/list/page| :|1| request))
