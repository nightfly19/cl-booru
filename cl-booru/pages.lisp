(in-package :cl-booru)

(load "views.lisp")

(defgeneric webmethod (directory basename request))

(load "pages/action/posts.lisp")
(load "pages/action/user.lisp")
(load "pages/api.lisp")
(load "pages/docs.lisp")
(load "pages/posts.lisp")
(load "pages/posts/list.lisp")
(load "pages/posts/search.lisp")
(load "pages/posts/upload.lisp")
(load "pages/posts/view.lisp")
(load "pages/user.lisp")

;;Doesn't match any known request
(defmethod webmethod (directory basename request)
  (webmethod nil nil request) )

;;Page not found
(defmethod webmethod ((directory null) (basename null) request)
  (error 'webapp-page-not-found) )

;;Home
(defmethod webmethod ((directory (eql :||)) (basename (eql :||)) request)
  (webmethod :|| :|posts| request))
