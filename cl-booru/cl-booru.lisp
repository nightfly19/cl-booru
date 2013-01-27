(require :asdf)
(push "./systems/" asdf:*central-registry*)

;; Silently load packages
(let ((*standard-output* (make-broadcast-stream)))
  ;; Load quicklisp packages
  (ql:quickload "sb-fastcgi")
  (ql:quickload "cl-who")
  (ql:quickload "postmodern")
  (ql:quickload "simple-date")
  (ql:quickload "s-sql")
  (ql:quickload "cl-json")
  (ql:quickload "cl-ppcre")
  (ql:quickload "arnesi")
  (ql:quickload "rfc2388")
  (ql:quickload "md5")

  ;; Load local packages
  (ql:quickload "webby") )

(cl-fastcgi::load-libfcgi "/usr/lib/libfcgi.so.0.0.0")

;; Load componants that will become their own package someday
(load "packages.lisp")
(load "conf.lisp")
(load "utility.lisp")
(load "types.lisp")
(load "search.lisp")
(load "template.lisp")
(load "pages.lisp")
(load "error.lisp")

;Garbase collect after loading libraries
(sb-ext:gc :full t)

;Start the webserver
(load "samplebooru.lisp")
