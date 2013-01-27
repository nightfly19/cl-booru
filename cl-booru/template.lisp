(in-package :cl-booru)

(defparameter
  *template*
  (lambda (page)
    (cl-who:with-html-output-to-string
      (*standard-output* nil :prologue "<!DOCTYPE html>" :indent 0)
      (:html
        ;;Head Section
        (:head
          (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
          (:meta :name "description" :content 
                 (if (<page>-content-field page :description)
                   (<page>-content-field page :description)
                   +site-title+))
          (:link
            :rel "stylesheet"
            :type "text/css"
            :href "/resources/css/cl-booru.css" )
          (:script
            :type "text/javascript"
            :src "/resources/js/jquery-1.7.2.min.js" )
          (:script
            :type "text/javascript"
            :src "/resources/js/cl-booru.js" )
          (:link
            :rel "icon"
            :href "/favicon.ico")
          (:title
            (cl-who:esc (<page>-title page)) )
          (:script
            :type "text/javascript"
            :src "/resources/js/analytics.js" ) )

        (cl-who:str (<page>-content-field page :extra-header))
        ;;Body Section
        (:body
          (:header
            (:div :id "banner"
                  (:a :href "/"
                      (:h1 (cl-who:esc +site-title+)))
                  )
            (:div :id "banner-ad")
            (:div :id "session-info"
                  (cl-who:str (<page>-content-field page :session-info))))
          (:nav :id "nav-bar"
                (:form :action "/posts/search/tags"
                       :id "tag-search-form"
                       (:input
                         :type "search"
                         :value (cl-who:esc (<page>-content-field page :tag-search-query))
                         :placeholder "Tag Search"
                         :id "query")
                       (:ul (:li
                              (:a :id "search-submit"
                                  :href "/posts/search/tags" "Search") ) )
                       (:noscript
                         (:input
                           :type "submit"
                           :id "fallback-search-submit"
                           :value "Search")
                         (:style :type "text/css"
                                 "#search-submit{display:none;}") ) )
                (:ul :id "nav-links"
                     (:li (:a :href "/posts/list" "Posts"))
                     (:li (:a :href "/posts/upload" "Upload"))
                     ;(:li (:a :href "/user" "User"))
                     ) )
          (:div :id "content-frame"
                (:div :id "content-left"
                      (:div :id "content-left-upper-ad")
                      (:div :id "content-left-content"
                            (cl-who:str (<page>-content-field page :sidebar)) ))
                (:div :id "content-middle"
                      (:div :id "content-middle-frame"
                            (:div :id "content-middle-upper-ad")
                            (:div :id "content-middle-content"
                                  (cl-who:str (<page>-content-field page :body)) )
                            (:div :id "content-middle-lower-ad") ) )
                (:div :id "content-right") )
          (when (<page>-content-field page :error)
            (cl-who:htm
              "<!--"
              (cl-who:esc (write-to-string (<page>-content-field page :error)))
              "-->"))
          (:footer :id "footer"
                   (:a :href "/docs/terms-of-service" "Terms of Service")
                   "&bull;"
                   (:a :href "/docs/about" "About")
                   "&bull;"
                   (:a :href "/docs/contact" "Contact")
                   "&ndash;"
                   "Images &copy; their owners"
                   ) ) ) ) ) )
