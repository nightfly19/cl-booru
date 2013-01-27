(in-package :cl-booru)
(start-server-in-thread (lambda (directory basename request)
                          (webmethod directory basename request) )
                        (lambda (error-object request)
                          (error-respond error-object request) )
                        :webapp-error-handler
                        (lambda (error-object request)
                          (webapp-error-respond error-object request) )
                        :db-connection-list
                        (list *db-name* *db-user* *db-password* *db-host* :pooled-p t))

