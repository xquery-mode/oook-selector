(require 'xquery-mode)
(require 'cider-any-uruk)
(require 'cider-eval-form)


;;;; functions to retrieve config from Clojure

(defvar xdmp-servers
  '(:rest-server
   (:host "localhost" :port "6000" :user "foo" :password "bar")
   :xdbc-server
   (:host "localhost" :port "7000" :user "foo" :password "bar")))

(defun xdmp-get-xdbc-server ()
  (plist-get xdmp-servers :xdbc-server))
(defun xdmp-get-rest-server ()
  (plist-get xdmp-servers :rest-server))

(defun xdmp-server-to-cider-any-uruk (server)
  (setq cider-any-uruk-host          (plist-get server :host)
        cider-any-uruk-port          (plist-get server :port)
        cider-any-uruk-user          (plist-get server :user)
        cider-any-uruk-password      (plist-get server :password)
        cider-any-uruk-content-base  (plist-get server :content-base))
  (list cider-any-uruk-host
        cider-any-uruk-port
        cider-any-uruk-user
        cider-any-uruk-password
        cider-any-uruk-content-base))

(defun xdmp-propagate-server-to-cider-any-uruk ()
  (xdmp-server-to-cider-any-uruk (xdmp-get-xdbc-server)))

;; make sure that cider-any-uruk has our current XDBC server configuration
(xdmp-propagate-server-to-cider-any-uruk)


;;;; functions to retrieve config from LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(defun xdmp-get-services/LW-conf ()
  (read (cider-eval-form/value "(keys (config-load))")))

(defun xdmp-set-server/LW-conf (service-name)
  (interactive (list (completing-read "Service: " (xdmp-get-services/LW-conf) nil t (cons ":xdbc-server" 0))))
  (let ((server (read (cider-eval-form/value (format "(config-load-for-emacs %s)" service-name)))))

    (let ((set-xdbc-server-p (/= 4 (prefix-numeric-value current-prefix-arg)))) ;; prefix set

      ;; set server in xdmp-servers
      (setq xdmp-servers
            (plist-put xdmp-servers
                       (if set-xdbc-server-p :xdbc-server :rest-server)
                       server))

      ;; also propagate to cider-any-uruk
      (when set-xdbc-server-p
        (xdmp-server-to-cider-any-uruk server)))))

(defun xdmp-set-servers/LW-conf ()
  (xdmp-set-server/LW-conf :xdbc-server)
  (setq current-prefix-arg '(4)) ; C-u
  (xdmp-set-server/LW-conf :rest-server))


;;;; xdmp interface functions to query for databases

(defun xdmp-get-current-database ()
  ;; should be the same as in variable 'cider-any-uruk-content-base'
  (car (cider-any-string->list "xdmp:database-name(xdmp:database())")))

(defun xdmp-get-default-database ()
  (car (cider-any-string->list "xdmp:database-name(xdmp:server-database(xdmp:server()))")))

(defun xdmp-get-modules-database ()
  (car (cider-any-string->list "xdmp:database-name(xdmp:modules-database())")))

(defun xdmp-get-databases ()
  (cider-any-string->list "for $d in xdmp:databases() return xdmp:database-name($d)"))


;;;; functions to encapsulate the REST server for Clojure

(defun xdmp-maybe-add-current-database (server)
  (if (plist-member server :database)
      server
    (append server (list :database (xdmp-get-current-database)))))

(defun xdmp-rest-connection->clj ()
  (cider-any-uruk-plist-to-map (xdmp-maybe-add-current-database (xdmp-get-rest-server))))

;; old method to directly use the cider-any-uruk connection
(defun xdmp-uruk-connection->clj-map ()
  (format "{:host \"%s\" :port \"%s\" :user \"%s\" :password \"%s\" :database \"%s\"}"
          cider-any-uruk-host
          cider-any-uruk-port
          cider-any-uruk-user
          cider-any-uruk-password
          (or cider-any-uruk-content-base (xdmp-get-current-database))))


;;;; functions to select databases

(defun xdmp-select-database (content-base)
  ;; also shows all databases because of the completion feature
  (interactive (list (completing-read "DB: " (xdmp-get-databases) nil t (cons (xdmp-get-default-database) 0))))
  (setq cider-any-uruk-content-base content-base))

(defun xdmp-select-default-database ()
  (interactive)
  (setq cider-any-uruk-content-base (xdmp-get-default-database)))

(defun xdmp-select-modules-database ()
  (interactive)
  (setq cider-any-uruk-content-base (xdmp-get-modules-database)))

(defun xdmp-show-current-database ()
  (interactive)
  (message (xdmp-get-current-database)))


;;;; helper function to temporarily switch to the modules databse

(defvar xdmp-previous-database-stack nil)
(defun xdmp-maybe-switch-to-modules-database () ;; silently uses the numerical prefix
  (let ((arg (prefix-numeric-value current-prefix-arg)))
    (when (= arg 4)
      (push (xdmp-get-current-database) xdmp-previous-database-stack)
      (xdmp-select-modules-database))))
(defun xdmp-maybe-switch-to-previous-database ()
  (let ((prev (pop xdmp-previous-database-stack)))
    (when prev
      (xdmp-select-database prev))))


;;;; main query function that wraps cider-any-eval

;; temporarily switches to modules database (using helper functions) when called with C-u (that is numerical prefix of 4)
(defun xdmp-query (string)
  "Eval an xquery -- temporarily switches to modules-database when called with C-u"
  (interactive "sQuery: ")
  (xdmp-maybe-switch-to-modules-database)
  (cider-any-eval string)
  (xdmp-maybe-switch-to-previous-database))


;;; document load / delete / list / show

(defvar xdmp-document-history nil)
;; idea: maybe use a separate history when temporarily switched to modules database (20160921 mgr)

;; load document using xquery
(defun xdmp-document-load/xquery (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-query (format "
xquery version \"1.0-ml\";
xdmp:document-load(\"%s\",
                   <options xmlns=\"xdmp:document-load\">
                     <uri>%s%s</uri>
                     <repair>none</repair>
                     <permissions>{xdmp:default-permissions()}</permissions>
                  </options>)"
                      (buffer-file-name)
                      (if (not (string-equal "" directory))
                          (file-name-as-directory directory)
                        "")
                      (buffer-name))))

;; load document using ml-file-loader api
(defun xdmp-document-load/rest (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (let ((form (format "(upload-document \"%s\" \"%s%s\" %s)"
           (buffer-file-name)
           (if (not (string-equal "" directory))
               (file-name-as-directory directory)
             "")
           (buffer-name)
           (xdmp-rest-connection->clj)))
        (ns "ml-file-loading.core"))
    (cider-eval-form form ns)))

(fset 'xdmp-document-load (symbol-function 'xdmp-document-load/xquery))

(defun xdmp-document-delete (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-query (format "
xquery version \"1.0-ml\";
xdmp:document-delete(\"%s%s\")"
                      (if (not (string-equal "" directory))
                          (file-name-as-directory directory)
                        "")
                      (buffer-name))))

(defun xdmp-list-documents (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-query (format "
xquery version \"1.0-ml\";
for $d in xdmp:directory(\"%s\",\"infinity\")
  return xdmp:node-uri($d)"
                      (file-name-as-directory directory))))

(defun xdmp-show (&optional uri)
  (interactive
   (list
    (read-string (format "URI [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-query (format "
xquery version \"1.0-ml\";
doc(\"%s\")"
                      uri)))

;; (global-set-key (kbd "C-c C-u") 'xdmp-document-load)
;; (global-set-key (kbd "C-c C-d") 'xdmp-document-delete)
;; (global-set-key (kbd "C-c C-q") 'xdmp-list-documents)

(provide 'xdmp-methods)
