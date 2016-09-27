(require 'xquery-mode)
(require 'cider-any-uruk)
(require 'cider-eval-form)

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


;;;; functions to retrieve config from clojure

(defun get-services/LW-conf ()
  (read (cider-eval-form/value "(keys (config-load))")))

(defun get-config/LW-conf (service-name)
  (interactive (list (completing-read "Service: " (get-services/LW-conf) nil t (cons ":xdbc-server" 0))))
  (let ((db (read (cider-eval-form/value (format "(config-load-for-emacs %s)" service-name)))))
    (setq cider-any-uruk-host          (plist-get db :host)
          cider-any-uruk-port          (plist-get db :port)
          cider-any-uruk-user          (plist-get db :user)
          cider-any-uruk-password      (plist-get db :password)
          cider-any-uruk-content-base  (plist-get db :content-base))
    (list cider-any-uruk-host
          cider-any-uruk-port
          cider-any-uruk-user
          cider-any-uruk-password
          cider-any-uruk-content-base)))


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

(defun xdmp-uruk-connection->clj ()
  (format "{:host \"%s\"
            :port \"%s\"
            :user \"%s\"
            :password \"%s\"
            :database \"%s\"}"
          cider-any-uruk-host
          7020 ;; cider-any-uruk-port
          cider-any-uruk-user
          cider-any-uruk-password
          (or cider-any-uruk-content-base (xdmp-get-default-database))))

;; load document using xquery
(defun xdmp-document-load (&optional directory)
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
(defun xdmp-document-load (&optional directory)
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
           (xdmp-uruk-connection->clj)))
        (ns "ml-file-loading.core"))
    (cider-eval-form form ns)))

;; xdmp:document-insert test
;;   turned out to be not nice at it expects node types not just strings for the content.
;; (defun xdmp-document-insert (&optional directory)
;;   (interactive
;;    (list
;;     (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
;;                  'xdmp-document-history
;;                  (car xdmp-document-history))))
;;   (xdmp-query (format "
;; xquery version \"1.0-ml\";
;; xdmp:document-insert(\"%s%s\",
;;                      text{\"%s\"},
;;                      xdmp:default-permissions(),
;;                      xdmp:default-collections())"
;;                       (if (not (string-equal "" directory))
;;                           (file-name-as-directory directory)
;;                         "")
;;                       (buffer-name)
;;                       (buffer-string))))

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
