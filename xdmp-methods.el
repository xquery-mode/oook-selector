(require 'xquery-mode)
(require 'oook)
(require 'cider-eval-form)
(require 'oook-list-mode)

;;;; functions to retrieve config from Clojure

(defvar xdmp-servers
  '(:rest-server (:host "localhost" :port "6000" :user "foo" :password "bar")
    :xdbc-server (:host "localhost" :port "7000" :user "foo" :password "bar")))

(defun xdmp-get-xdbc-server ()
  (plist-get xdmp-servers :xdbc-server))

(defun xdmp-get-rest-server ()
  (plist-get xdmp-servers :rest-server))

(defun xdmp-server-to-oook (server)
  (setq oook-connection server))

(defun xdmp-propagate-server-to-oook ()
  (xdmp-server-to-oook (xdmp-get-xdbc-server)))

;; make sure that oook has our current XDBC server configuration
(xdmp-propagate-server-to-oook)

;;;; functions to retrieve config from LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(defun xdmp-get-services/LW-conf ()
  (unless (featurep 'lambdawerk.marklogic)
    (error "Error: feature lambdawerk.marklogic not available, aborting command."))
  (let ((result (cider-eval-form/value "(do (require 'lambdawerk.marklogic.emacs) (keys (lambdawerk.marklogic.emacs/get-config)))")))
    (if result
        (read result)
      (error "Error: no connections available from LW configuration service, aborting command."))))

(defun xdmp-set-server/LW-conf (service-name)
  (interactive (list (completing-read "Service: " (xdmp-get-services/LW-conf) nil t (cons ":default" 0))))
  (let ((result (cider-eval-form/value (format "(lambdawerk.marklogic.emacs/get-config-for-emacs %s)" service-name))))
    (if result
        (let ((servers (read result)))
          ;; set server in xdmp-servers
          (setq xdmp-servers servers)
          ;; also propagate to oook
          (xdmp-propagate-server-to-oook)
          ;; inform user
          (message "Connection %s selected." service-name))
      (error "Error: no connection specified, aborting command."))))


;;;; xdmp interface functions to query for databases

(defun xdmp-get-current-database ()
  ;; should be the same as in variable 'oook-content-base'
  (car (oook-eval-sync "xdmp:database-name(xdmp:database())")))

(defun xdmp-get-default-database ()
  (car (oook-eval-sync "xdmp:database-name(xdmp:server-database(xdmp:server()))")))

(defun xdmp-get-modules-database ()
  (car (oook-eval-sync "xdmp:database-name(xdmp:modules-database())")))

(defun xdmp-get-databases ()
  (oook-eval-sync "for $d in xdmp:databases() return xdmp:database-name($d)"))


;;;; functions to encapsulate the REST server for Clojure

(defun xdmp-maybe-add-current-database (server)
  (if (plist-member server :database)
      server
    (append server (list :database (xdmp-get-current-database)))))

(defun xdmp-rest-connection->clj ()
  (oook-plist-to-map (xdmp-maybe-add-current-database (xdmp-get-rest-server))))

;;;; functions to select databases

(defun xdmp-select-database (content-base)
  ;; also shows all databases because of the completion feature
  (interactive (list (completing-read "DB: " (xdmp-get-databases) nil t (cons (xdmp-get-default-database) 0))))
  (setq oook-connection (plist-put oook-connection :content-base content-base)))

(defun xdmp-select-default-database ()
  (interactive)
  (xdmp-select-database (xdmp-get-default-database)))

(defun xdmp-select-modules-database ()
  (interactive)
  (xdmp-select-database (xdmp-get-modules-database)))

(defun xdmp-show-current-database ()
  (interactive)
  (message (xdmp-get-current-database)))


;;;; helper function to temporarily switch to the modules databse

(defvar xdmp-previous-database-stack nil)

(defun xdmp-switch-to-database (database)
  (push (xdmp-get-current-database) xdmp-previous-database-stack)
  (xdmp-select-database database))

(defun xdmp-switch-to-modules-database ()
  (xdmp-switch-to-database (xdmp-get-modules-database)))

(defun xdmp-maybe-switch-to-previous-database ()
  (let ((prev (pop xdmp-previous-database-stack)))
    (when prev
      (xdmp-select-database prev))))

(defmacro xdmp-with-database (database &rest body)
  `(prog2
       (xdmp-switch-to-database ,database)
       (progn ,@body)
     (xdmp-maybe-switch-to-previous-database)))

(defmacro xdmp-with-modules-database (&rest body)
  `(xdmp-with-database (xdmp-get-modules-database)
     ,@body))


;;;; main query function that wraps oook-eval

(defun xdmp-query (string &rest args)
  "Eval an xquery -- temporarily switches to modules-database when called with C-u"
  (interactive "sQuery: ")
  (let ((filename (plist-get args :filename))
        (args (plist-put args :eval-in-buffer `(progn
                                                 (xdmp-set-buffer-database ,(xdmp-get-current-database))
                                                 ,(plist-get args :eval-in-buffer)))))
    (if filename
        (apply 'oook-eval string #'oook-eval-to-file-handler nil args)
      (apply 'oook-eval string oook-eval-handler nil args))))

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
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
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
                                          (buffer-name)))))

;; load document using ml-file-loader api
(defun xdmp-document-load/rest-from-file (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
   (let ((form (format "(upload-document \"%s\" \"%s%s\" %s)"
                       (buffer-file-name)
                       (if (not (string-equal "" directory))
                           (file-name-as-directory directory)
                         "")
                       (buffer-name)
                       (xdmp-rest-connection->clj)))
         (ns "lambdawerk.marklogic.calls"))
     (cider-eval-form form ns))))

;; load document using ml-file-loader api
(defun xdmp-document-load/rest-from-string (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
   (let ((form (format "(upload-document \"%s\" \"%s%s\" %s)"
                       (replace-regexp-in-string "\"" "\\\\\""
                                                 (replace-regexp-in-string "\\\\" "\\\\\\\\" (buffer-string)))
                       (if (not (string-equal "" directory))
                           (file-name-as-directory directory)
                         "")
                       (buffer-name)
                       (xdmp-rest-connection->clj)))
         (ns "lambdawerk.marklogic.calls"))
     (cider-eval-form form ns))))

(fset 'xdmp-document-load (symbol-function 'xdmp-document-load/xquery))

(defun xdmp-document-delete (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
   (xdmp-query (format "
xquery version \"1.0-ml\";
xdmp:document-delete(\"%s%s\")"
                      (if (not (string-equal "" directory))
                          (file-name-as-directory directory)
                        "")
                      (buffer-name)))))

(defvar xdmp-page-limit 1000)
(defun xdmp-set-page-limit (number)
  (interactive "NNew page limit: ")
  (setq xdmp-page-limit number))

(defvar xdmp-buffer-database
  nil
  "variable to hold the buffer's xdmp-database")

(defun xdmp-get-buffer-or-current-database ()
  (interactive)
  (or xdmp-buffer-database
      (xdmp-get-current-database)))

(defun xdmp-set-buffer-database (database)
  ;; also shows all databases because of the completion feature
  (interactive (list (completing-read "DB: " (xdmp-get-databases) nil t (cons (xdmp-get-buffer-or-current-database) 0))))
  (make-local-variable 'xdmp-buffer-database)
  (setq xdmp-buffer-database
        database))

(defun xdmp-list-documents (&optional directory)
  "List documents (For paged output, set page limit with xdmp-set-page-limit.)
Use numerical prefix to switch to a different page.
(Cannot list documents with an URL not beginning with a '/'.)"
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (let ((page (prefix-numeric-value current-prefix-arg))
        (limit (or xdmp-page-limit 0)))
    (xdmp-query (format "
xquery version \"1.0-ml\";
let $results := xdmp:directory(\"%s\",\"infinity\")
let $count := count($results)
let $limit := %s
let $page := %s
let $offset := 1 + ($page - 1) * $limit
let $pageEnd := min(($offset + $limit - 1,$count))
let $database := xdmp:database-name(xdmp:database())
let $message :=
  if ($limit) then
    (let $numpages := ceiling($count div $limit)
      return concat('Displaying results ', $offset, ' - ', $pageEnd, ' of ', $count, ' (Page ', $page, ' of ', $numpages, ')'))
  else
    concat('Displaying all ', $count, ' results')
let $message := concat ($message, ' from DB: ', $database, ', path: %s')
return (
  $message
  ,
  fn:string-join( (for $d in (if ($limit) then subsequence($results,$offset,$limit) else $results)
                     return xdmp:node-uri($d)), '
'))
"
                        (file-name-as-directory directory)
                        limit
                        page
                        (file-name-as-directory directory))
  :eval-in-buffer `(oook-list-mode))))

(defun xdmp-show (&optional uri)
  (interactive
   (list
    (read-string (format "URI [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (let ((fs-uri (file-relative-name uri "/")))
    ;; TODO(m-g-r): this is unnecessary with new `oook-to-file' approach.
    ;;(setq oook-buffer-filename fs-uri)
    (xdmp-with-database (xdmp-get-buffer-or-current-database)
     (xdmp-query (format "
xquery version \"1.0-ml\";
doc(\"%s\")"
                        uri)
                 :filename fs-uri))))

(defun xdmp-show-this ()
  (interactive)
  (let ((uri (whitespace-delimited-thing-at-point)))
    (xdmp-show uri)))

;; (global-set-key (kbd "C-c C-u") 'xdmp-document-load)
;; (global-set-key (kbd "C-c C-d") 'xdmp-document-delete)
;; (global-set-key (kbd "C-c C-q") 'xdmp-list-documents)

(provide 'xdmp-methods)
