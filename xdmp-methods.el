(require 'xquery-mode)
(require 'cider-any-uruk)
(require 'cider-eval-form)


;;;; functions to retrieve config from Clojure

(defvar xdmp-servers
  '(:rest-server (:host "localhost" :port "6000" :user "foo" :password "bar")
    :xdbc-server (:host "localhost" :port "7000" :user "foo" :password "bar")))

(defun xdmp-get-xdbc-server ()
  (plist-get xdmp-servers :xdbc-server))
(defun xdmp-get-rest-server ()
  (plist-get xdmp-servers :rest-server))

(defun xdmp-server-to-cider-any-uruk (server)
  (setq cider-any-uruk-connection server))

(defun xdmp-propagate-server-to-cider-any-uruk ()
  (xdmp-server-to-cider-any-uruk (xdmp-get-xdbc-server)))

;; make sure that cider-any-uruk has our current XDBC server configuration
(xdmp-propagate-server-to-cider-any-uruk)


;;;; functions to retrieve config from LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(defun xdmp-get-services/LW-conf ()
  (read (cider-eval-form/value "(keys (lambdawerk.marklogic.emacs/get-config))")))

(defun xdmp-set-server/LW-conf (service-name)
  (interactive (list (completing-read "Service: " (xdmp-get-services/LW-conf) nil t (cons ":marklogic-connection" 0))))
  (let ((servers (read (cider-eval-form/value (format "(lambdawerk.marklogic.emacs/get-config-for-emacs %s)" service-name)))))
    ;; set server in xdmp-servers
    (setq xdmp-servers servers)
    ;; also propagate to cider-any-uruk
    (xdmp-propagate-server-to-cider-any-uruk)))


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

;;;; functions to select databases

(defun xdmp-select-database (content-base)
  ;; also shows all databases because of the completion feature
  (interactive (list (completing-read "DB: " (xdmp-get-databases) nil t (cons (xdmp-get-default-database) 0))))
  (setq cider-any-uruk-connection (plist-put cider-any-uruk-connection :content-base content-base)))

(defun xdmp-select-default-database ()
  (interactive)
  (xdmp-select-database (xdmp-get-default-database)))

(defun xdmp-select-modules-database ()
  (interactive)
  (xdmp-select-database  (xdmp-get-modules-database)))

(defun xdmp-show-current-database ()
  (interactive)
  (message (xdmp-get-current-database)))


;;;; helper function to temporarily switch to the modules databse

(defvar xdmp-previous-database-stack nil)


(defun xdmp-switch-to-modules-database ()
  (push (xdmp-get-current-database) xdmp-previous-database-stack)
  (xdmp-select-modules-database))

(defun xdmp-maybe-switch-to-previous-database ()
  (let ((prev (pop xdmp-previous-database-stack)))
    (when prev
      (xdmp-select-database prev))))

(defmacro with-modules-database (&rest body)
  `(prog2
       (xdmp-switch-to-modules-database)
       (progn ,@body)
     (xdmp-maybe-switch-to-previous-database)))


;;;; main query function that wraps cider-any-eval

;; temporarily switches to modules database (using helper functions) when called with C-u (that is numerical prefix of 4)
(defun xdmp-query (string)
  "Eval an xquery -- temporarily switches to modules-database when called with C-u"
  (interactive "sQuery: ")
  (cider-any-eval string))


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
(defun xdmp-document-load/rest-from-file (&optional directory)
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
        (ns "lambdawerk.marklogic.rest-api"))
    (cider-eval-form form ns)))

;; load document using ml-file-loader api
(defun xdmp-document-load/rest-from-string (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (let ((form (format "(upload-document \"%s\" \"%s%s\" %s)"
                      (replace-regexp-in-string "\"" "\\\\\""
                                                (replace-regexp-in-string "\\\\" "\\\\\\\\" (buffer-string)))
                      (if (not (string-equal "" directory))
                          (file-name-as-directory directory)
                        "")
                      (buffer-name)
                      (xdmp-rest-connection->clj)))
        (ns "lambdawerk.marklogic.rest-api"))
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

(defvar xdmp-page-limit 1000)
(defun xdmp-set-page-limit (number)
  (interactive "NNew page limit: ")
  (setq xdmp-page-limit number))

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
let $message :=
  if ($limit) then
    (let $numpages := ceiling($count div $limit)
      return concat('Displaying results ', $offset, ' - ', $pageEnd, ' of ', $count, ' (Page ', $page, ' of ', $numpages, ')'))
  else
    concat('Displaying all ', $count, ' results')
return (
  $message
  ,
  fn:string-join( (for $d in (if ($limit) then subsequence($results,$offset,$limit) else $results)
                     return xdmp:node-uri($d)), '
'))
"
                        (file-name-as-directory directory)
                        limit
                        page))))

(defun xdmp-show (&optional uri)
  (interactive
   (list
    (read-string (format "URI [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (let ((fs-uri (expand-file-name (file-relative-name uri "/") default-directory)))
    (setq cider-any-uruk-buffer-filename fs-uri))
  (xdmp-query (format "
xquery version \"1.0-ml\";
doc(\"%s\")"
                      uri)))

;; (global-set-key (kbd "C-c C-u") 'xdmp-document-load)
;; (global-set-key (kbd "C-c C-d") 'xdmp-document-delete)
;; (global-set-key (kbd "C-c C-q") 'xdmp-list-documents)

(provide 'xdmp-methods)
