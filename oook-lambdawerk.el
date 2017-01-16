(require 'oook-selector)
;; This file contains an extension named oook-lambdawerk.
;; It uses some unpublished services. Just ignore it if you don't have them.


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


;;; document load / delete / list / show

;; load document using ml-file-loader api
(defun xdmp-document-load/rest-from-file (&optional directory)
  (interactive
   (list
    (let ((default (or xdmp-buffer-path (car xdmp-document-history))))
      (read-string (format "Directory [%s]: " (or default "")) nil
                   'xdmp-document-history
                   default))))
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
   (prog1
       (let* ((local-uri (buffer-file-name))
              (filename (file-name-nondirectory (buffer-file-name)))
              (directory (if (not (string-equal "" directory))
                             (file-name-as-directory directory)
                           ""))
              (server-uri (concat directory filename))
              (form (format "(upload-document \"%s\" \"%s\" %s)"
                            local-uri
                            server-uri
                            (xdmp-rest-connection->clj)))
              (ns "lambdawerk.marklogic.calls"))
         (cider-eval-form form ns))
     (xdmp-set-buffer-database (xdmp-get-current-database))
     (xdmp-set-buffer-path directory))))

;; load document using ml-file-loader api
(defun xdmp-document-load/rest-from-string (&optional directory)
  (interactive
   (list
    (let ((default (or xdmp-buffer-path (car xdmp-document-history))))
      (read-string (format "Directory [%s]: " (or default "")) nil
                   'xdmp-document-history
                   default))))
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
   (prog1
       (let* ((local-uri (buffer-file-name))
              (filename (file-name-nondirectory (buffer-file-name)))
              (directory (if (not (string-equal "" directory))
                             (file-name-as-directory directory)
                           ""))
              (server-uri (concat directory filename))
              (form (format "(upload-document \"%s\" \"%s\" %s)"
                            (replace-regexp-in-string "\"" "\\\\\""
                                                      (replace-regexp-in-string "\\\\" "\\\\\\\\" (buffer-string)))
                            server-uri
                            (xdmp-rest-connection->clj)))
              (ns "lambdawerk.marklogic.calls"))
         (cider-eval-form form ns)))
   (xdmp-set-buffer-database (xdmp-get-current-database))
   (xdmp-set-buffer-path directory)))

;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/xquery))
;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-file))
;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-string))
(when (featurep 'lambdawerk.marklogic)
  ;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-file))
  (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-string)))


;;;; LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(oook-selector-defmethod ?g
  "Get connection settings for ML connection from LW configuration service"
  (call-interactively 'xdmp-set-server/LW-conf))


(provide 'oook-lambdawerk)
