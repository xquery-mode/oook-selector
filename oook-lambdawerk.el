(require 'oook-selector)
;; This file contains an extension named oook-lambdawerk.
;; It uses some unpublished services. Just ignore it if you don't have them.


;;;; functions to retrieve config from LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(defun xdmp-get-services/LW-conf ()
  (unless (featurep 'lambdawerk.marklogic)
    (error "Error: feature lambdawerk.marklogic not available, aborting command."))
  (let ((result (cider-eval-form/value "(do (require 'lambdawerk.marklogic.emacs) (keys (lambdawerk.marklogic.oook-selector/get-named-connection-specs)))")))
    (if result
        (read result)
      (error "Error: no connections available from LW configuration service, aborting command."))))

(defvar xdmp-server-history (list ":marklogic"))

(defun xdmp-set-server/LW-conf (service-name)
  (interactive (list (completing-read (format "Service (default %s): " (car xdmp-server-history)) (xdmp-get-services/LW-conf) nil t nil 'xdmp-server-history xdmp-server-history)))
  (let ((result (cider-eval-form/value (format "(lambdawerk.marklogic.oook-selector/get-connection-spec-for-emacs %s)" service-name))))
    (if result
        (let ((server (read result)))
          ;; setserver for oook
          (setq oook-connection server)
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
       (let* ((filename (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
              (directory (if (not (string-equal "" directory))
                             (file-name-as-directory directory)
                           ""))
              (server-uri (concat directory filename))
              (form (format "(put-document \"%s\" \"%s\" %s)"
                            (replace-regexp-in-string "\"" "\\\\\""
                                                      (replace-regexp-in-string "\\\\" "\\\\\\\\" (buffer-string)))
                            server-uri
                            (xdmp-rest-connection->clj)))
              (ns "lambdawerk.marklogic.calls"))
         (cider-eval-form form ns)))
   (xdmp-set-buffer-database (xdmp-get-current-database))
   (xdmp-set-buffer-path directory)))

;; load document using new uruk/insert-string from Uruk 0.3.7
(defun xdmp-document-load/uruk-insert-string (&optional directory)
  (interactive
   (list
    (let ((default (or xdmp-buffer-path (car xdmp-document-history))))
      (read-string (format "Directory [%s]: " (or default "")) nil
                   'xdmp-document-history
                   default))))
  (xdmp-with-database (xdmp-get-buffer-or-current-database)
   (prog1
       (let* ((filename (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
              (directory (if (not (string-equal "" directory))
                             (file-name-as-directory directory)
                           ""))
              (server-uri (concat directory filename))
              (eval-form (format "(let [host \"%s\"
                                        port %s
                                        db %s]
                                    (with-open [session (uruk.core/create-default-session (uruk.core/make-hosted-content-source host port db))]
                                      (doall (map str (uruk.core/insert-string session \"%%s\" \"%%s\")))))"
                                 (plist-get oook-connection :host)
                                 (plist-get oook-connection :port)
                                 (oook-plist-to-map oook-connection)))
              (form (format eval-form
                            server-uri
                            (replace-regexp-in-string "\"" "\\\\\""
                                                      (replace-regexp-in-string "\\\\" "\\\\\\\\" (buffer-string)))))
              (ns "uruk.core"))
         (cider-eval-form form ns)))
   (xdmp-set-buffer-database (xdmp-get-current-database))
   (xdmp-set-buffer-path directory)))

;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/xquery))
;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-file))
;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-string))
(when (featurep 'lambdawerk.marklogic)
  ;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-file))
  ;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-string))
  (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/uruk-insert-string)))


;;;; LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(oook-selector-defmethod ?g
  "Get connection settings for ML connection from LW configuration service"
  (call-interactively 'xdmp-set-server/LW-conf))


(provide 'oook-lambdawerk)
