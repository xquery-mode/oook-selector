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

;;;; LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(oook-selector-defmethod ?g
  "Get connection settings for ML connection from LW configuration service"
  (call-interactively 'xdmp-set-server/LW-conf))


(provide 'oook-lambdawerk)
