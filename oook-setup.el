(let ((default-directory  (file-name-directory load-file-name)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'oook-selector)

;;; register selector as global key binding
(global-set-key "\C-cm" 'oook-selector)

(require 'oook-pprint)
(oook-pprint-mode)
(require 'oook-to-file)

(add-hook 'xquery-mode-hook 'oook-mode)

;; (require 'oook-unzip-binaries) ;; activate if wanted

(when (featurep 'lambdawerk.marklogic)
  (require 'oook-lambdawerk))

(provide 'oook-setup)
