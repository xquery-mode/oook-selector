(require 'xdbc-selector)

;;; register selector as global key binding
(global-set-key "\C-cm" 'xdbc-selector)

(require 'cider-any-uruk-pprint)

(add-hook 'xquery-mode-hook 'cider-any-mode)

(when (featurep 'ml-file-loading)
  ;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-file))
  (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-string)))

(provide 'xdbc-setup)
