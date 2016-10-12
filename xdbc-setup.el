(require 'xdbc-selector)

;;; register selector as global key binding
(global-set-key "\C-cm" 'xdbc-selector)

(require 'cider-any-uruk-pprint)
(cider-any-uruk-pprint-mode)

(add-hook 'xquery-mode-hook 'cider-any-mode)

(when (featurep 'lambdawerk.marklogic)
  ;; (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-file))
  (fset 'xdmp-document-load (symbol-function 'xdmp-document-load/rest-from-string)))

(provide 'xdbc-setup)
