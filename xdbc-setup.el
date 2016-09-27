(require 'xdbc-selector)

;;; register selector as global key binding
(global-set-key "\C-cm" 'xdbc-selector)

(require 'cider-any-uruk-pprint)

(add-hook 'xquery-mode-hook 'cider-any-mode)

(provide 'xdbc-setup)
