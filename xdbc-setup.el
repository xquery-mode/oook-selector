(provide 'xdbc-setup)

(require 'xquery-mode)
(require 'cider-any-uruk)
(add-hook 'xquery-mode-hook 'cider-any-mode)

(require 'cider-any-uruk-pprint)

(require 'xdbc-selector)
;;; register selector as global key binding
(global-set-key "\C-cm" 'xdbc-selector)
