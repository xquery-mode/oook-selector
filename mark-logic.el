(provide 'mark-logic)

(require 'nxml-pretty-format)
(global-set-key (kbd "C-c C-p") 'nxml-pretty-format)

(require 'xquery-mode)
(require 'cider-any-uruk)
(add-hook 'xquery-mode-hook 'cider-any-mode)

(require 'cider-any-uruk-pprint)

(require 'xdmp-selector)
;;; register selector as global key binding
(global-set-key "\C-cm" 'xdbc-selector)
