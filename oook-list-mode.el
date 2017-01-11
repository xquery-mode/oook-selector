(defvar oook-list-mode-keymap
  (make-sparse-keymap)
  "keymap for Oook-List mode")

(let ((map oook-list-mode-keymap))
  (setq oook-list-mode-keymap (make-sparse-keymap))
  (define-key map (kbd "C-m") (lambda ()
                                (interactive)
                                (xdmp-with-database (xdmp-get-buffer-or-current-database)
                                 (xdmp-show-this)))))

(define-minor-mode oook-list-mode
  "Interact with document lists of oook"
  :lighter " Oook-List"
  :keymap oook-list-mode-keymap)

(provide 'oook-list-mode)
