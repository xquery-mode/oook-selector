(define-minor-mode oook-list-mode
  "Interact with document lists of oook"
  :lighter " Oook-List"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-m") 'xdmp-show-this)
            map))

(provide 'oook-list-mode)
