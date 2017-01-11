(defvar oook-list-mode-keymap
  (make-sparse-keymap)
  "keymap for Oook-List mode")

(let ((map oook-list-mode-keymap))
  (setq oook-list-mode-keymap (make-sparse-keymap))
  (define-key map (kbd "C-m") 'xdmp-show-this)
  (define-key map (kbd "t") 'xdmp-show-this))

(define-minor-mode oook-list-mode
  "Interact with document lists of oook"
  :lighter " Oook-List"
  :keymap oook-list-mode-keymap)

;; make sure this mode is first in minor-mode-map-alist so that keybindings take precedence (esp. over those of view-mode)
;; from http://stackoverflow.com/a/5340797 but improved
(let ((mykeys (assq 'oook-list-mode minor-mode-map-alist)))
  (setq minor-mode-map-alist (assq-delete-all 'oook-list-mode minor-mode-map-alist))
  (add-to-list 'minor-mode-map-alist mykeys))

(provide 'oook-list-mode)
