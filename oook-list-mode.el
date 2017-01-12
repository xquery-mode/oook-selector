(require 'view)

(defvar oook-list-mode-keymap
  nil
  "keymap for Oook-List mode")

(progn
  (setq oook-list-mode-keymap (make-sparse-keymap))
  (let ((map oook-list-mode-keymap))
    (define-key map (kbd "u") (lambda ()
                                (interactive)
                                (xdmp-with-database (xdmp-get-buffer-or-current-database)
                                                    (let ((path xdmp-buffer-path))
                                                      (if path
                                                          (xdmp-list-documents path)
                                                        (call-interactively 'xdmp-list-documents))))))
    (define-key map (kbd "C-m") 'xdmp-show-this)
    (define-key map (kbd "t") 'xdmp-show-this)
    (define-key map (kbd "d") 'xdmp-delete-this)))

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
