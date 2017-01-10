(defun whitespace-delimited-thing-at-point ()
  (interactive)
  (save-excursion
    (let ((beg (progn (forward-whitespace -1)
                      (skip-chars-forward "[:space:]")
                      (point)))
          (end (progn (forward-whitespace 1)
                      (skip-chars-backward "[:space:]")
                      (point))))
      (buffer-substring beg end))))

(provide 'oook-tools)
