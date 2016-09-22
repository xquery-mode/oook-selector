;; based on cider-read-and-eval
(defun cider-eval-form (form &optional namespace)
  "Read a sexp from the minibuffer and output its result to the echo area.
If NAMESPACE is non-nil, it is sent to cider as current namespace."
  (interactive)
  (let* ((override cider-interactive-eval-override)
         (ns-form (format "(ns %s)" (or namespace (cider-current-ns)))))
    (with-current-buffer (get-buffer-create cider-read-eval-buffer)
      (erase-buffer)
      (clojure-mode)
      (unless (string= "" ns-form)
        (insert ns-form "\n\n"))
      (insert form)

      (let ((cider-interactive-eval-override override))
        (cider-interactive-eval form)))))

(defun cider-eval-form/value (form &optional namespace)
  "Read a sexp from the minibuffer and output its result to the echo area.
If NAMESPACE is non-nil, it is sent to cider as current namespace."
  (interactive)
  (let* ((override cider-interactive-eval-override)
         (ns-form (format "(ns %s)" (or namespace (cider-current-ns)))))
    (with-current-buffer (get-buffer-create cider-read-eval-buffer)
      (erase-buffer)
      (clojure-mode)
      (unless (string= "" ns-form)
        (insert ns-form "\n\n"))
      (insert form)

      (let ((cider-interactive-eval-override override))
        (nrepl-dict-get
         (nrepl-sync-request:eval form (cider-current-connection) (cider-current-session))
         "value")))))
