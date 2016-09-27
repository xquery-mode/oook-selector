(require 'cider-any-uruk)

;; based on cider-read-and-eval
(defun cider-eval-form (form &optional namespace)
  "Read a sexp from the minibuffer and output its result to the echo area.
If NAMESPACE is non-nil, it is sent to cider as current namespace."
  (interactive)
  (let* ((override cider-interactive-eval-override)
         (ns-form (format "(ns %s)" (or namespace (cider-current-ns))))
         (new-form (with-current-buffer (get-buffer-create cider-read-eval-buffer)
                     (erase-buffer)
                     (clojure-mode)
                     (unless (string= "" ns-form)
                       (insert ns-form "\n\n"))
                     (insert form)

                     (buffer-string))))
    (let ((cider-interactive-eval-override override))
              (cider-interactive-eval new-form))))

(defun cider-eval-form/value (form &optional namespace)
  "Read a sexp from the minibuffer and output its result to the echo area.
If NAMESPACE is non-nil, it is sent to cider as current namespace."
  (interactive)
  (let* ((override cider-interactive-eval-override)
         (ns-form (format "(ns %s)" (or namespace (cider-current-ns))))
         (new-form
          (with-current-buffer (get-buffer-create cider-read-eval-buffer)
            (erase-buffer)
            (clojure-mode)
            (unless (string= "" ns-form)
              (insert ns-form "\n\n"))
            (insert form)

            (buffer-string))))
    (let ((cider-interactive-eval-override override))
      (nrepl-dict-get
       (nrepl-sync-request:eval form ;; should be  new-form  but there's a problem with multiple top-level forms
                                (cider-current-connection) (cider-current-session))
       "value"))))

(provide 'cider-eval-form)
