(require 'cider-any-uruk)

(defun cider-eval-form (form &optional namespace)
  (cider-ensure-connected)
  (cider-nrepl-request:eval
   form
   (cider-interactive-eval-handler)
   (or namespace (cider-current-ns))))

(defun cider-eval-form/value (form &optional namespace)
  (cider-ensure-connected)
  (thread-first
      (cider-nrepl-sync-request:eval form namespace)
    (nrepl-dict-get "value")))

(provide 'cider-eval-form)
