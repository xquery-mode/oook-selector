(provide 'xdmp-methods)

(defvar cider-any-uruk-connections
  '(("xdbc://localhost:8021/" "admin" "admin")
    ("xdbc://localhost:8022/" "admin" "admin")))

(defun xdmp-select-db (sel)
  "switch between two DBs." ;; this is clearly a hack
  (interactive "p")
  (let ((connection (nth (if (= sel 1) 0 1)
                         cider-any-uruk-connections)))
    (setq cider-any-uruk-uri          (first connection)
          cider-any-uruk-user         (second connection)
          cider-any-uruk-password     (third connection)
          cider-any-uruk-content-base (nth 3 connection))))


(defvar xdmp-document-history nil)

(defun xdmp-document-load (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) "")) nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (cider-any-string (format "
xquery version \"1.0-ml\";
xdmp:document-load(\"%s\",
                   <options xmlns=\"xdmp:document-load\">
                     <uri>%s%s</uri>
                     <repair>none</repair>
                     <permissions>{xdmp:default-permissions()}</permissions>
                  </options>)

"
                            (buffer-file-name)
                            (if (not (string-equal "" directory))
                                (file-name-as-directory directory)
                              "")
                            (buffer-name))))

(defun xdmp-document-delete (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (cider-any-string (format "
xquery version \"1.0-ml\";
xdmp:document-delete(\"%s%s\")

"
                            (if (not (string-equal "" directory))
                                (file-name-as-directory directory)
                              "")
                            (buffer-name))))

(defun xdmp-list-documents (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (cider-any-string (format "
xquery version \"1.0-ml\";
for $d in xdmp:directory(\"%s\",\"infinity\")
  return xdmp:node-uri($d)

"
                            (file-name-as-directory directory))))

(defun xdmp-show (&optional uri)
  (interactive
   (list
    (read-string (format "URI [%s]: " (or (car xdmp-document-history) ""))
                 nil
                 'xdmp-document-history
                 (car xdmp-document-history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (cider-any-string (format "
xquery version \"1.0-ml\";
doc(\"%s\")
"
                            uri)))

;; (global-set-key (kbd "C-c C-u") 'xdmp-document-load)
;; (global-set-key (kbd "C-c C-d") 'xdmp-document-delete)
;; (global-set-key (kbd "C-c C-q") 'xdmp-list-documents)
