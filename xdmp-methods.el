(defun xdmp-select-db (sel)
  "switch between two DBs." ;; this is clearly a hack
  (interactive "p")
  (if (= sel 1)
      (setq cider-any-uruk-uri "xdbc://localhost:8021/"
            cider-any-uruk-user "admin"
            cider-any-uruk-password "admin")
    (setq cider-any-uruk-uri "xdbc://localhost:8022/"
          cider-any-uruk-user "admin"
          cider-any-uruk-password "admin")))

(defun xdmp-document-load (&optional directory)
  (interactive
   (list
    (read-string (format "Directory [%s]: " (or (car xdmp-document-load/history) "")) nil
                 'xdmp-document-load/history
                 (car xdmp-document-load/history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (setq xdmp-document-load/last-dir directory)
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
    (read-string (format "Directory [%s]: " (or (car xdmp-document-delete/history) ""))
                 nil
                 'xdmp-document-delete/history
                 (car xdmp-document-delete/history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (setq xdmp-document-delete/last-dir directory)
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
    (read-string (format "Directory [%s]: " (or (car xdmp-list-documents/history) ""))
                 nil
                 'xdmp-list-documents/history
                 (car xdmp-list-documents/history))))
  (xdmp-select-db (prefix-numeric-value current-prefix-arg))
  (cider-any-string (format "
xquery version \"1.0-ml\";
for $d in xdmp:directory(\"%s\",\"infinity\")
  return xdmp:node-uri($d)

"
                            (file-name-as-directory directory))))

;; (global-set-key (kbd "C-c C-u") 'xdmp-document-load)
;; (global-set-key (kbd "C-c C-d") 'xdmp-document-delete)
;; (global-set-key (kbd "C-c C-q") 'xdmp-list-documents)
