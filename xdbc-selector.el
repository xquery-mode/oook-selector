(require 'xdmp-methods)

;;;; xdbc selector

;; based on slime-selector, a.k.a. Slime's Buffer selector
;; see: https://github.com/slime/slime/blob/master/slime.el

;;;; License and Commentary

;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;     Copyright (C) 2016  Max-Gerd Retzlaff
;;
;;     For a detailed list of contributors, see the manual.
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(defvar xdbc-selector-methods nil
  "List of buffer-selection methods for the `xdbc-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defvar xdbc-selector-other-window nil
  "If non-nil use switch-to-buffer-other-window.")

(defun xdbc-selector (&optional other-window)
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-xdbc-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car xdbc-selector-methods)))
  (let* ((xdbc-selector-other-window other-window)
         (ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (cl-find ch xdbc-selector-methods :key #'car)))
    (cond (method
           (funcall (cl-third method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (xdbc-selector)))))

(defmacro def-xdbc-selector-method (key description &rest body)
  "Define a new `xdbc-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  (let ((method `(lambda ()
                   ,@body)))
    `(setq xdbc-selector-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key xdbc-selector-methods :key #'car))
                    #'< :key #'car))))

(def-xdbc-selector-method ?? "Selector help buffer"
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (cl-loop for (key line nil) in xdbc-selector-methods
             do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (xdbc-selector)
  (current-buffer))

;; (cl-pushnew (list ?4 "Select in other window" (lambda () (xdbc-selector t)))
;;             xdbc-selector-methods :key #'car)

(def-xdbc-selector-method ?q "Quit / abort"
  (top-level))


;;; custom methods

;;;; simple xquery evaluation

(def-xdbc-selector-method ?x
  "Evaluate an xquery from minibuffer"
  (call-interactively 'xdmp-query))

(def-xdbc-selector-method ?X
  "Evaluate an xquery from minibuffer in the modules database"
  (with-modules-database
   (call-interactively 'xdmp-query)))


;;;; LW configuration service
;; (Just ignore if you don't have such a service or don't know what it is.)

(def-xdbc-selector-method ?g
  "Get connection settings for ML connection from LW configuration service"
  (call-interactively 'xdmp-set-server/LW-conf))


;;;; document management

(def-xdbc-selector-method ?l
  "List documents (For paged output, set page limit with xdmp-set-page-limit.)"
  (call-interactively 'xdmp-list-documents))

(def-xdbc-selector-method ?L
  "List documents in the modules database"
  (with-modules-database
   (call-interactively 'xdmp-list-documents)))


(def-xdbc-selector-method ?s
  "Show document"
  (call-interactively 'xdmp-show))

(def-xdbc-selector-method ?S
  "Show document in the modules database"
  (with-modules-database
   (call-interactively 'xdmp-show)))

(def-xdbc-selector-method ?t ;; show "this" document, use in documents list
  "Show document at point"
  (xdmp-show (replace-regexp-in-string "\n$" "" (thing-at-point 'line))))

(def-xdbc-selector-method ?T ;; show "This" document, use in documents list
  "Show document at point in the modules database"
  (with-modules-database
   (xdmp-show (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))))

(def-xdbc-selector-method ?u
  "Upload a document"
  (call-interactively 'xdmp-document-load))

(def-xdbc-selector-method ?U
  "Upload a document in the modules database"
  (with-modules-database
   (call-interactively 'xdmp-document-load)))


(def-xdbc-selector-method ?d
  "Delete a document"
  (call-interactively 'xdmp-document-delete))

(def-xdbc-selector-method ?D
  "Delete a document in the modules database"
  (with-modules-database
   (call-interactively 'xdmp-document-delete)))


;;;; database selection

(def-xdbc-selector-method ?c ;; "choose"
  "Choose/select database within current session/connection "
  (call-interactively 'xdmp-select-database))

(def-xdbc-selector-method ?.
  "Select default database of the server"
  (xdmp-select-default-database))

(def-xdbc-selector-method ?,
  "Select modules database of the server"
  (xdmp-select-modules-database))

(def-xdbc-selector-method ?-
  "Show which database is currently used"
  (xdmp-show-current-database))
(def-xdbc-selector-method ?/
  "Show which database is currently used"
  (xdmp-show-current-database))

(provide 'xdbc-selector)
