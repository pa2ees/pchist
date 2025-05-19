;;; pchist-structured.el --- Structured compile history per project -*- lexical-binding: t; -*-

(require 'persist)
(require 'projectile)
(require 'cl-lib)

(defvar pchist-structured-history nil
  "Alist mapping project roots to metadata and command history.")

(persist-defvar pchist-structured-history nil
  "Persistent compile history per project, using structured entries.")

(defun pchist--load-history ()
  "Load the persistent compile history."
  (interactive)
  (persist-load 'pchist-structured-history))

(defun pchist--save-history ()
  "Save the persistent compile history."
  (persist-save 'pchist-structured-history))

(defun pchist--get-project-root (&optional project-root)
  "Return the given PROJECT-ROOT or the current Projectile project root."
  (or project-root (projectile-project-root)))

(defun pchist--get-project-entry (project-root)
  "Return the full entry plist for PROJECT-ROOT, or nil."
  (assoc project-root pchist-structured-history))

(defun pchist--get-or-create-project-entry (project-root)
  "Return the entry plist for PROJECT-ROOT, creating it if needed."
  (or (pchist--get-project-entry project-root)
      (let ((entry (cons project-root '(:metadata nil :commands nil))))
        (push entry pchist-structured-history)
        entry)))

(defun pchist--get-commands (project-root)
  "Get the list of commands for PROJECT-ROOT."
  (plist-get (cdr (pchist--get-or-create-project-entry project-root)) :commands))

(defun pchist--get-project-names ()
  "Get the list of projects"
  (mapcar #'car pchist-structured-history))

(defun pchist-add-structured-command (cmd-entry &optional project-root)
  "Add CMD-ENTRY to the history of PROJECT-ROOT, removing duplicates and saving."
  (pchist--load-history)
  (let* ((project-root (pchist--get-project-root project-root))
         (entry (pchist--get-or-create-project-entry project-root))
         (commands (plist-get (cdr entry) :commands)))
    (setq commands (cl-delete cmd-entry commands :test #'equal))
    (push cmd-entry commands)
    (setf (plist-get (cdr entry) :commands) commands)
    (pchist--save-history)))

(defun pchist--get-metadata (project-root key)
  "Get metadata KEY for PROJECT-ROOT."
  (plist-get (plist-get (cdr (pchist--get-or-create-project-entry project-root)) :metadata) key))

(defun pchist--set-metadata (project-root key value)
  "Set metadata KEY to VALUE for PROJECT-ROOT."
  (pchist--load-history)
  (let* ((entry (pchist--get-or-create-project-entry project-root))
         (meta (plist-get (cdr entry) :metadata)))
    (setf (plist-get (cdr entry) :metadata) (plist-put meta key value))
    (pchist--save-history)))

;; Actually load the history
(pchist--load-history)

(provide 'pchist-structured)
;;; pchist-structured.el ends here
