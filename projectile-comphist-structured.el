;;; projectile-comphist-structured.el --- Structured compile history per project -*- lexical-binding: t; -*-

(require 'persist)
(require 'projectile)
(require 'cl-lib)

(defvar project-comphist-structured-history nil
  "Alist mapping project roots to metadata and command history.")

(persist-defvar project-comphist-structured-history nil
  "Persistent compile history per project, using structured entries.")

(defun projectile-comphist--load-history ()
  "Load the persistent compile history."
  (interactive)
  (persist-load 'project-comphist-structured-history))

(defun projectile-comphist--save-history ()
  "Save the persistent compile history."
  (persist-save 'project-comphist-structured-history))

(defun projectile-comphist--get-project-root (&optional project-root)
  "Return the given PROJECT-ROOT or the current Projectile project root."
  (or project-root (projectile-project-root)))

(defun projectile-comphist--get-project-entry (project-root)
  "Return the full entry plist for PROJECT-ROOT, or nil."
  (assoc project-root project-comphist-structured-history))

(defun projectile-comphist--get-or-create-project-entry (project-root)
  "Return the entry plist for PROJECT-ROOT, creating it if needed."
  (or (projectile-comphist--get-project-entry project-root)
      (let ((entry (cons project-root '(:metadata nil :commands nil))))
        (push entry project-comphist-structured-history)
        entry)))

(defun projectile-comphist--get-commands (project-root)
  "Get the list of commands for PROJECT-ROOT."
  (plist-get (cdr (projectile-comphist--get-or-create-project-entry project-root)) :commands))

(defun projectile-comphist--get-project-names ()
  "Get the list of projects"
  (mapcar #'car project-comphist-structured-history))

(defun projectile-comphist-add-structured-command (cmd-entry &optional project-root)
  "Add CMD-ENTRY to the history of PROJECT-ROOT, removing duplicates and saving."
  (projectile-comphist--load-history)
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (entry (projectile-comphist--get-or-create-project-entry project-root))
         (commands (plist-get (cdr entry) :commands)))
    (setq commands (cl-delete cmd-entry commands :test #'equal))
    (push cmd-entry commands)
    (setf (plist-get (cdr entry) :commands) commands)
    (projectile-comphist--save-history)))

(defun projectile-comphist--get-metadata (project-root key)
  "Get metadata KEY for PROJECT-ROOT."
  (plist-get (plist-get (cdr (projectile-comphist--get-or-create-project-entry project-root)) :metadata) key))

(defun projectile-comphist--set-metadata (project-root key value)
  "Set metadata KEY to VALUE for PROJECT-ROOT."
  (projectile-comphist--load-history)
  (let* ((entry (projectile-comphist--get-or-create-project-entry project-root))
         (meta (plist-get (cdr entry) :metadata)))
    (setf (plist-get (cdr entry) :metadata) (plist-put meta key value))
    (projectile-comphist--save-history)))

;; Actually load the history
(projectile-comphist--load-history)

(provide 'projectile-comphist-structured)
;;; projectile-comphist-structured.el ends here
