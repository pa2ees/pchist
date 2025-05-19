;;; projectile-comphist-metadata.el --- Inspect and modify compile history metadata -*- lexical-binding: t; -*-

(require 'projectile-comphist-structured)

(defun projectile-comphist-print-metadata (&optional project-root)
  "Print metadata for PROJECT-ROOT in a readable format."
  (interactive)
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (entry (projectile-comphist--get-project-entry project-root))
         (metadata (plist-get (cdr entry) :metadata)))
    (if metadata
        (message "%S" metadata)
      (message "No metadata found for project: %s" project-root))))

(defun projectile-comphist-list-metadata-keys (&optional project-root)
  "List all metadata keys for PROJECT-ROOT."
  (interactive)
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (entry (projectile-comphist--get-project-entry project-root))
         (metadata (plist-get (cdr entry) :metadata)))
    (if metadata
        (message "%s" (mapcar #'symbol-name (cl-loop for (k v) on metadata by #'cddr collect k)))
      (message "No metadata found for project: %s" project-root))))

(defun projectile-comphist-remove-metadata-key (key &optional project-root)
  "Remove metadata KEY from PROJECT-ROOT."
  (interactive "sMetadata key to remove: ")
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (entry (projectile-comphist--get-or-create-project-entry project-root))
         (metadata (plist-get (cdr entry) :metadata)))
    (setf (plist-get (cdr entry) :metadata) (cl-remf metadata (intern key)))
    (projectile-comphist--save-history)
    (message "Removed metadata key: %s" key)))

(defun projectile-comphist-add-artifact-path (path &optional project-root)
  "Add an artifact PATH to the :artifact-paths metadata for PROJECT-ROOT."
  (interactive "sArtifact path to add: ")
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (paths (projectile-comphist--get-metadata project-root :artifact-paths)))
    (unless (member path paths)
      (setq paths (cons path paths))
      (projectile-comphist--set-metadata project-root :artifact-paths paths)
      (message "Added artifact path: %s" path))))

(defun projectile-comphist-list-artifact-paths (&optional project-root)
  "List all artifact paths for PROJECT-ROOT."
  (interactive)
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (paths (projectile-comphist--get-metadata project-root :artifact-paths)))
    (if paths
        (message "%s" paths)
      (message "No artifact paths for project: %s" project-root))))

(defun projectile-comphist-remove-artifact-path (path &optional project-root)
  "Remove PATH from the artifact paths in PROJECT-ROOT."
  (interactive "sArtifact path to remove: ")
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (paths (remove path (projectile-comphist--get-metadata project-root :artifact-paths))))
    (projectile-comphist--set-metadata project-root :artifact-paths paths)
    (message "Removed artifact path: %s" path)))

(defun projectile-comphist-edit-artifact-path (old-path new-path &optional project-root)
  "Replace OLD-PATH with NEW-PATH in artifact paths for PROJECT-ROOT."
  (interactive "sOld path: \nsNew path: ")
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (paths (mapcar (lambda (p) (if (equal p old-path) new-path p))
                        (projectile-comphist--get-metadata project-root :artifact-paths))))
    (projectile-comphist--set-metadata project-root :artifact-paths paths)
    (message "Replaced %s with %s" old-path new-path)))

(provide 'projectile-comphist-metadata)
