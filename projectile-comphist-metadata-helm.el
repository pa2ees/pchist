;;; projectile-comphist-metadata-helm.el --- Helm interface for metadata management -*- lexical-binding: t; -*-

(require 'projectile-comphist-metadata)
(require 'helm)
(require 'helm-source)
(require 'helm-files)

(defun projectile-comphist-helm-artifact-paths (&optional input-root)
  "Helm interface to manage artifact paths for PROJECT-ROOT."
  (interactive)
  (let* ((project-root (projectile-comphist--get-project-root input-root)))
    (if (not project-root)
        (message "Not in a project")
      (let ((paths (projectile-comphist--get-metadata project-root :artifact-paths)))
        (helm :sources
              (helm-build-sync-source "Artifact Paths"
                :candidates paths
                :action
                (list
                 (cons "Copy to Clipboard" #'kill-new)
                 (cons "Remove" (lambda (path)
                                   (let ((project-root (projectile-comphist--get-project-root input-root)))
                                     (projectile-comphist-remove-artifact-path path project-root))))
                 (cons "Edit" (lambda (path)
                                 (let* ((project-root (projectile-comphist--get-project-root input-root))
                                        (new (read-string (format "New path for '%s': " path) path)))
                                   (projectile-comphist-edit-artifact-path path new project-root)))))
                :action-transformer
                (lambda (actions candidate)
                  (append actions nil))
                :coerce #'identity
                :persistent-action #'kill-new
                :persistent-help "Copy to kill ring")
              :buffer "*Helm Artifact Paths*")))))

(defun projectile-comphist-helm-add-artifact-path (&optional project-root)
  "Helm interface to browse and add an artifact path for PROJECT-ROOT using helm-read-file-name."
  (interactive)
  (let ((project-root (projectile-comphist--get-project-root project-root)))
    (if (not project-root)
        (message "Not in a project")
      (let ((file (helm-read-file-name "Select artifact path: " :initial-input (file-name-as-directory project-root))))
        (when (and file (file-exists-p file))
          (projectile-comphist-add-artifact-path file project-root)
          (message "Added artifact path: %s" file))))))

(provide 'projectile-comphist-metadata-helm)
;;; projectile-comphist-metadata-helm.el ends here
