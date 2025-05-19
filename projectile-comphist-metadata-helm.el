;;; pchist-metadata-helm.el --- Helm interface for metadata management -*- lexical-binding: t; -*-

(require 'pchist-metadata)
(require 'helm)
(require 'helm-source)
(require 'helm-files)

(defun pchist-helm-artifact-paths (&optional input-root)
  "Helm interface to manage artifact paths for PROJECT-ROOT."
  (interactive)
  (let* ((project-root (pchist--get-project-root input-root)))
    (if (not project-root)
        (message "Not in a project")
      (let ((paths (pchist--get-metadata project-root :artifact-paths)))
        (helm :sources
              (helm-build-sync-source "Artifact Paths"
                :candidates paths
                :action
                (list
                 (cons "Copy to Clipboard" #'kill-new)
                 (cons "Remove" (lambda (path)
                                   (let ((project-root (pchist--get-project-root input-root)))
                                     (pchist-remove-artifact-path path project-root))))
                 (cons "Edit" (lambda (path)
                                 (let* ((project-root (pchist--get-project-root input-root))
                                        (new (read-string (format "New path for '%s': " path) path)))
                                   (pchist-edit-artifact-path path new project-root)))))
                :action-transformer
                (lambda (actions candidate)
                  (append actions nil))
                :coerce #'identity
                :persistent-action #'kill-new
                :persistent-help "Copy to kill ring")
              :buffer "*Helm Artifact Paths*")))))

(defun pchist-helm-add-artifact-path (&optional project-root)
  "Helm interface to browse and add an artifact path for PROJECT-ROOT using helm-read-file-name."
  (interactive)
  (let ((project-root (pchist--get-project-root project-root)))
    (if (not project-root)
        (message "Not in a project")
      (let ((file (helm-read-file-name "Select artifact path: " :initial-input (file-name-as-directory project-root))))
        (when (and file (file-exists-p file))
          (pchist-add-artifact-path file project-root)
          (message "Added artifact path: %s" file))))))

(provide 'pchist-metadata-helm)
;;; pchist-metadata-helm.el ends here
