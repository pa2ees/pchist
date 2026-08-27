;;; pchist2-ui.el --- Helm UI for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (helm "3.0"))

;;; Commentary:

;; This module provides the Helm-based UI for pchist v2, implementing:
;; - List interface for browsing and selecting command entries
;; - Commands for creating, editing, duplicating, and deleting entries
;; - Filter switching (global, specific project, current project)
;; - Command execution via projectile-run-compilation
;;
;; Main entry point: `pchist2-ui-select-command'

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'pchist2-ui-builder)
(require 'helm)
(require 'helm-source)
(require 'projectile)
(require 'cl-lib)

;;; Display Settings

(defvar pchist2-ui--current-filter 'current-project
  "Current filter mode: 'current-project, 'specific-project, or 'global.")

(defvar pchist2-ui--specific-project nil
  "Project path when filter is 'specific-project.")

;;; Formatting Functions

(defun pchist2-ui--format-command (cmd &optional short-paths)
  "Format CMD as a human-readable string for display.
If SHORT-PATHS is non-nil, show only basenames for project paths."
  (pchist2-format-command cmd short-paths))

(defun pchist2-ui--get-filter-description ()
  "Return a string describing the current filter."
  (pchist2-format-filter-description pchist2-ui--current-filter
                                     pchist2-ui--specific-project
                                     (projectile-project-root)))

;;; Command Retrieval

(defun pchist2-ui--get-filtered-commands ()
  "Get commands based on current filter settings."
  (pcase pchist2-ui--current-filter
    ('current-project
     (let ((project-root (projectile-project-root)))
       (if project-root
           (pchist2-get-commands project-root)
         (message "Not in a projectile project")
         nil)))
    ('specific-project
     (if pchist2-ui--specific-project
         (pchist2-get-commands pchist2-ui--specific-project)
       nil))
    ('global
     (pchist2-get-commands))))

;;; Helm Actions

(defun pchist2-ui--action-run (cmd)
  "Run the selected command CMD."
  (let ((formatted (pchist2-ui--format-command cmd nil))
        (default-directory (alist-get 'project cmd)))
    (projectile-run-compilation formatted)))

(defun pchist2-ui--action-delete (cmd)
  "Delete the selected command CMD."
  (when (yes-or-no-p (format "Delete command: %s? "
                             (pchist2-ui--format-command cmd t)))
    (pchist2-delete-command cmd)
    (message "Command deleted")))

(defun pchist2-ui--action-edit (cmd)
  "Edit the selected command CMD using the interactive builder."
  (let* ((project (alist-get 'project cmd))
         (built-cmd (pchist2-ui-build-command project cmd)))
    (when built-cmd
      (pchist2-update-command cmd
                              `((command . ,(alist-get 'command built-cmd))
                                (switches . ,(alist-get 'switches built-cmd))
                                (targets . ,(alist-get 'targets built-cmd))
                                (installers . ,(alist-get 'installers built-cmd))))
      (message "Command updated"))))

(defun pchist2-ui--action-duplicate (cmd)
  "Duplicate the selected command CMD and allow editing before saving."
  (let* ((project (alist-get 'project cmd))
         (built-cmd (pchist2-ui-build-command project cmd)))
    (when built-cmd
      (pchist2-add-command (alist-get 'project built-cmd)
                           (alist-get 'command built-cmd)
                           (alist-get 'switches built-cmd)
                           (alist-get 'targets built-cmd)
                           (alist-get 'installers built-cmd))
      (message "Command duplicated and saved"))))

;;; Filter Switching

(defun pchist2-ui--cycle-filter ()
  "Cycle through filter modes."
  (interactive)
  (with-helm-alive-p
    (setq pchist2-ui--current-filter
          (pcase pchist2-ui--current-filter
            ('current-project 'global)
            ('global 'specific-project)
            ('specific-project 'current-project)))

    ;; If switching to specific-project, prompt for project
    (when (eq pchist2-ui--current-filter 'specific-project)
      (let ((projects (pchist2-get-unique-projects)))
        (setq pchist2-ui--specific-project
              (helm-comp-read "Select project: " projects :must-match t))))

    (helm-update)))

;;; Helm Source

(defun pchist2-ui--build-source ()
  "Build the Helm source for command selection."
  (helm-build-sync-source (format "pchist2 [%s]" (pchist2-ui--get-filter-description))
    :candidates (lambda ()
                  (mapcar (lambda (cmd)
                            (cons (pchist2-ui--format-command cmd t) cmd))
                          (pchist2-ui--get-filtered-commands)))
    :action (helm-make-actions
             "Run command (RET)" #'pchist2-ui--action-run
             "Edit command (C-c e)" #'pchist2-ui--action-edit
             "Duplicate and modify (C-c d)" #'pchist2-ui--action-duplicate
             "Delete command (C-c k)" #'pchist2-ui--action-delete)
    :keymap (let ((map (make-sparse-keymap)))
              (set-keymap-parent map helm-map)
              (define-key map (kbd "C-c f") #'pchist2-ui--cycle-filter)
              (define-key map (kbd "C-c e") (lambda ()
                                               (interactive)
                                               (helm-exit-and-execute-action
                                                #'pchist2-ui--action-edit)))
              (define-key map (kbd "C-c d") (lambda ()
                                               (interactive)
                                               (helm-exit-and-execute-action
                                                #'pchist2-ui--action-duplicate)))
              (define-key map (kbd "C-c k") (lambda ()
                                               (interactive)
                                               (helm-exit-and-execute-action
                                                #'pchist2-ui--action-delete)))
              (define-key map (kbd "C-c n") (lambda ()
                                               (interactive)
                                               (helm-run-after-exit #'pchist2-ui-create-command)))
              map)))

;;; Entry Point

;;;###autoload
(defun pchist2-ui-select-command ()
  "Select and run a command from pchist2 history.

Key bindings:
  RET     - Run the selected command
  C-c f   - Cycle filter (current project / global / specific project)
  C-c e   - Edit selected command
  C-c d   - Duplicate and modify selected command
  C-c k   - Delete selected command
  C-c n   - Create new command"
  (interactive)
  (pchist2-load)
  ;; Reset filter to current-project by default
  (setq pchist2-ui--current-filter 'current-project)
  (let ((commands (pchist2-ui--get-filtered-commands)))
    (if (null commands)
        (progn
          (message "No commands in history. Create one now.")
          (pchist2-ui-create-command))
      (helm :sources (pchist2-ui--build-source)
            :buffer "*pchist2-select-command*"))))

(provide 'pchist2-ui)
;;; pchist2-ui.el ends here
