;;; pchist2-ui-select.el --- Command selection UI for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides the command selection interface using tabulated-list-mode.
;; It displays commands in a table format with single-key commands for actions.

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'tabulated-list)
(require 'projectile)
(require 'cl-lib)

;;; Filter State

(defvar-local pchist2-select--filter 'current-project
  "Current filter mode: 'current-project, 'specific-project, or 'global.")

(defvar-local pchist2-select--specific-project nil
  "Project path when filter is 'specific-project.")

;;; Mode Definition

(defvar pchist2-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Actions
    (define-key map (kbd "RET") #'pchist2-select-run)
    (define-key map (kbd "e") #'pchist2-select-edit)
    (define-key map (kbd "c") #'pchist2-select-new)
    (define-key map (kbd "d") #'pchist2-select-duplicate)
    (define-key map (kbd "k") #'pchist2-select-delete)
    ;; Filter
    (define-key map (kbd "f") #'pchist2-select-cycle-filter)
    ;; Refresh
    (define-key map (kbd "g") #'pchist2-select-refresh)
    ;; Quit
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `pchist2-select-mode'.")

(define-derived-mode pchist2-select-mode tabulated-list-mode "pchist2-select"
  "Major mode for selecting and managing pchist2 commands.

\\{pchist2-select-mode-map}"
  (setq tabulated-list-format [("Command" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq buffer-read-only t)
  (add-hook 'tabulated-list-revert-hook #'pchist2-select--refresh nil t))

;;; Data Retrieval

(defun pchist2-select--get-filtered-commands ()
  "Get commands based on current filter settings."
  (pcase pchist2-select--filter
    ('current-project
     (let ((project-root (projectile-project-root)))
       (if project-root
           (pchist2-get-commands project-root)
         (message "Not in a projectile project")
         nil)))
    ('specific-project
     (if pchist2-select--specific-project
         (pchist2-get-commands pchist2-select--specific-project)
       nil))
    ('global
     (pchist2-get-commands))))

(defun pchist2-select--get-command-at-point ()
  "Get the command record at point."
  (tabulated-list-get-id))

;;; Buffer Rendering

(defun pchist2-select--refresh ()
  "Refresh the command list."
  (let ((commands (pchist2-select--get-filtered-commands))
        (inhibit-read-only t))
    (setq tabulated-list-entries
          (mapcar (lambda (cmd)
                    (list cmd (vector (pchist2-format-command cmd t))))
                  commands))
    (tabulated-list-init-header)

    ;; Add header and footer
    (save-excursion
      (goto-char (point-min))
      (let ((filter-desc (pchist2-format-filter-description
                          pchist2-select--filter
                          pchist2-select--specific-project
                          (projectile-project-root))))
        (insert (propertize "Select Command" 'face 'bold))
        (insert "\n\n")
        (insert (propertize "Filter: " 'face 'bold))
        (insert filter-desc)
        (insert "\n\n")
        (insert (propertize (make-string 60 ?─) 'face 'shadow))
        (insert "\n\n")))

    ;; Add footer at end
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (insert (propertize (make-string 60 ?─) 'face 'shadow))
      (insert "\n\n")
      (insert (propertize "Keys: " 'face 'bold))
      (insert "RET:run  e:edit  c:create  d:dup  k:del  f:filter  g:refresh  q:quit")
      (insert "\n"))))

;;; Interactive Commands

(defun pchist2-select-run ()
  "Run the command at point."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point)))
    (if cmd
        (let ((command-string (pchist2-format-command-for-execution cmd))
              (default-directory (alist-get 'project cmd)))
          (quit-window)
          (projectile-run-compilation command-string))
      (user-error "No command at point"))))

(defun pchist2-select-edit ()
  "Edit the command at point."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point)))
    (if cmd
        (progn
          (require 'pchist2-ui-edit)
          (pchist2-edit-command cmd)
          (pchist2-select-refresh))
      (user-error "No command at point"))))

(defun pchist2-select-duplicate ()
  "Duplicate the command at point."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point)))
    (if cmd
        (progn
          (require 'pchist2-ui-edit)
          (pchist2-edit-command cmd t)
          (pchist2-select-refresh))
      (user-error "No command at point"))))

(defun pchist2-select-delete ()
  "Delete the command at point."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point)))
    (if cmd
        (when (yes-or-no-p (format "Delete command: %s? "
                                   (pchist2-format-command cmd t)))
          (pchist2-delete-command cmd)
          (pchist2-select-refresh)
          (message "Command deleted"))
      (user-error "No command at point"))))

(defun pchist2-select-new ()
  "Create a new command."
  (interactive)
  (require 'pchist2-ui-edit)
  (let ((project-root (or (projectile-project-root)
                          (read-directory-name "Project root: "))))
    (pchist2-edit-command nil nil project-root)
    (pchist2-select-refresh)))

(defalias 'pchist2-select-create #'pchist2-select-new
  "Alias for creating a new command.")

(defun pchist2-select-cycle-filter ()
  "Cycle through filter modes."
  (interactive)
  (setq pchist2-select--filter
        (pcase pchist2-select--filter
          ('current-project 'global)
          ('global 'specific-project)
          ('specific-project 'current-project)))

  ;; If switching to specific-project, prompt for project
  (when (eq pchist2-select--filter 'specific-project)
    (let ((projects (pchist2-get-unique-projects)))
      (setq pchist2-select--specific-project
            (completing-read "Select project: " projects nil t))))

  (pchist2-select-refresh))

(defun pchist2-select-refresh ()
  "Refresh the command list display."
  (interactive)
  (pchist2-load)
  (tabulated-list-revert))

;;; Entry Point

;;;###autoload
(defun pchist2-ui-select-command ()
  "Select and manage commands from pchist2 history.

Key bindings:
  RET - Run the selected command
  e   - Edit selected command
  c   - Create new command
  d   - Duplicate and modify selected command
  k   - Delete selected command
  f   - Cycle filter (current project / global / specific project)
  g   - Refresh list
  q   - Quit"
  (interactive)
  (pchist2-load)

  ;; Create or switch to buffer
  (let ((buffer (get-buffer-create "*pchist2-commands*")))
    (with-current-buffer buffer
      (pchist2-select-mode)
      ;; Reset filter to current-project by default
      (setq pchist2-select--filter 'current-project)
      (pchist2-select-refresh)
      (tabulated-list-print))

    ;; Check if we have any commands
    (if (null (pchist2-select--get-filtered-commands))
        (progn
          (message "No commands in history. Create one now.")
          (require 'pchist2-ui-edit)
          (let ((project-root (or (projectile-project-root)
                                  (read-directory-name "Project root: "))))
            (pchist2-edit-command nil nil project-root)))
      (switch-to-buffer buffer))))

(provide 'pchist2-ui-select)
;;; pchist2-ui-select.el ends here
