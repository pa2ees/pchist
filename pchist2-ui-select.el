;;; pchist2-ui-select.el --- Command selection UI for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides the command selection interface.
;; It displays commands with custom header/footer.

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'projectile)
(require 'cl-lib)

;; Forward declarations
(declare-function pchist2-edit-command "pchist2-ui-edit")

;;; Filter State

(defvar-local pchist2-select--filter 'current-project
  "Current filter mode: `current-project', `specific-project', or `global'.")

(defvar-local pchist2-select--specific-project nil
  "Project path when filter is `specific-project'.")

(defvar-local pchist2-select--show-full-paths nil
  "Non-nil to show full paths instead of basenames.")

(defvar-local pchist2-select--help-visible nil
  "Non-nil if help section is visible.")

(defvar-local pchist2-select--commands nil
  "List of commands currently displayed.")

;;; Mode Definition

(defvar pchist2-select-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Actions
    (define-key map (kbd "RET") #'pchist2-select-run)
    (define-key map (kbd "e") #'pchist2-select-edit)
    (define-key map (kbd "c") #'pchist2-select-create)
    (define-key map (kbd "d") #'pchist2-select-duplicate)
    (define-key map (kbd "D") #'pchist2-select-duplicate-to-project)
    (define-key map (kbd "k") #'pchist2-select-delete)
    (define-key map (kbd "K") #'pchist2-select-clear-all)
    ;; Filter/View
    (define-key map (kbd "f") #'pchist2-select-cycle-filter)
    (define-key map (kbd "F") #'pchist2-select-toggle-full-paths)
    (define-key map (kbd "?") #'pchist2-select-toggle-help)
    ;; Navigation
    (define-key map (kbd "n") #'pchist2-select-next-command)
    (define-key map (kbd "p") #'pchist2-select-previous-command)
    (define-key map (kbd "<down>") #'pchist2-select-next-command)
    (define-key map (kbd "<up>") #'pchist2-select-previous-command)
    ;; Quick jump by number
    (dotimes (i 10)
      (define-key map (kbd (number-to-string i)) #'pchist2-select-jump-to-number))
    ;; Refresh
    (define-key map (kbd "g") #'pchist2-select-refresh)
    ;; Quit
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `pchist2-select-mode'.")

(define-derived-mode pchist2-select-mode special-mode "pchist2-select"
  "Major mode for selecting and managing pchist2 commands.

\\{pchist2-select-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

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
  (get-text-property (point) 'pchist2-command))

(defun pchist2-select--has-multiple-projects ()
  "Return non-nil if commands span multiple projects."
  (let ((commands pchist2-select--commands))
    (> (length (cl-remove-duplicates
                (mapcar (lambda (cmd) (alist-get 'project cmd))
                        commands)
                :test #'string=))
       1)))

;;; Formatting

(defun pchist2-select--format-command (cmd)
  "Format CMD for display in select list."
  (let* ((show-project (pchist2-select--has-multiple-projects))
         (cmd-str (pchist2-select--format-command-full cmd))
         (parts nil))

    ;; Add project prefix if multiple projects
    (when show-project
      (let* ((project (alist-get 'project cmd))
             (proj-display (if pchist2-select--show-full-paths
                               project
                             (file-name-nondirectory (directory-file-name project)))))
        (push (propertize (format "[%s]" proj-display) 'face 'shadow) parts)))

    ;; Add the full command
    (push cmd-str parts)

    (string-join parts " ")))

(defun pchist2-select--format-command-full (cmd)
  "Format CMD as a full command string respecting path display setting."
  (let* ((command (alist-get 'command cmd))
         (switches (alist-get 'switches cmd))
         (targets (alist-get 'targets cmd))
         (installers (alist-get 'installers cmd))
         (parts (list command)))

    ;; Add switches
    (when switches
      (setq parts (append parts switches)))

    ;; Add targets
    (when targets
      (setq parts (append parts targets)))

    ;; Add installer info
    (when installers
      (dolist (inst installers)
        (let* ((inst-cmd (alist-get 'command inst))
               (artifacts (alist-get 'artifacts inst))
               (host (alist-get 'host inst))
               (dest (alist-get 'dest_path inst))
               (inst-parts (list inst-cmd)))
          ;; Add artifacts
          (when artifacts
            (let ((art-display (if pchist2-select--show-full-paths
                                   artifacts
                                 (mapcar #'file-name-nondirectory artifacts))))
              (setq inst-parts (append inst-parts art-display))))
          ;; Add destination
          (when (or host dest)
            (setq inst-parts (append inst-parts
                                     (list (format "%s%s"
                                                   (if host (concat host ":") "")
                                                   (or dest ""))))))
          ;; Build full installer string
          (setq parts (append parts
                              (list (concat "&& " (string-join inst-parts " "))))))))

    (string-join parts " ")))

;;; Buffer Rendering

(defun pchist2-select--refresh ()
  "Refresh the command list."
  (let ((commands (pchist2-select--get-filtered-commands))
        (current-cmd (pchist2-select--get-command-at-point))
        (inhibit-read-only t))

    (setq pchist2-select--commands commands)
    (erase-buffer)

    ;; Header
    (pchist2-select--insert-header)

    ;; Command list
    (if commands
        (let ((num 1))
          (dolist (cmd commands)
            (let ((start (point)))
              (insert (propertize (format "%2d. " num) 'face 'bold))
              (insert (pchist2-select--format-command cmd))
              (insert "\n")
              (put-text-property start (point) 'pchist2-command cmd)
              (setq num (1+ num)))))
      (insert "  (no commands)\n"))

    (insert "\n")

    ;; Footer
    (pchist2-select--insert-footer)

    ;; Restore or set position
    (if current-cmd
        (pchist2-select--goto-command current-cmd)
      (pchist2-select--goto-first-command))))

(defun pchist2-select--insert-header ()
  "Insert the header section."
  ;; Title
  (insert (propertize "Select Command" 'face 'bold))
  (insert "\n\n")

  ;; Filter info
  (insert (propertize "Filter:  " 'face 'bold))
  (let ((filter-desc (pchist2-format-filter-description
                      pchist2-select--filter
                      pchist2-select--specific-project
                      (projectile-project-root))))
    (insert filter-desc))
  (insert "\n")

  (insert (propertize "Display: " 'face 'bold))
  (insert (if pchist2-select--show-full-paths
             "Full paths"
           "Basenames only"))
  (insert "\n\n"))

(defun pchist2-select--insert-footer ()
  "Insert the footer section."
  (if pchist2-select--help-visible
      (pchist2-select--insert-help)
    (insert (propertize "[?] Show help" 'face 'shadow)))
  (insert "\n"))

(defun pchist2-select--insert-help ()
  "Insert the help section."
  (insert (propertize "[?] Hide help\n\n" 'face 'shadow))
  (insert (propertize "Actions:\n" 'face 'bold))
  (insert "  RET         Run command\n")
  (insert "  e           Edit command\n")
  (insert "  c           Create new command\n")
  (insert "  d           Duplicate command\n")
  (insert "  D           Duplicate to current project\n")
  (insert "  k           Delete command\n")
  (insert "  K           Clear all commands (with confirmation)\n")
  (insert "\n")
  (insert (propertize "View:\n" 'face 'bold))
  (insert "  f           Cycle filter (current/global/specific project)\n")
  (insert "  F           Toggle full paths display\n")
  (insert "  g           Refresh list\n")
  (insert "\n")
  (insert (propertize "Navigation:\n" 'face 'bold))
  (insert "  n/p, ↓/↑    Next/previous command\n")
  (insert "  1-9, 0      Jump to command #1-10\n")
  (insert "\n")
  (insert (propertize "Other:\n" 'face 'bold))
  (insert "  ?           Toggle help\n")
  (insert "  q           Quit\n"))

;;; Navigation

(defun pchist2-select--goto-first-command ()
  "Move point to the first command in the list."
  (goto-char (point-min))
  (while (and (not (pchist2-select--get-command-at-point))
              (not (eobp)))
    (forward-line 1)))

(defun pchist2-select--goto-command (cmd)
  "Move point to CMD in the list."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (when (equal (pchist2-select--get-command-at-point) cmd)
        (setq found t))
      (unless found (forward-line 1)))
    (unless found
      (pchist2-select--goto-first-command))))

(defun pchist2-select-next-command ()
  "Move to the next command."
  (interactive)
  (forward-line 1)
  (unless (pchist2-select--get-command-at-point)
    (pchist2-select--goto-first-command)))

(defun pchist2-select-previous-command ()
  "Move to the previous command."
  (interactive)
  (forward-line -1)
  (unless (pchist2-select--get-command-at-point)
    (goto-char (point-max))
    (while (and (not (pchist2-select--get-command-at-point))
                (not (bobp)))
      (forward-line -1))))

(defun pchist2-select-jump-to-number ()
  "Jump to command by number (press digit key)."
  (interactive)
  (let* ((key (this-command-keys))
         (digit-char (aref key 0))
         (digit (- digit-char ?0))
         (target-num (if (zerop digit) 10 digit)))
    (goto-char (point-min))
    (let ((found nil)
          (current-num 1))
      (while (and (not found) (not (eobp)))
        (when (pchist2-select--get-command-at-point)
          (if (= current-num target-num)
              (setq found t)
            (setq current-num (1+ current-num))))
        (unless found (forward-line 1)))
      (unless found
        (message "No command #%d" target-num)
        (pchist2-select--goto-first-command)))))

;;; Toggle Commands

(defun pchist2-select-toggle-help ()
  "Toggle help visibility."
  (interactive)
  (setq pchist2-select--help-visible (not pchist2-select--help-visible))
  (pchist2-select--refresh))

(defun pchist2-select-toggle-full-paths ()
  "Toggle full paths display."
  (interactive)
  (setq pchist2-select--show-full-paths (not pchist2-select--show-full-paths))
  (pchist2-select--refresh))

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
          (pchist2-edit-command cmd))
      (user-error "No command at point"))))

(defun pchist2-select-create ()
  "Create a new command."
  (interactive)
  (require 'pchist2-ui-edit)
  (let ((project-root (or (projectile-project-root)
                          (read-directory-name "Project root: "))))
    (pchist2-edit-command nil nil project-root)))

(defun pchist2-select-duplicate ()
  "Duplicate the command at point."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point)))
    (if cmd
        (progn
          (require 'pchist2-ui-edit)
          (pchist2-edit-command cmd t))
      (user-error "No command at point"))))

(defun pchist2-select-duplicate-to-project ()
  "Duplicate the command at point to the current project."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point))
        (current-project (projectile-project-root)))
    (unless cmd
      (user-error "No command at point"))
    (unless current-project
      (user-error "Not in a projectile project"))

    (let ((cmd-project (alist-get 'project cmd)))
      (if (string= cmd-project current-project)
          (message "Command is already in current project")
        (pchist2-add-command current-project
                            (alist-get 'command cmd)
                            (alist-get 'switches cmd)
                            (alist-get 'targets cmd)
                            (alist-get 'installers cmd))
        (message "Command duplicated to %s"
                 (file-name-nondirectory (directory-file-name current-project)))
        (pchist2-select-refresh)))))

(defun pchist2-select-delete ()
  "Delete the command at point."
  (interactive)
  (let ((cmd (pchist2-select--get-command-at-point)))
    (if cmd
        (when (yes-or-no-p (format "Delete command: %s? "
                                   (pchist2-select--format-command cmd)))
          (pchist2-delete-command cmd)
          (pchist2-select-refresh)
          (message "Command deleted"))
      (user-error "No command at point"))))

(defun pchist2-select-clear-all ()
  "Clear all commands in history."
  (interactive)
  (when (yes-or-no-p "Clear ALL commands from history? This cannot be undone! ")
    (pchist2-clear-all)
    (pchist2-select-refresh)
    (message "All commands cleared")))

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
  (pchist2-select--refresh))

;;; Entry Point

;;;###autoload
(defun pchist2-ui-select-command ()
  "Select and manage commands from pchist2 history.

Key bindings:
  RET - Run the selected command
  e   - Edit selected command
  c   - Create new command
  d   - Duplicate command
  D   - Duplicate to current project
  k   - Delete command
  K   - Clear all commands
  f   - Cycle filter
  F   - Toggle full paths
  ?   - Toggle help
  n/p - Next/previous command
  g   - Refresh list
  q   - Quit"
  (interactive)
  (pchist2-load)

  ;; Create or switch to buffer
  (let ((buffer (get-buffer-create "*pchist2-commands*"))
        (has-commands nil))
    (with-current-buffer buffer
      (pchist2-select-mode)
      ;; Reset to defaults
      (setq pchist2-select--filter 'current-project)
      (setq pchist2-select--show-full-paths nil)
      (setq pchist2-select--help-visible nil)
      (pchist2-select--refresh)
      ;; Capture whether we have commands (in buffer-local context)
      (setq has-commands (not (null pchist2-select--commands))))

    ;; Check if we have any commands
    (if has-commands
        (switch-to-buffer buffer)
      (progn
        (message "No commands in history. Create one now.")
        (require 'pchist2-ui-edit)
        (let ((project-root (or (projectile-project-root)
                                (read-directory-name "Project root: "))))
          (pchist2-edit-command nil nil project-root))))))

(provide 'pchist2-ui-select)
;;; pchist2-ui-select.el ends here
