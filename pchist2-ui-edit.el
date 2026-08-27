;;; pchist2-ui-edit.el --- Command editor for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides the command editing interface with structured navigation.
;; Users can see all parts of a command at once and edit individual parts.

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'cl-lib)

;;; Edit State

(defvar-local pchist2-edit--original-cmd nil
  "Original command being edited (nil for new commands).")

(defvar-local pchist2-edit--is-duplicate nil
  "Non-nil if this is a duplicate operation.")

(defvar-local pchist2-edit--project nil
  "Project root for the command.")

(defvar-local pchist2-edit--command nil
  "Base command string.")

(defvar-local pchist2-edit--switches nil
  "List of switches.")

(defvar-local pchist2-edit--targets nil
  "List of targets.")

(defvar-local pchist2-edit--installers nil
  "List of installer records.")

;;; Mode Definition

(defvar pchist2-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pchist2-edit-modify-at-point)
    (define-key map (kbd "e") #'pchist2-edit-modify-at-point)
    (define-key map (kbd "n") #'pchist2-edit-next-field)
    (define-key map (kbd "p") #'pchist2-edit-previous-field)
    (define-key map (kbd "TAB") #'pchist2-edit-next-field)
    (define-key map (kbd "<backtab>") #'pchist2-edit-previous-field)
    (define-key map (kbd "C-c C-c") #'pchist2-edit-save)
    (define-key map (kbd "C-c C-k") #'pchist2-edit-cancel)
    (define-key map (kbd "q") #'pchist2-edit-cancel)
    map)
  "Keymap for `pchist2-edit-mode'.")

(define-derived-mode pchist2-edit-mode special-mode "pchist2-edit"
  "Major mode for editing pchist2 commands.

\\{pchist2-edit-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;; Buffer Rendering

(defun pchist2-edit--render ()
  "Render the edit buffer."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)

    ;; Header
    (insert (propertize "Edit Command" 'face 'bold))
    (insert "\n\n")

    ;; Command preview
    (insert (propertize "Preview: " 'face 'shadow))
    (insert (pchist2-format-builder-state
             pchist2-edit--command
             pchist2-edit--switches
             pchist2-edit--targets
             pchist2-edit--installers))
    (insert "\n\n")

    ;; Separator
    (insert (propertize (make-string 60 ?─) 'face 'shadow))
    (insert "\n\n")

    ;; Fields
    (pchist2-edit--insert-field 'command "Command"
                                (or pchist2-edit--command ""))
    (pchist2-edit--insert-field 'switches "Switches"
                                (if pchist2-edit--switches
                                    (string-join pchist2-edit--switches " ")
                                  ""))
    (pchist2-edit--insert-field 'targets "Targets"
                                (if pchist2-edit--targets
                                    (string-join pchist2-edit--targets " ")
                                  ""))
    (pchist2-edit--insert-field 'installers "Installers"
                                (if pchist2-edit--installers
                                    (format "(%d configured)"
                                            (length pchist2-edit--installers))
                                  "(none)"))

    ;; Separator
    (insert "\n")
    (insert (propertize (make-string 60 ?─) 'face 'shadow))
    (insert "\n\n")

    ;; Instructions
    (insert (propertize "Keys: " 'face 'bold))
    (insert "RET/e:edit  n/p:navigate  C-c C-c:save  C-c C-k:cancel")
    (insert "\n")

    ;; Restore point or go to first field
    (if (> pos (point-min))
        (goto-char pos)
      (pchist2-edit--goto-first-field))))

(defun pchist2-edit--insert-field (field-name label value)
  "Insert a field line with FIELD-NAME, LABEL, and VALUE."
  (let ((start (point)))
    (insert (propertize (format "  %-12s " (concat label ":")) 'face 'bold))
    (insert (propertize value 'face (if (string-empty-p value) 'shadow 'default)))
    (insert "\n")
    (put-text-property start (point) 'pchist2-field field-name)))

;;; Navigation

(defun pchist2-edit--goto-first-field ()
  "Move point to the first editable field."
  (goto-char (point-min))
  (pchist2-edit-next-field))

(defun pchist2-edit-next-field ()
  "Move to the next editable field."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'pchist2-field)))
      (forward-line 1))
    (when (not (get-text-property (point) 'pchist2-field))
      (goto-char start)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (get-text-property (point) 'pchist2-field)))
        (forward-line 1))))
  (beginning-of-line))

(defun pchist2-edit-previous-field ()
  "Move to the previous editable field."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'pchist2-field)))
      (forward-line -1))
    (when (not (get-text-property (point) 'pchist2-field))
      (goto-char start)
      (goto-char (point-max))
      (while (and (not (bobp))
                  (not (get-text-property (point) 'pchist2-field)))
        (forward-line -1))))
  (beginning-of-line))

(defun pchist2-edit--get-field-at-point ()
  "Get the field name at point."
  (get-text-property (point) 'pchist2-field))

;;; Field Editing

(defun pchist2-edit-modify-at-point ()
  "Modify the field at point."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point)))
    (unless field
      (user-error "Not on an editable field"))
    (pcase field
      ('command (pchist2-edit--modify-command))
      ('switches (pchist2-edit--modify-switches))
      ('targets (pchist2-edit--modify-targets))
      ('installers (pchist2-edit--modify-installers)))))

(defun pchist2-edit--modify-command ()
  "Modify the command field."
  (require 'pchist2-ui-completion)
  (let* ((candidates (pchist2-get-unique-commands pchist2-edit--project))
         (new-val (pchist2-complete "Command: "
                                    candidates
                                    pchist2-edit--command)))
    (when (and new-val (not (string-empty-p new-val)))
      (setq pchist2-edit--command (string-trim new-val))
      (pchist2-edit--render))))

(defun pchist2-edit--modify-switches ()
  "Modify the switches field."
  (require 'pchist2-ui-completion)
  (let* ((candidates (pchist2-get-unique-switches
                      pchist2-edit--project
                      pchist2-edit--command))
         (current (string-join pchist2-edit--switches " "))
         (new-val (pchist2-complete "Switches (space-separated): "
                                    candidates
                                    current)))
    (setq pchist2-edit--switches
          (if (and new-val (not (string-empty-p new-val)))
              (split-string new-val " " t)
            nil))
    (pchist2-edit--render)))

(defun pchist2-edit--modify-targets ()
  "Modify the targets field."
  (require 'pchist2-ui-completion)
  (let* ((candidates (pchist2-get-unique-targets
                      pchist2-edit--project
                      pchist2-edit--command))
         (current (string-join pchist2-edit--targets " "))
         (new-val (pchist2-complete "Targets (space-separated): "
                                    candidates
                                    current)))
    (setq pchist2-edit--targets
          (if (and new-val (not (string-empty-p new-val)))
              (split-string new-val " " t)
            nil))
    (pchist2-edit--render)))

(defun pchist2-edit--modify-installers ()
  "Modify the installers list using a submenu."
  (let ((choices '("Add new installer"
                   "Edit existing installer"
                   "Remove installer"
                   "Clear all installers")))
    (pcase (completing-read "Installer action: " choices nil t)
      ("Add new installer"
       (pchist2-edit--add-installer))
      ("Edit existing installer"
       (pchist2-edit--edit-installer))
      ("Remove installer"
       (pchist2-edit--remove-installer))
      ("Clear all installers"
       (when (yes-or-no-p "Clear all installers? ")
         (setq pchist2-edit--installers nil)
         (pchist2-edit--render))))))

(defun pchist2-edit--add-installer ()
  "Add a new installer to the list."
  (require 'pchist2-ui-completion)
  (let* ((cmd-candidates (pchist2-get-installer-commands
                          pchist2-edit--project
                          pchist2-edit--command))
         (inst-cmd (pchist2-complete "Installer command: " cmd-candidates nil)))
    (when (and inst-cmd (not (string-empty-p inst-cmd)))
      (let* ((switches-str (read-string "Installer switches (space-separated): "))
             (switches (if (string-empty-p switches-str)
                           nil
                         (split-string switches-str " " t)))
             (artifacts-str (read-string "Artifacts (space-separated paths): "))
             (artifacts (if (string-empty-p artifacts-str)
                            nil
                          (split-string artifacts-str " " t)))
             (host-candidates (pchist2-get-installer-hosts pchist2-edit--project))
             (host-choice (pchist2-complete "Host (empty for local): "
                                           host-candidates
                                           nil))
             (host (if (or (null host-choice) (string-empty-p host-choice))
                       nil
                     host-choice))
             (dest-path (read-string "Destination path: ")))
        (push `((command . ,inst-cmd)
                (switches . ,switches)
                (artifacts . ,artifacts)
                (host . ,host)
                (dest_path . ,(if (string-empty-p dest-path) nil dest-path)))
              pchist2-edit--installers)
        (pchist2-edit--render)))))

(defun pchist2-edit--edit-installer ()
  "Edit an existing installer."
  (if (null pchist2-edit--installers)
      (user-error "No installers to edit")
    (let* ((choices (cl-loop for inst in pchist2-edit--installers
                            for i from 0
                            collect (cons (format "%d: %s" i (alist-get 'command inst))
                                         i)))
           (choice (completing-read "Select installer: "
                                   (mapcar #'car choices) nil t))
           (idx (cdr (assoc choice choices))))
      (when idx
        (let ((inst (nth idx pchist2-edit--installers)))
          (setq pchist2-edit--installers
                (cl-remove-if (lambda (x) (eq x inst)) pchist2-edit--installers))
          (pchist2-edit--add-installer))))))

(defun pchist2-edit--remove-installer ()
  "Remove an installer from the list."
  (if (null pchist2-edit--installers)
      (user-error "No installers to remove")
    (let* ((choices (cl-loop for inst in pchist2-edit--installers
                            for i from 0
                            collect (cons (format "%d: %s" i (alist-get 'command inst))
                                         inst)))
           (choice (completing-read "Remove installer: "
                                   (mapcar #'car choices) nil t))
           (inst (cdr (assoc choice choices))))
      (when inst
        (setq pchist2-edit--installers
              (cl-remove-if (lambda (x) (eq x inst)) pchist2-edit--installers))
        (pchist2-edit--render)))))

;;; Save/Cancel

(defun pchist2-edit-save ()
  "Save the command and exit."
  (interactive)
  (unless pchist2-edit--command
    (user-error "Command is required"))

  (if pchist2-edit--is-duplicate
      ;; Duplicate: always add as new
      (progn
        (pchist2-add-command pchist2-edit--project
                            pchist2-edit--command
                            pchist2-edit--switches
                            pchist2-edit--targets
                            pchist2-edit--installers)
        (message "Command duplicated and saved"))
    ;; Edit or new
    (if pchist2-edit--original-cmd
        (progn
          (pchist2-update-command pchist2-edit--original-cmd
                                 `((command . ,pchist2-edit--command)
                                   (switches . ,pchist2-edit--switches)
                                   (targets . ,pchist2-edit--targets)
                                   (installers . ,pchist2-edit--installers)))
          (message "Command updated"))
      (progn
        (pchist2-add-command pchist2-edit--project
                            pchist2-edit--command
                            pchist2-edit--switches
                            pchist2-edit--targets
                            pchist2-edit--installers)
        (message "Command saved"))))

  (quit-window t))

(defun pchist2-edit-cancel ()
  "Cancel editing and exit."
  (interactive)
  (when (yes-or-no-p "Discard changes? ")
    (quit-window t)))

;;; Entry Point

(defun pchist2-edit-command (cmd &optional duplicate project-root)
  "Edit command CMD in a structured editor.
If DUPLICATE is non-nil, treat as a duplicate operation.
If CMD is nil and PROJECT-ROOT is provided, create a new command."
  (let ((buffer (get-buffer-create "*pchist2-edit*")))
    (with-current-buffer buffer
      (pchist2-edit-mode)

      ;; Initialize state
      (setq pchist2-edit--original-cmd (if duplicate nil cmd))
      (setq pchist2-edit--is-duplicate duplicate)

      (if cmd
          (progn
            (setq pchist2-edit--project (alist-get 'project cmd))
            (setq pchist2-edit--command (alist-get 'command cmd))
            (setq pchist2-edit--switches (alist-get 'switches cmd))
            (setq pchist2-edit--targets (alist-get 'targets cmd))
            (setq pchist2-edit--installers (alist-get 'installers cmd)))
        ;; New command
        (setq pchist2-edit--project project-root)
        (setq pchist2-edit--command nil)
        (setq pchist2-edit--switches nil)
        (setq pchist2-edit--targets nil)
        (setq pchist2-edit--installers nil))

      (pchist2-edit--render))

    (switch-to-buffer buffer)))

(provide 'pchist2-ui-edit)
;;; pchist2-ui-edit.el ends here
