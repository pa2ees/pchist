;;; pchist2-ui-edit.el --- Command editor for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides the command editing interface with structured navigation.
;; Users can see all parts of a command at once and edit individual parts.
;; Switches, targets, and installers are displayed one per line for easy editing.

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
    (define-key map (kbd "a") #'pchist2-edit-add-at-point)
    (define-key map (kbd "i") #'pchist2-edit-add-installer)
    (define-key map (kbd "k") #'pchist2-edit-delete-at-point)
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
    (insert (propertize (make-string 80 ?─) 'face 'shadow))
    (insert "\n\n")

    ;; Command field
    (pchist2-edit--insert-field 'command nil "Command"
                                (or pchist2-edit--command "(required)"))
    (insert "\n")

    ;; Switches section
    (pchist2-edit--insert-section-header "Switches")
    (if pchist2-edit--switches
        (cl-loop for switch in pchist2-edit--switches
                 for idx from 0
                 do (pchist2-edit--insert-field 'switch idx "  " switch))
      (pchist2-edit--insert-field 'switches-empty nil "  " "(none)"))
    (insert "\n")

    ;; Targets section
    (pchist2-edit--insert-section-header "Targets")
    (if pchist2-edit--targets
        (cl-loop for target in pchist2-edit--targets
                 for idx from 0
                 do (pchist2-edit--insert-field 'target idx "  " target))
      (pchist2-edit--insert-field 'targets-empty nil "  " "(none)"))
    (insert "\n")

    ;; Installers section
    (pchist2-edit--insert-section-header "Installers")
    (if pchist2-edit--installers
        (cl-loop for installer in pchist2-edit--installers
                 for inst-idx from 0
                 do (pchist2-edit--insert-installer installer inst-idx))
      (pchist2-edit--insert-field 'installers-empty nil "  " "(none)"))
    (insert "\n")

    ;; Separator
    (insert (propertize (make-string 80 ?─) 'face 'shadow))
    (insert "\n\n")

    ;; Instructions
    (insert (propertize "Keys: " 'face 'bold))
    (insert "RET/e:edit  a:add  i:add-installer  k:delete  n/p:navigate  C-c C-c:save  C-c C-k:cancel")
    (insert "\n")

    ;; Restore point or go to first field
    (if (> pos (point-min))
        (goto-char (min pos (point-max)))
      (pchist2-edit--goto-first-field))))

(defun pchist2-edit--insert-section-header (label)
  "Insert a section header with LABEL."
  (insert (propertize label 'face 'bold))
  (insert ":\n"))

(defun pchist2-edit--insert-field (field-type field-index label value)
  "Insert a field line.
FIELD-TYPE is the field type symbol.
FIELD-INDEX is the index (or nil for single fields).
LABEL is the display label.
VALUE is the display value."
  (let ((start (point)))
    (insert label)
    (insert (propertize value 'face (if (string-prefix-p "(" value) 'shadow 'default)))
    (insert "\n")
    (put-text-property start (1- (point)) 'pchist2-field field-type)
    (when field-index
      (put-text-property start (1- (point)) 'pchist2-field-index field-index))))

(defun pchist2-edit--insert-installer (installer inst-idx)
  "Insert an installer record INSTALLER with index INST-IDX."
  (let ((cmd (alist-get 'command installer))
        (switches (alist-get 'switches installer))
        (artifacts (alist-get 'artifacts installer))
        (host (alist-get 'host installer))
        (dest (alist-get 'dest_path installer)))

    ;; Installer header
    (let ((start (point)))
      (insert (propertize (format "  Installer %d\n" (1+ inst-idx)) 'face 'italic))
      (put-text-property start (1- (point)) 'pchist2-field 'installer-header)
      (put-text-property start (1- (point)) 'pchist2-field-index inst-idx))

    ;; Command
    (pchist2-edit--insert-field 'installer-command inst-idx
                                "    Command:    " (or cmd "(required)"))

    ;; Switches
    (pchist2-edit--insert-field 'installer-switches inst-idx
                                "    Switches:   "
                                (if switches (string-join switches " ") "(none)"))

    ;; Artifacts
    (if artifacts
        (cl-loop for artifact in artifacts
                 for art-idx from 0
                 do (pchist2-edit--insert-field 'installer-artifact
                                                (cons inst-idx art-idx)
                                                "    Artifact:   "
                                                (pchist2-edit--format-artifact artifact)))
      (pchist2-edit--insert-field 'installer-artifacts-empty inst-idx
                                  "    Artifacts:  " "(none)"))

    ;; Host
    (pchist2-edit--insert-field 'installer-host inst-idx
                                "    Host:       " (or host "(local)"))

    ;; Dest path
    (pchist2-edit--insert-field 'installer-dest inst-idx
                                "    Dest Path:  " (or dest "(none)"))

    (insert "\n")))

(defun pchist2-edit--format-artifact (artifact)
  "Format ARTIFACT to show basename with full path in parens."
  (let ((basename (file-name-nondirectory artifact)))
    (if (string= basename artifact)
        artifact
      (format "%s (%s)" basename artifact))))

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
    (beginning-of-line)
    (while (and (not (eobp))
                (not (pchist2-edit--is-editable-field-p (point))))
      (forward-line 1)
      (beginning-of-line))
    (when (not (pchist2-edit--is-editable-field-p (point)))
      (goto-char start)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (pchist2-edit--is-editable-field-p (point))))
        (forward-line 1)
        (beginning-of-line)))))

(defun pchist2-edit-previous-field ()
  "Move to the previous editable field."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (beginning-of-line)
    (while (and (not (bobp))
                (not (pchist2-edit--is-editable-field-p (point))))
      (forward-line -1)
      (beginning-of-line))
    (when (not (pchist2-edit--is-editable-field-p (point)))
      (goto-char start)
      (goto-char (point-max))
      (while (and (not (bobp))
                  (not (pchist2-edit--is-editable-field-p (point))))
        (forward-line -1)
        (beginning-of-line)))))

(defun pchist2-edit--is-editable-field-p (pos)
  "Return non-nil if POS is on an editable field."
  (let ((field (get-text-property pos 'pchist2-field)))
    (and field
         (not (memq field '(installer-header))))))

(defun pchist2-edit--get-field-at-point ()
  "Get the field type at point."
  (get-text-property (point) 'pchist2-field))

(defun pchist2-edit--get-field-index-at-point ()
  "Get the field index at point."
  (get-text-property (point) 'pchist2-field-index))

;;; Field Editing

(defun pchist2-edit-modify-at-point ()
  "Modify the field at point."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point)))
    (unless field
      (user-error "Not on an editable field"))
    (pcase field
      ('command (pchist2-edit--modify-command))
      ('switch (pchist2-edit--modify-switch))
      ('switches-empty (pchist2-edit--add-switch))
      ('target (pchist2-edit--modify-target))
      ('targets-empty (pchist2-edit--add-target))
      ('installers-empty (pchist2-edit-add-installer))
      ('installer-command (pchist2-edit--modify-installer-command))
      ('installer-switches (pchist2-edit--modify-installer-switches))
      ('installer-artifact (pchist2-edit--modify-installer-artifact))
      ('installer-artifacts-empty (pchist2-edit--add-installer-artifact))
      ('installer-host (pchist2-edit--modify-installer-host))
      ('installer-dest (pchist2-edit--modify-installer-dest)))))

(defun pchist2-edit-add-at-point ()
  "Add a new item at the current section."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point)))
    (pcase field
      ((or 'switch 'switches-empty) (pchist2-edit--add-switch))
      ((or 'target 'targets-empty) (pchist2-edit--add-target))
      (_ (user-error "Cannot add here. Use 'i' to add an installer")))))

(defun pchist2-edit-delete-at-point ()
  "Delete the item at point."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point))
        (idx (pchist2-edit--get-field-index-at-point)))
    (pcase field
      ('switch (pchist2-edit--delete-switch idx))
      ('target (pchist2-edit--delete-target idx))
      ((or 'installer-header 'installer-command 'installer-switches
           'installer-artifact 'installer-artifacts-empty
           'installer-host 'installer-dest)
       (pchist2-edit--delete-installer (if (consp idx) (car idx) idx)))
      (_ (user-error "Cannot delete this field")))))

;;; Command Field

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

;;; Switch Fields

(defun pchist2-edit--add-switch ()
  "Add a new switch."
  (require 'pchist2-ui-completion)
  (let* ((candidates (pchist2-get-unique-switches
                      pchist2-edit--project
                      pchist2-edit--command))
         (new-val (pchist2-complete "Switch: " candidates nil)))
    (when (and new-val (not (string-empty-p new-val)))
      (setq pchist2-edit--switches
            (append pchist2-edit--switches (list (string-trim new-val))))
      (pchist2-edit--render))))

(defun pchist2-edit--modify-switch ()
  "Modify a switch."
  (require 'pchist2-ui-completion)
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (current (nth idx pchist2-edit--switches))
         (candidates (pchist2-get-unique-switches
                      pchist2-edit--project
                      pchist2-edit--command))
         (new-val (pchist2-complete "Switch: " candidates current)))
    (when (and new-val (not (string-empty-p new-val)))
      (setf (nth idx pchist2-edit--switches) (string-trim new-val))
      (pchist2-edit--render))))

(defun pchist2-edit--delete-switch (idx)
  "Delete switch at IDX."
  (setq pchist2-edit--switches
        (append (cl-subseq pchist2-edit--switches 0 idx)
                (cl-subseq pchist2-edit--switches (1+ idx))))
  (pchist2-edit--render))

;;; Target Fields

(defun pchist2-edit--add-target ()
  "Add a new target."
  (require 'pchist2-ui-completion)
  (let* ((candidates (pchist2-get-unique-targets
                      pchist2-edit--project
                      pchist2-edit--command))
         (new-val (pchist2-complete "Target: " candidates nil)))
    (when (and new-val (not (string-empty-p new-val)))
      (setq pchist2-edit--targets
            (append pchist2-edit--targets (list (string-trim new-val))))
      (pchist2-edit--render))))

(defun pchist2-edit--modify-target ()
  "Modify a target."
  (require 'pchist2-ui-completion)
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (current (nth idx pchist2-edit--targets))
         (candidates (pchist2-get-unique-targets
                      pchist2-edit--project
                      pchist2-edit--command))
         (new-val (pchist2-complete "Target: " candidates current)))
    (when (and new-val (not (string-empty-p new-val)))
      (setf (nth idx pchist2-edit--targets) (string-trim new-val))
      (pchist2-edit--render))))

(defun pchist2-edit--delete-target (idx)
  "Delete target at IDX."
  (setq pchist2-edit--targets
        (append (cl-subseq pchist2-edit--targets 0 idx)
                (cl-subseq pchist2-edit--targets (1+ idx))))
  (pchist2-edit--render))

;;; Installer Management

(defun pchist2-edit-add-installer ()
  "Add a new installer."
  (interactive)
  (require 'pchist2-ui-completion)
  (let* ((cmd-candidates (pchist2-get-installer-commands
                          pchist2-edit--project
                          pchist2-edit--command))
         (inst-cmd (pchist2-complete "Installer command: " cmd-candidates nil)))
    (when (and inst-cmd (not (string-empty-p inst-cmd)))
      (let ((new-installer `((command . ,inst-cmd)
                            (switches . nil)
                            (artifacts . nil)
                            (host . nil)
                            (dest_path . nil))))
        (setq pchist2-edit--installers
              (append pchist2-edit--installers (list new-installer)))
        (pchist2-edit--render)))))

(defun pchist2-edit--delete-installer (idx)
  "Delete installer at IDX."
  (when (yes-or-no-p (format "Delete installer %d? " (1+ idx)))
    (setq pchist2-edit--installers
          (append (cl-subseq pchist2-edit--installers 0 idx)
                  (cl-subseq pchist2-edit--installers (1+ idx))))
    (pchist2-edit--render)))

;;; Installer Field Editing

(defun pchist2-edit--modify-installer-command ()
  "Modify installer command field."
  (require 'pchist2-ui-completion)
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (installer (nth idx pchist2-edit--installers))
         (current (alist-get 'command installer))
         (candidates (pchist2-get-installer-commands
                      pchist2-edit--project
                      pchist2-edit--command))
         (new-val (pchist2-complete "Installer command: " candidates current)))
    (when (and new-val (not (string-empty-p new-val)))
      (setf (alist-get 'command installer) (string-trim new-val))
      (pchist2-edit--render))))

(defun pchist2-edit--modify-installer-switches ()
  "Modify installer switches field."
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (installer (nth idx pchist2-edit--installers))
         (current (alist-get 'switches installer))
         (current-str (if current (string-join current " ") ""))
         (new-val (read-string "Installer switches (space-separated): " current-str)))
    (setf (alist-get 'switches installer)
          (if (string-empty-p new-val)
              nil
            (split-string new-val " " t)))
    (pchist2-edit--render)))

(defun pchist2-edit--add-installer-artifact ()
  "Add an artifact to the current installer."
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (installer (nth idx pchist2-edit--installers))
         (new-val (read-file-name "Artifact path: " pchist2-edit--project)))
    (when (and new-val (not (string-empty-p new-val)))
      (let ((artifacts (alist-get 'artifacts installer)))
        (setf (alist-get 'artifacts installer)
              (append artifacts (list new-val)))
        (pchist2-edit--render)))))

(defun pchist2-edit--modify-installer-artifact ()
  "Modify an installer artifact."
  (let* ((indices (pchist2-edit--get-field-index-at-point))
         (inst-idx (car indices))
         (art-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (artifacts (alist-get 'artifacts installer))
         (current (nth art-idx artifacts))
         (new-val (read-file-name "Artifact path: " pchist2-edit--project nil nil current)))
    (when (and new-val (not (string-empty-p new-val)))
      (setf (nth art-idx artifacts) new-val)
      (pchist2-edit--render))))

(defun pchist2-edit--modify-installer-host ()
  "Modify installer host field."
  (require 'pchist2-ui-completion)
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (installer (nth idx pchist2-edit--installers))
         (current (alist-get 'host installer))
         (candidates (pchist2-get-installer-hosts pchist2-edit--project))
         (new-val (pchist2-complete "Host (empty for local): " candidates current)))
    (setf (alist-get 'host installer)
          (if (or (null new-val) (string-empty-p new-val))
              nil
            (string-trim new-val)))
    (pchist2-edit--render)))

(defun pchist2-edit--modify-installer-dest ()
  "Modify installer destination path field."
  (let* ((idx (pchist2-edit--get-field-index-at-point))
         (installer (nth idx pchist2-edit--installers))
         (current (alist-get 'dest_path installer))
         (new-val (read-string "Destination path: " current)))
    (setf (alist-get 'dest_path installer)
          (if (string-empty-p new-val) nil (string-trim new-val)))
    (pchist2-edit--render)))

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
  "Cancel editing and exit without confirmation."
  (interactive)
  (quit-window t))

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
