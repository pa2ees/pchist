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

;; Forward declarations
(declare-function pchist2-complete "pchist2-ui-completion")

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

(defvar-local pchist2-edit--help-visible nil
  "Non-nil if help section is visible.")

(defvar-local pchist2-edit--initial-state nil
  "Initial state snapshot for change detection.")

;;; Mode Definition

(defvar pchist2-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pchist2-edit-modify-at-point)
    (define-key map (kbd "e") #'pchist2-edit-modify-at-point)
    (define-key map (kbd "a") #'pchist2-edit-add-at-point)
    (define-key map (kbd "k") #'pchist2-edit-delete-at-point)
    (define-key map (kbd "n") #'pchist2-edit-next-field)
    (define-key map (kbd "p") #'pchist2-edit-previous-field)
    (define-key map (kbd "TAB") #'pchist2-edit-next-field)
    (define-key map (kbd "<backtab>") #'pchist2-edit-previous-field)
    (define-key map (kbd "M-p") #'pchist2-edit-move-up)
    (define-key map (kbd "M-n") #'pchist2-edit-move-down)
    (define-key map (kbd "M-<up>") #'pchist2-edit-move-up)
    (define-key map (kbd "M-<down>") #'pchist2-edit-move-down)
    (define-key map (kbd "?") #'pchist2-edit-toggle-help)
    (define-key map (kbd "g") #'pchist2-edit--render)
    (define-key map (kbd "C-c C-c") #'pchist2-edit-save)
    (define-key map (kbd "C-c C-k") #'pchist2-edit-cancel)
    (define-key map (kbd "q") #'pchist2-edit-cancel)
    map)
  "Keymap for `pchist2-edit-mode'.")

(define-derived-mode pchist2-edit-mode special-mode "pchist2-edit"
  "Major mode for editing pchist2 commands.

\\{pchist2-edit-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

;;; Buffer Rendering

(defun pchist2-edit--render ()
  "Render the edit buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)

    ;; Title
    (insert (propertize "Edit Command" 'face 'bold))
    (insert "\n\n")

    ;; Command preview
    (insert (propertize "Preview: " 'face 'italic))
    (insert (pchist2-format-builder-state
             pchist2-edit--command
             pchist2-edit--switches
             pchist2-edit--targets
             pchist2-edit--installers))
    (insert "\n\n")

    ;; Command field
    (pchist2-edit--insert-field 'command nil "Command:   "
                                (or pchist2-edit--command "(required)"))

    ;; Switches section
    (if pchist2-edit--switches
        (cl-loop for switch in pchist2-edit--switches
                 for idx from 0
                 do (pchist2-edit--insert-field 'switch idx
                                                (if (zerop idx) "Switches:  " "           ")
                                                switch))
      (pchist2-edit--insert-field 'switches-empty nil "Switches:  " "(none)"))

    ;; Targets section
    (if pchist2-edit--targets
        (cl-loop for target in pchist2-edit--targets
                 for idx from 0
                 do (pchist2-edit--insert-field 'target idx
                                                (if (zerop idx) "Targets:   " "           ")
                                                target))
      (pchist2-edit--insert-field 'targets-empty nil "Targets:   " "(none)"))

    ;; Installers section
    (insert (propertize "Installers:" 'face 'default))
    (insert "\n")
    (if pchist2-edit--installers
        (cl-loop for installer in pchist2-edit--installers
                 for inst-idx from 0
                 do (pchist2-edit--insert-installer installer inst-idx))
      (pchist2-edit--insert-field 'installers-empty nil "  " "(none)"))

    ;; Help section
    (insert "\n")
    (if pchist2-edit--help-visible
        (pchist2-edit--insert-help)
      (insert (propertize "[?] Show help" 'face 'shadow)))
    (insert "\n")

    ;; Restore point or go to first field
    (if (> pos (point-min))
        (goto-char (min pos (point-max)))
      (pchist2-edit--goto-first-field))))

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

    ;; Installer header (navigable and deletable)
    (let ((start (point)))
      (insert (format "  Installer %d\n" (1+ inst-idx)))
      (put-text-property start (1- (point)) 'pchist2-field 'installer-header)
      (put-text-property start (1- (point)) 'pchist2-field-index inst-idx))

    ;; Command
    (pchist2-edit--insert-field 'installer-command inst-idx
                                "    Command:    " (or cmd "(required)"))

    ;; Switches
    (if switches
        (cl-loop for switch in switches
                 for sw-idx from 0
                 do (pchist2-edit--insert-field 'installer-switch
                                                (cons inst-idx sw-idx)
                                                (if (zerop sw-idx) "    Switches:   " "                ")
                                                switch))
      (pchist2-edit--insert-field 'installer-switches-empty inst-idx
                                  "    Switches:   " "(none)"))

    ;; Artifacts
    (if artifacts
        (cl-loop for artifact in artifacts
                 for art-idx from 0
                 do (pchist2-edit--insert-field 'installer-artifact
                                                (cons inst-idx art-idx)
                                                (if (zerop art-idx) "    Artifacts:  " "                ")
                                                (pchist2-edit--format-artifact artifact)))
      (pchist2-edit--insert-field 'installer-artifacts-empty inst-idx
                                  "    Artifacts:  " "(none)"))

    ;; Host
    (pchist2-edit--insert-field 'installer-host inst-idx
                                "    Host:       " (or host "(local)"))

    ;; Dest path
    (pchist2-edit--insert-field 'installer-dest inst-idx
                                "    Dest Path:  " (or dest "(none)"))))

(defun pchist2-edit--format-artifact (artifact)
  "Format ARTIFACT to show basename with full path in parens."
  (let ((basename (file-name-nondirectory artifact)))
    (if (string= basename artifact)
        artifact
      (format "%s (%s)" basename artifact))))

(defun pchist2-edit--insert-help ()
  "Insert the help section."
  (insert (propertize "[?] Hide help\n\n" 'face 'shadow))
  (insert (propertize "Navigation:\n" 'face 'bold))
  (insert "  n/p, TAB/S-TAB  Move between fields\n")
  (insert "\n")
  (insert (propertize "Editing:\n" 'face 'bold))
  (insert "  RET, e          Edit field at point\n")
  (insert "  a               Add item (switch/target/installer part)\n")
  (insert "  k               Delete item at point\n")
  (insert "  M-p/n, M-↑/↓    Move item up/down\n")
  (insert "\n")
  (insert (propertize "Save/Cancel:\n" 'face 'bold))
  (insert "  C-c C-c         Save and exit\n")
  (insert "  C-c C-k, q      Cancel and exit\n"))

(defun pchist2-edit-toggle-help ()
  "Toggle help visibility."
  (interactive)
  (setq pchist2-edit--help-visible (not pchist2-edit--help-visible))
  (pchist2-edit--render))

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
                (not (get-text-property (point) 'pchist2-field)))
      (forward-line 1)
      (beginning-of-line))
    (when (not (get-text-property (point) 'pchist2-field))
      (goto-char start)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (get-text-property (point) 'pchist2-field)))
        (forward-line 1)
        (beginning-of-line)))))

(defun pchist2-edit-previous-field ()
  "Move to the previous editable field."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (beginning-of-line)
    (while (and (not (bobp))
                (not (get-text-property (point) 'pchist2-field)))
      (forward-line -1)
      (beginning-of-line))
    (when (not (get-text-property (point) 'pchist2-field))
      (goto-char start)
      (goto-char (point-max))
      (while (and (not (bobp))
                  (not (get-text-property (point) 'pchist2-field)))
        (forward-line -1)
        (beginning-of-line)))))

(defun pchist2-edit--get-field-at-point ()
  "Get the field type at point."
  (get-text-property (point) 'pchist2-field))

(defun pchist2-edit--get-field-index-at-point ()
  "Get the field index at point."
  (get-text-property (point) 'pchist2-field-index))

(defun pchist2-edit--goto-switch (idx)
  "Move point to switch at IDX."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (and (eq (pchist2-edit--get-field-at-point) 'switch)
                        (equal (pchist2-edit--get-field-index-at-point) idx))))
    (forward-line 1))
  (beginning-of-line))

(defun pchist2-edit--goto-target (idx)
  "Move point to target at IDX."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (and (eq (pchist2-edit--get-field-at-point) 'target)
                        (equal (pchist2-edit--get-field-index-at-point) idx))))
    (forward-line 1))
  (beginning-of-line))

(defun pchist2-edit--goto-installer (idx)
  "Move point to installer header at IDX."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (and (eq (pchist2-edit--get-field-at-point) 'installer-header)
                        (equal (pchist2-edit--get-field-index-at-point) idx))))
    (forward-line 1))
  (beginning-of-line))

(defun pchist2-edit--goto-installer-switch (inst-idx sw-idx)
  "Move point to installer switch at INST-IDX, SW-IDX."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (and (eq (pchist2-edit--get-field-at-point) 'installer-switch)
                        (equal (pchist2-edit--get-field-index-at-point) (cons inst-idx sw-idx)))))
    (forward-line 1))
  (beginning-of-line))

(defun pchist2-edit--goto-installer-artifact (inst-idx art-idx)
  "Move point to installer artifact at INST-IDX, ART-IDX."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (and (eq (pchist2-edit--get-field-at-point) 'installer-artifact)
                        (equal (pchist2-edit--get-field-index-at-point) (cons inst-idx art-idx)))))
    (forward-line 1))
  (beginning-of-line))

;;; Context-Aware Actions

(defun pchist2-edit-add-at-point ()
  "Context-aware add: add switch, target, installer part, or installer."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point)))
    (pcase field
      ((or 'switch 'switches-empty)
       (pchist2-edit--add-switch))
      ((or 'target 'targets-empty)
       (pchist2-edit--add-target))
      ('installer-header
       (message "Use 'a' on a specific installer field to add to it, or navigate elsewhere"))
      ((or 'installer-switch 'installer-switches-empty)
       (pchist2-edit--add-installer-switch))
      ((or 'installer-artifact 'installer-artifacts-empty)
       (pchist2-edit--add-installer-artifact))
      ((or 'installers-empty)
       (pchist2-edit-add-installer))
      ('installer-host
       (message "Host can only be edited, not added (already exists)"))
      ((or 'command 'installer-command 'installer-dest)
       (message "Cannot add here - this field can only be edited"))
      (_
       (message "Cannot add at this location")))))

(defun pchist2-edit-delete-at-point ()
  "Context-aware delete: delete switch, target, installer, or installer part."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point))
        (idx (pchist2-edit--get-field-index-at-point)))
    (pcase field
      ('switch (pchist2-edit--delete-switch idx))
      ('target (pchist2-edit--delete-target idx))
      ('installer-header (pchist2-edit--delete-installer idx))
      ('installer-switch (pchist2-edit--delete-installer-switch idx))
      ('installer-artifact (pchist2-edit--delete-installer-artifact idx))
      ((or 'command 'installer-command)
       (message "Cannot delete command field - edit it instead"))
      ('installer-host
       (let ((installer (nth idx pchist2-edit--installers)))
         (if (alist-get 'host installer)
             (progn
               (setf (alist-get 'host installer) nil)
               (pchist2-edit--render)
               (message "Host cleared"))
           (message "Host is already empty"))))
      ((or 'installer-dest 'switches-empty 'targets-empty
           'installer-switches-empty 'installer-artifacts-empty 'installers-empty)
       (message "Nothing to delete here"))
      (_
       (message "Cannot delete at this location")))))

(defun pchist2-edit-move-up ()
  "Move the item at point up within its list."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point))
        (idx (pchist2-edit--get-field-index-at-point)))
    (pcase field
      ('switch (pchist2-edit--move-switch-up idx))
      ('target (pchist2-edit--move-target-up idx))
      ('installer-header (pchist2-edit--move-installer-up idx))
      ('installer-switch (pchist2-edit--move-installer-switch-up idx))
      ('installer-artifact (pchist2-edit--move-installer-artifact-up idx))
      (_ (message "Cannot move this item")))))

(defun pchist2-edit-move-down ()
  "Move the item at point down within its list."
  (interactive)
  (let ((field (pchist2-edit--get-field-at-point))
        (idx (pchist2-edit--get-field-index-at-point)))
    (pcase field
      ('switch (pchist2-edit--move-switch-down idx))
      ('target (pchist2-edit--move-target-down idx))
      ('installer-header (pchist2-edit--move-installer-down idx))
      ('installer-switch (pchist2-edit--move-installer-switch-down idx))
      ('installer-artifact (pchist2-edit--move-installer-artifact-down idx))
      (_ (message "Cannot move this item")))))

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
      ('installer-header (message "Use 'k' to delete this installer, or navigate to a field to edit it"))
      ('installer-command (pchist2-edit--modify-installer-command))
      ('installer-switch (pchist2-edit--modify-installer-switch))
      ('installer-switches-empty (pchist2-edit--add-installer-switch))
      ('installer-artifact (pchist2-edit--modify-installer-artifact))
      ('installer-artifacts-empty (pchist2-edit--add-installer-artifact))
      ('installer-host (pchist2-edit--modify-installer-host))
      ('installer-dest (pchist2-edit--modify-installer-dest)))))

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

(defun pchist2-edit--move-switch-up (idx)
  "Move switch at IDX up one position."
  (if (zerop idx)
      (message "Already at top of switches")
    (let ((item (nth idx pchist2-edit--switches)))
      (setq pchist2-edit--switches
            (append (cl-subseq pchist2-edit--switches 0 (1- idx))
                    (list item)
                    (list (nth (1- idx) pchist2-edit--switches))
                    (cl-subseq pchist2-edit--switches (1+ idx))))
      (pchist2-edit--render)
      ;; Move point to the moved item's new position
      (pchist2-edit--goto-switch (1- idx)))))

(defun pchist2-edit--move-switch-down (idx)
  "Move switch at IDX down one position."
  (if (>= idx (1- (length pchist2-edit--switches)))
      (message "Already at bottom of switches")
    (let ((item (nth idx pchist2-edit--switches)))
      (setq pchist2-edit--switches
            (append (cl-subseq pchist2-edit--switches 0 idx)
                    (list (nth (1+ idx) pchist2-edit--switches))
                    (list item)
                    (cl-subseq pchist2-edit--switches (+ idx 2))))
      (pchist2-edit--render)
      ;; Move point to the moved item's new position
      (pchist2-edit--goto-switch (1+ idx)))))

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

(defun pchist2-edit--move-target-up (idx)
  "Move target at IDX up one position."
  (if (zerop idx)
      (message "Already at top of targets")
    (let ((item (nth idx pchist2-edit--targets)))
      (setq pchist2-edit--targets
            (append (cl-subseq pchist2-edit--targets 0 (1- idx))
                    (list item)
                    (list (nth (1- idx) pchist2-edit--targets))
                    (cl-subseq pchist2-edit--targets (1+ idx))))
      (pchist2-edit--render)
      ;; Move point to the moved item's new position
      (pchist2-edit--goto-target (1- idx)))))

(defun pchist2-edit--move-target-down (idx)
  "Move target at IDX down one position."
  (if (>= idx (1- (length pchist2-edit--targets)))
      (message "Already at bottom of targets")
    (let ((item (nth idx pchist2-edit--targets)))
      (setq pchist2-edit--targets
            (append (cl-subseq pchist2-edit--targets 0 idx)
                    (list (nth (1+ idx) pchist2-edit--targets))
                    (list item)
                    (cl-subseq pchist2-edit--targets (+ idx 2))))
      (pchist2-edit--render)
      ;; Move point to the moved item's new position
      (pchist2-edit--goto-target (1+ idx)))))

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

(defun pchist2-edit--move-installer-up (idx)
  "Move installer at IDX up one position."
  (if (zerop idx)
      (message "Already at top of installers")
    (let ((item (nth idx pchist2-edit--installers)))
      (setq pchist2-edit--installers
            (append (cl-subseq pchist2-edit--installers 0 (1- idx))
                    (list item)
                    (list (nth (1- idx) pchist2-edit--installers))
                    (cl-subseq pchist2-edit--installers (1+ idx))))
      (pchist2-edit--render)
      ;; Move point to the moved installer's new position
      (pchist2-edit--goto-installer (1- idx)))))

(defun pchist2-edit--move-installer-down (idx)
  "Move installer at IDX down one position."
  (if (>= idx (1- (length pchist2-edit--installers)))
      (message "Already at bottom of installers")
    (let ((item (nth idx pchist2-edit--installers)))
      (setq pchist2-edit--installers
            (append (cl-subseq pchist2-edit--installers 0 idx)
                    (list (nth (1+ idx) pchist2-edit--installers))
                    (list item)
                    (cl-subseq pchist2-edit--installers (+ idx 2))))
      (pchist2-edit--render)
      ;; Move point to the moved installer's new position
      (pchist2-edit--goto-installer (1+ idx)))))

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

(defun pchist2-edit--add-installer-switch ()
  "Add a switch to the current installer."
  (let* ((idx (if (consp (pchist2-edit--get-field-index-at-point))
                  (car (pchist2-edit--get-field-index-at-point))
                (pchist2-edit--get-field-index-at-point)))
         (installer (nth idx pchist2-edit--installers))
         (new-val (read-string "Installer switch: ")))
    (when (and new-val (not (string-empty-p new-val)))
      (let ((switches (alist-get 'switches installer)))
        (setf (alist-get 'switches installer)
              (append switches (list (string-trim new-val))))
        (pchist2-edit--render)))))

(defun pchist2-edit--modify-installer-switch ()
  "Modify an installer switch."
  (let* ((indices (pchist2-edit--get-field-index-at-point))
         (inst-idx (car indices))
         (sw-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (switches (alist-get 'switches installer))
         (current (nth sw-idx switches))
         (new-val (read-string "Installer switch: " current)))
    (when (and new-val (not (string-empty-p new-val)))
      (setf (nth sw-idx switches) (string-trim new-val))
      (pchist2-edit--render))))

(defun pchist2-edit--delete-installer-switch (indices)
  "Delete installer switch at INDICES (cons of inst-idx . sw-idx)."
  (let* ((inst-idx (car indices))
         (sw-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (switches (alist-get 'switches installer)))
    (setf (alist-get 'switches installer)
          (append (cl-subseq switches 0 sw-idx)
                  (cl-subseq switches (1+ sw-idx))))
    (pchist2-edit--render)))

(defun pchist2-edit--move-installer-switch-up (indices)
  "Move installer switch at INDICES up one position."
  (let* ((inst-idx (car indices))
         (sw-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (switches (alist-get 'switches installer)))
    (if (zerop sw-idx)
        (message "Already at top of installer switches")
      (let ((item (nth sw-idx switches)))
        (setf (alist-get 'switches installer)
              (append (cl-subseq switches 0 (1- sw-idx))
                      (list item)
                      (list (nth (1- sw-idx) switches))
                      (cl-subseq switches (1+ sw-idx))))
        (pchist2-edit--render)
        (pchist2-edit--goto-installer-switch inst-idx (1- sw-idx))))))

(defun pchist2-edit--move-installer-switch-down (indices)
  "Move installer switch at INDICES down one position."
  (let* ((inst-idx (car indices))
         (sw-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (switches (alist-get 'switches installer)))
    (if (>= sw-idx (1- (length switches)))
        (message "Already at bottom of installer switches")
      (let ((item (nth sw-idx switches)))
        (setf (alist-get 'switches installer)
              (append (cl-subseq switches 0 sw-idx)
                      (list (nth (1+ sw-idx) switches))
                      (list item)
                      (cl-subseq switches (+ sw-idx 2))))
        (pchist2-edit--render)
        (pchist2-edit--goto-installer-switch inst-idx (1+ sw-idx))))))

(defun pchist2-edit--add-installer-artifact ()
  "Add an artifact to the current installer."
  (let* ((idx (if (consp (pchist2-edit--get-field-index-at-point))
                  (car (pchist2-edit--get-field-index-at-point))
                (pchist2-edit--get-field-index-at-point)))
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

(defun pchist2-edit--delete-installer-artifact (indices)
  "Delete installer artifact at INDICES (cons of inst-idx . art-idx)."
  (let* ((inst-idx (car indices))
         (art-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (artifacts (alist-get 'artifacts installer)))
    (setf (alist-get 'artifacts installer)
          (append (cl-subseq artifacts 0 art-idx)
                  (cl-subseq artifacts (1+ art-idx))))
    (pchist2-edit--render)))

(defun pchist2-edit--move-installer-artifact-up (indices)
  "Move installer artifact at INDICES up one position."
  (let* ((inst-idx (car indices))
         (art-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (artifacts (alist-get 'artifacts installer)))
    (if (zerop art-idx)
        (message "Already at top of installer artifacts")
      (let ((item (nth art-idx artifacts)))
        (setf (alist-get 'artifacts installer)
              (append (cl-subseq artifacts 0 (1- art-idx))
                      (list item)
                      (list (nth (1- art-idx) artifacts))
                      (cl-subseq artifacts (1+ art-idx))))
        (pchist2-edit--render)
        (pchist2-edit--goto-installer-artifact inst-idx (1- art-idx))))))

(defun pchist2-edit--move-installer-artifact-down (indices)
  "Move installer artifact at INDICES down one position."
  (let* ((inst-idx (car indices))
         (art-idx (cdr indices))
         (installer (nth inst-idx pchist2-edit--installers))
         (artifacts (alist-get 'artifacts installer)))
    (if (>= art-idx (1- (length artifacts)))
        (message "Already at bottom of installer artifacts")
      (let ((item (nth art-idx artifacts)))
        (setf (alist-get 'artifacts installer)
              (append (cl-subseq artifacts 0 art-idx)
                      (list (nth (1+ art-idx) artifacts))
                      (list item)
                      (cl-subseq artifacts (+ art-idx 2))))
        (pchist2-edit--render)
        (pchist2-edit--goto-installer-artifact inst-idx (1+ art-idx))))))

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

  ;; Refresh and return to select screen
  (let ((select-buffer (get-buffer "*pchist2-commands*")))
    (if select-buffer
        (progn
          (quit-window t)
          (switch-to-buffer select-buffer)
          (when (fboundp 'pchist2-select-refresh)
            (pchist2-select-refresh)))
      ;; No select buffer, just quit
      (quit-window t))))

(defun pchist2-edit-cancel ()
  "Cancel editing and exit, confirming if changes were made."
  (interactive)
  (if (pchist2-edit--has-changes)
      (when (yes-or-no-p "Discard changes? ")
        (pchist2-edit--return-to-select))
    (pchist2-edit--return-to-select)))

(defun pchist2-edit--return-to-select ()
  "Return to select screen in same window."
  (let ((select-buffer (get-buffer "*pchist2-commands*")))
    (if select-buffer
        (progn
          (quit-window t)
          (switch-to-buffer select-buffer))
      ;; No select buffer, just quit
      (quit-window t))))

(defun pchist2-edit--has-changes ()
  "Return non-nil if the command has been modified."
  (let ((current-state (list pchist2-edit--command
                            pchist2-edit--switches
                            pchist2-edit--targets
                            pchist2-edit--installers)))
    (not (equal current-state pchist2-edit--initial-state))))

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
      (setq pchist2-edit--help-visible nil)

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

      ;; Capture initial state for change detection
      (setq pchist2-edit--initial-state (list pchist2-edit--command
                                              pchist2-edit--switches
                                              pchist2-edit--targets
                                              pchist2-edit--installers))

      (pchist2-edit--render))

    ;; Switch in same window (don't create new splits)
    (switch-to-buffer buffer)))

(provide 'pchist2-ui-edit)
;;; pchist2-ui-edit.el ends here
