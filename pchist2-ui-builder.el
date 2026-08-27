;;; pchist2-ui-builder.el --- Command builder for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (helm "3.0"))

;;; Commentary:

;; This module provides the interactive command builder for pchist v2.
;; It guides the user through creating a new command with suggestions
;; based on history.

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'helm)
(require 'projectile)
(require 'cl-lib)

;;; Builder State

(defvar pchist2-builder--project nil
  "Project root for the command being built.")

(defvar pchist2-builder--command nil
  "Base command being built.")

(defvar pchist2-builder--switches nil
  "List of switches being built.")

(defvar pchist2-builder--targets nil
  "List of targets being built.")

(defvar pchist2-builder--installers nil
  "List of installer records being built.")

;;; Display Helpers

(defun pchist2-builder--format-current-state ()
  "Format the current builder state as a display string."
  (pchist2-format-builder-state pchist2-builder--command
                                pchist2-builder--switches
                                pchist2-builder--targets
                                pchist2-builder--installers))

;;; Prompting Functions

(defun pchist2-builder--prompt-command ()
  "Prompt for the base command with suggestions from history."
  (let* ((candidates (pchist2-get-unique-commands pchist2-builder--project))
         (prompt (format "Command [%s]: "
                         (pchist2-builder--format-current-state)))
         (choice (helm-comp-read prompt
                                 (or candidates '())
                                 :must-match nil
                                 :allow-nest t)))
    (when (and choice (not (string-empty-p choice)))
      (setq pchist2-builder--command (string-trim choice)))))

(defun pchist2-builder--prompt-switch ()
  "Prompt for a switch with suggestions from history.
Returns nil when user is done adding switches."
  (let* ((candidates (cons "<done>" (pchist2-get-unique-switches
                                     pchist2-builder--project
                                     pchist2-builder--command)))
         (prompt (format "Switch [%s]: "
                         (pchist2-builder--format-current-state)))
         (choice (helm-comp-read prompt
                                 candidates
                                 :must-match nil
                                 :allow-nest t)))
    (if (or (null choice)
            (string-empty-p choice)
            (string= choice "<done>"))
        nil
      (push (string-trim choice) pchist2-builder--switches)
      t)))

(defun pchist2-builder--prompt-target ()
  "Prompt for a target with suggestions from history.
Returns nil when user is done adding targets."
  (let* ((candidates (cons "<done>" (pchist2-get-unique-targets
                                     pchist2-builder--project
                                     pchist2-builder--command)))
         (prompt (format "Target [%s]: "
                         (pchist2-builder--format-current-state)))
         (choice (helm-comp-read prompt
                                 candidates
                                 :must-match nil
                                 :allow-nest t)))
    (if (or (null choice)
            (string-empty-p choice)
            (string= choice "<done>"))
        nil
      (push (string-trim choice) pchist2-builder--targets)
      t)))

(defun pchist2-builder--prompt-installer ()
  "Prompt for installer details.
Returns an installer alist or nil if user is done adding installers."
  (let* ((cmd-candidates (cons "<done>" (pchist2-get-installer-commands
                                         pchist2-builder--project
                                         pchist2-builder--command)))
         (inst-cmd (helm-comp-read "Installer command: "
                                   cmd-candidates
                                   :must-match nil
                                   :allow-nest t)))
    (if (or (null inst-cmd)
            (string-empty-p inst-cmd)
            (string= inst-cmd "<done>"))
        nil
      ;; Build installer record
      (let* ((switches-str (read-string "Installer switches (space-separated): "))
             (switches (if (string-empty-p switches-str)
                           nil
                         (split-string switches-str " " t)))
             (artifacts-str (read-string "Artifacts (space-separated paths): "))
             (artifacts (if (string-empty-p artifacts-str)
                            nil
                          (split-string artifacts-str " " t)))
             (host-candidates (cons "<local>" (pchist2-get-installer-hosts
                                               pchist2-builder--project)))
             (host-choice (helm-comp-read "Host: "
                                          host-candidates
                                          :must-match nil
                                          :allow-nest t))
             (host (if (or (null host-choice)
                           (string-empty-p host-choice)
                           (string= host-choice "<local>"))
                       nil
                     host-choice))
             (dest-path (read-string "Destination path: ")))
        `((command . ,inst-cmd)
          (switches . ,switches)
          (artifacts . ,artifacts)
          (host . ,host)
          (dest_path . ,(if (string-empty-p dest-path) nil dest-path)))))))

;;; Main Builder Function

(defun pchist2-ui-build-command (project-root &optional initial-cmd)
  "Interactively build a command for PROJECT-ROOT.
If INITIAL-CMD is provided, pre-populate the builder with its values.
Returns the built command or nil if cancelled."
  (unless project-root
    (user-error "Not in a projectile project"))

  ;; Initialize builder state from initial-cmd if provided
  (setq pchist2-builder--project project-root
        pchist2-builder--command (alist-get 'command initial-cmd)
        pchist2-builder--switches (alist-get 'switches initial-cmd)
        pchist2-builder--targets (alist-get 'targets initial-cmd)
        pchist2-builder--installers (alist-get 'installers initial-cmd))

    ;; Prompt for command
    (pchist2-builder--prompt-command)
    (unless pchist2-builder--command
      (user-error "Command is required"))

    ;; Prompt for switches
    (while (pchist2-builder--prompt-switch))
    (setq pchist2-builder--switches (reverse pchist2-builder--switches))

    ;; Prompt for targets
    (while (pchist2-builder--prompt-target))
    (setq pchist2-builder--targets (reverse pchist2-builder--targets))

    ;; Prompt for installers
    (let ((done nil))
      (while (not done)
        (let ((installer (pchist2-builder--prompt-installer)))
          (if installer
              (push installer pchist2-builder--installers)
            (setq done t)))))
    (setq pchist2-builder--installers (reverse pchist2-builder--installers))

    ;; Return the built command as an alist
    `((project . ,pchist2-builder--project)
      (command . ,pchist2-builder--command)
      (switches . ,pchist2-builder--switches)
      (targets . ,pchist2-builder--targets)
      (installers . ,pchist2-builder--installers))))

;;;###autoload
(defun pchist2-ui-create-command (&optional project-root)
  "Interactively create a new command for PROJECT-ROOT.
If PROJECT-ROOT is nil, uses current projectile project.
Prompts for command, switches, targets, and installers with suggestions
from history."
  (interactive)
  (let* ((project-root (or project-root (projectile-project-root)))
         (built-cmd (pchist2-ui-build-command project-root nil)))
    (when built-cmd
      ;; Save the command
      (pchist2-add-command (alist-get 'project built-cmd)
                           (alist-get 'command built-cmd)
                           (alist-get 'switches built-cmd)
                           (alist-get 'targets built-cmd)
                           (alist-get 'installers built-cmd))
      (message "Command saved: %s"
               (pchist2-format-builder-state (alist-get 'command built-cmd)
                                             (alist-get 'switches built-cmd)
                                             (alist-get 'targets built-cmd)
                                             (alist-get 'installers built-cmd))))))

(provide 'pchist2-ui-builder)
;;; pchist2-ui-builder.el ends here
