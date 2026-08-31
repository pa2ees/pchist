;;; pchist2-format.el --- Formatting utilities for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Pure formatting functions for pchist v2.
;; No dependencies on UI frameworks (Helm, etc.).
;; These functions can be used programmatically and are easily testable.

;;; Code:

(require 'cl-lib)

;;; Command Formatting

(defun pchist2-format-command (cmd &optional short-paths)
  "Format CMD as a human-readable string for display.
If SHORT-PATHS is non-nil, show only basenames for project paths."
  (let* ((project (alist-get 'project cmd))
         (command (alist-get 'command cmd))
         (switches (alist-get 'switches cmd))
         (targets (alist-get 'targets cmd))
         (installers (alist-get 'installers cmd))
         (project-display (if short-paths
                              (file-name-nondirectory (directory-file-name project))
                            project))
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
               (inst-switches (alist-get 'switches inst))
               (artifacts (alist-get 'artifacts inst))
               (host (alist-get 'host inst))
               (dest (alist-get 'dest_path inst))
               (inst-parts (list inst-cmd)))
          ;; Add installer switches
          (when inst-switches
            (setq inst-parts (append inst-parts inst-switches)))
          ;; Add artifacts
          (when artifacts
            (setq inst-parts (append inst-parts artifacts)))
          ;; Add destination
          (when (or host dest)
            (setq inst-parts (append inst-parts
                                     (list (format "%s%s"
                                                   (if host (concat host ":") "")
                                                   (or dest ""))))))
          ;; Build full installer string
          (setq parts (append parts
                              (list (concat "&& " (string-join inst-parts " "))))))))

    (format "[%s] %s" project-display (string-join parts " "))))

;;; Command Execution String (shared formatting)

(defun pchist2-format--build-command-string (command switches targets installers)
  "Build a command string from components.
COMMAND is the base command string.
SWITCHES is a list of switch strings.
TARGETS is a list of target strings.
INSTALLERS is a list of installer alists.
Returns the formatted command string."
  (let ((parts (list (or command "<<command>>"))))
    (when switches
      (setq parts (append parts switches)))
    (when targets
      (setq parts (append parts targets)))
    (when installers
      (dolist (inst installers)
        (let* ((cmd (alist-get 'command inst))
               (inst-switches (alist-get 'switches inst))
               (artifacts (alist-get 'artifacts inst))
               (host (alist-get 'host inst))
               (dest (alist-get 'dest_path inst))
               (inst-parts (list cmd)))
          ;; Add installer switches
          (when inst-switches
            (setq inst-parts (append inst-parts inst-switches)))
          ;; Add artifacts
          (when artifacts
            (setq inst-parts (append inst-parts artifacts)))
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

(defun pchist2-format-builder-state (command switches targets installers)
  "Format builder state as a display string.
COMMAND is the base command string.
SWITCHES is a list of switch strings.
TARGETS is a list of target strings.
INSTALLERS is a list of installer alists."
  (pchist2-format--build-command-string command switches targets installers))

(defun pchist2-format-command-for-execution (cmd)
  "Format CMD as an executable command string (no project prefix).
This is the string that should be passed to the shell."
  (pchist2-format--build-command-string
   (alist-get 'command cmd)
   (alist-get 'switches cmd)
   (alist-get 'targets cmd)
   (alist-get 'installers cmd)))

;;; Filter Descriptions

(defun pchist2-format-filter-description (filter-mode &optional specific-project current-project)
  "Format a description of the current filter mode.
FILTER-MODE is one of: current-project, specific-project, global.
SPECIFIC-PROJECT is the path when mode is specific-project.
CURRENT-PROJECT is the path when mode is current-project."
  (pcase filter-mode
    ('current-project
     (format "Current Project: %s" (or current-project "none")))
    ('specific-project
     (format "Specific Project: %s" (or specific-project "none")))
    ('global
     "All Projects")))

(provide 'pchist2-format)
;;; pchist2-format.el ends here
