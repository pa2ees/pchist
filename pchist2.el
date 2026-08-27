;;; pchist2.el --- Project Compile History v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (helm "3.0") (projectile "2.0"))
;; Keywords: tools, convenience, compile

;;; Commentary:

;; pchist2 provides a robust compile history system for managing project
;; compile commands with a Helm-based UI.
;;
;; Features:
;; - JSON-based persistent storage with file locking
;; - Automatic deduplication
;; - Helm interface for browsing, creating, editing, and running commands
;; - Filter by current project, specific project, or global
;; - Support for installers (scp, rsync, etc.)
;;
;; Quick start:
;;   (require 'pchist2)
;;   (global-set-key (kbd "C-x p c") #'pchist2-ui-select-command)
;;
;; Key bindings in selection interface:
;;   RET     - Run the selected command
;;   C-c f   - Cycle filter (current / specific / global)
;;   C-c n   - Create new command
;;   C-c e   - Edit selected command
;;   C-c d   - Duplicate and modify selected command
;;   C-c k   - Delete selected command

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'pchist2-ui)
(require 'pchist2-ui-builder)

;;;###autoload
(defalias 'pchist2-compile #'pchist2-ui-select-command
  "Main entry point for pchist2 compile history.")

(provide 'pchist2)
;;; pchist2.el ends here
