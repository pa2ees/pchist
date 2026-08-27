;;; pchist2.el --- Project Compile History v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (projectile "2.0"))
;; Keywords: tools, convenience, compile

;;; Commentary:

;; pchist2 provides a robust compile history system for managing project
;; compile commands with a custom UI.
;;
;; Features:
;; - JSON-based persistent storage with file locking
;; - Automatic deduplication
;; - Custom tabulated interface for browsing and managing commands
;; - Structured edit screen with one-per-line display and inline editing
;; - Filter by current project, specific project, or global
;; - Support for installers (scp, rsync, etc.) with full field editing
;; - Works with any completion framework (Helm, Ivy, Vertico, etc.)
;;
;; Quick start:
;;   (require 'pchist2)
;;   (global-set-key (kbd "C-x p c") #'pchist2-ui-select-command)
;;
;; Key bindings in selection interface:
;;   RET     - Run the selected command
;;   e       - Edit selected command
;;   c       - Create new command
;;   d       - Duplicate and modify selected command
;;   k       - Delete selected command
;;   f       - Cycle filter (current / specific / global)
;;   g       - Refresh list
;;   q       - Quit
;;
;; Key bindings in edit screen:
;;   RET/e   - Edit field at point
;;   a       - Add switch/target
;;   i       - Add installer
;;   k       - Delete item at point
;;   n/p     - Navigate between fields
;;   C-c C-c - Save changes
;;   C-c C-k - Cancel

;;; Code:

(require 'pchist2-data)
(require 'pchist2-format)
(require 'pchist2-ui-select)
(require 'pchist2-ui-edit)
(require 'pchist2-ui-completion)

;;;###autoload
(defalias 'pchist2-compile #'pchist2-ui-select-command
  "Main entry point for pchist2 compile history.")

(provide 'pchist2)
;;; pchist2.el ends here
