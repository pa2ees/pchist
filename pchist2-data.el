;;; pchist2-data.el --- Core data structures and CRUD for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides the core data layer for pchist v2, implementing:
;; - JSON-based persistent storage with file locking
;; - CRUD operations for project compile commands
;; - Automatic silent deduplication
;; - Query helpers for command suggestions
;;
;; Data structures:
;; - Command: project, command, switches (list), targets (list),
;;            installers (list of installer records), last_used (ISO8601 string)
;; - Installer: command, switches (list), artifacts (list), host (string or nil),
;;              dest_path (string)
;;
;; Storage format: JSON file at ~/.emacs.d/pchist/commands.json
;; In-memory format: List of alists with symbol keys

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Configuration

(defcustom pchist2-storage-file
  (expand-file-name "pchist/commands.json" user-emacs-directory)
  "Path to the JSON file storing pchist command history."
  :type 'file
  :group 'pchist2)

(defcustom pchist2-lock-timeout 5.0
  "Maximum seconds to wait for file lock acquisition."
  :type 'number
  :group 'pchist2)

;;; Internal State

(defvar pchist2--commands nil
  "In-memory list of all commands across all projects.
Each command is an alist with keys: project, command, switches, targets,
installers, last_used.")

(defvar pchist2--loaded nil
  "Non-nil if data has been loaded from disk.")

;;; File Locking Utilities

(defun pchist2--lock-file ()
  "Return the path to the lock file."
  (concat pchist2-storage-file ".lock"))

(defun pchist2--acquire-lock ()
  "Acquire exclusive lock on storage file.
Returns non-nil on success. Waits up to `pchist2-lock-timeout' seconds.
Uses make-directory for atomic lock creation."
  (let ((lock-file (pchist2--lock-file))
        (start-time (float-time))
        (acquired nil))
    (pchist2--ensure-directory)
    (while (and (not acquired)
                (< (- (float-time) start-time) pchist2-lock-timeout))
      (condition-case nil
          (progn
            ;; Try to create lock directory atomically
            (make-directory lock-file nil)
            ;; Write PID into lock for debugging
            (with-temp-buffer
              (insert (format "%d\n" (emacs-pid)))
              (write-region (point-min) (point-max)
                            (expand-file-name "pid" lock-file)
                            nil 'silent))
            (setq acquired t))
        (file-already-exists
         ;; Lock exists, check if stale
         (when (file-exists-p lock-file)
           (condition-case nil
               (let* ((attrs (file-attributes lock-file))
                      (lock-age (- (float-time) (float-time (nth 5 attrs)))))
                 ;; If lock is older than timeout * 2, consider it stale
                 (when (> lock-age (* 2 pchist2-lock-timeout))
                   (ignore-errors (delete-directory lock-file t))))
             (error nil)))
         ;; Wait a bit before retrying
         (sleep-for 0.05))))
    acquired))

(defun pchist2--release-lock ()
  "Release the lock on storage file."
  (let ((lock-file (pchist2--lock-file)))
    (when (file-exists-p lock-file)
      (ignore-errors (delete-directory lock-file t)))))

(defmacro pchist2--with-file-lock (&rest body)
  "Execute BODY with exclusive lock on storage file."
  `(if (pchist2--acquire-lock)
       (unwind-protect
           (progn ,@body)
         (pchist2--release-lock))
     (error "Failed to acquire lock on %s" pchist2-storage-file)))

;;; JSON Serialization

(defun pchist2--alist-to-json-alist (alist)
  "Convert alist with symbol keys to alist with string keys for JSON encoding.
Recursively handles nested alists."
  (mapcar (lambda (pair)
            (let ((key (symbol-name (car pair)))
                  (val (cdr pair)))
              (cons key
                    (cond
                     ;; If value is a list of alists, convert each recursively
                     ((and (listp val)
                           (not (null val))
                           (listp (car val))
                           (consp (car val)))
                      (mapcar #'pchist2--alist-to-json-alist val))
                     ;; Otherwise keep as-is
                     (t val)))))
          alist))

(defun pchist2--json-alist-to-alist (json-alist)
  "Convert alist with string keys from JSON to alist with symbol keys.
Recursively handles nested alists."
  (mapcar (lambda (pair)
            (let ((key (intern (car pair)))
                  (val (cdr pair)))
              (cons key
                    (cond
                     ;; If value is a list of alists, convert each recursively
                     ((and (listp val)
                           (not (null val))
                           (listp (car val))
                           (consp (car val)))
                      (mapcar #'pchist2--json-alist-to-alist val))
                     ;; Otherwise keep as-is
                     (t val)))))
          json-alist))

(defun pchist2--serialize-commands (commands)
  "Convert COMMANDS list to JSON-serializable format."
  (mapcar #'pchist2--alist-to-json-alist commands))

(defun pchist2--deserialize-commands (json-data)
  "Convert JSON-DATA to internal command list format."
  (mapcar #'pchist2--json-alist-to-alist json-data))

;;; File I/O

(defun pchist2-load ()
  "Load commands from JSON storage file.
Returns the loaded command list. Creates empty storage if file doesn't exist.
Uses file locking for multi-process safety."
  (interactive)
  (pchist2--ensure-directory)
  (pchist2--with-file-lock
   (let ((file pchist2-storage-file))
     (if (file-exists-p file)
         (condition-case err
             (with-temp-buffer
               (insert-file-contents file)
               (let* ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'string)
                      (json-false nil)
                      (json-data (json-read)))
                 (setq pchist2--commands (pchist2--deserialize-commands json-data))
                 (setq pchist2--loaded t)
                 pchist2--commands))
           (error
            (message "Error loading pchist2 data from %s: %S" file err)
            (setq pchist2--commands nil)
            (setq pchist2--loaded t)
            nil))
       ;; File doesn't exist, initialize empty
       (setq pchist2--commands nil)
       (setq pchist2--loaded t)
       nil))))

(defun pchist2--ensure-directory ()
  "Ensure the storage directory exists."
  (let ((dir (file-name-directory pchist2-storage-file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun pchist2-save ()
  "Save commands to JSON storage file.
Uses file locking for multi-process safety."
  (interactive)
  (pchist2--ensure-loaded)
  (pchist2--with-file-lock
   (pchist2--ensure-directory)
   (let ((json-encoding-pretty-print t)
         (json-data (pchist2--serialize-commands pchist2--commands)))
     (with-temp-buffer
       (insert (json-encode json-data))
       (write-region (point-min) (point-max) pchist2-storage-file nil 'silent)))))

(defun pchist2--ensure-loaded ()
  "Ensure data is loaded from disk."
  (unless pchist2--loaded
    (pchist2-load)))

;;; Command Comparison and Deduplication

(defun pchist2--commands-equal-p (cmd1 cmd2)
  "Return non-nil if CMD1 and CMD2 represent the same command.
Compares all fields except last_used."
  (and (equal (alist-get 'project cmd1) (alist-get 'project cmd2))
       (equal (alist-get 'command cmd1) (alist-get 'command cmd2))
       (equal (alist-get 'switches cmd1) (alist-get 'switches cmd2))
       (equal (alist-get 'targets cmd1) (alist-get 'targets cmd2))
       (equal (alist-get 'installers cmd1) (alist-get 'installers cmd2))))

(defun pchist2--find-duplicate (cmd commands)
  "Find a command in COMMANDS that matches CMD (ignoring last_used).
Returns the matching command or nil."
  (cl-find-if (lambda (existing)
                (pchist2--commands-equal-p cmd existing))
              commands))

;;; CRUD Operations

(defun pchist2-add-command (project command switches targets installers)
  "Add a new command to the history.
PROJECT is the project root path (string).
COMMAND is the command string (e.g., \"./build.sh\").
SWITCHES is a list of switch strings (can contain spaces).
TARGETS is a list of target strings.
INSTALLERS is a list of installer alists, each with keys:
  command, switches, artifacts, host, dest_path.

If an identical command already exists (ignoring last_used), silently updates
its last_used timestamp instead of creating a duplicate.

Returns the command alist (either newly created or existing)."
  (interactive
   (list (read-string "Project: ")
         (read-string "Command: ")
         (split-string (read-string "Switches (space-separated): ") " " t)
         (split-string (read-string "Targets (space-separated): ") " " t)
         nil))
  (pchist2--ensure-loaded)
  (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S.%6N"))
         (new-cmd `((project . ,project)
                    (command . ,command)
                    (switches . ,switches)
                    (targets . ,targets)
                    (installers . ,installers)
                    (last_used . ,timestamp)))
         (existing (pchist2--find-duplicate new-cmd pchist2--commands)))
    (if existing
        ;; Update existing command's last_used
        (progn
          (setf (alist-get 'last_used existing) timestamp)
          (pchist2-save)
          existing)
      ;; Add new command
      (push new-cmd pchist2--commands)
      (pchist2-save)
      new-cmd)))

(defun pchist2-get-commands (&optional project)
  "Get all commands, optionally filtered by PROJECT.
If PROJECT is nil, returns all commands across all projects.
Returns a list of command alists."
  (interactive)
  (pchist2--ensure-loaded)
  (if project
      (cl-remove-if-not (lambda (cmd)
                          (equal (alist-get 'project cmd) project))
                        pchist2--commands)
    pchist2--commands))

(defun pchist2-update-command (old-cmd updated-fields)
  "Update OLD-CMD with UPDATED-FIELDS.
OLD-CMD is a command alist (typically obtained from pchist2-get-commands).
UPDATED-FIELDS is an alist of fields to update (e.g., ((switches . (...)) ...)).
Always updates last_used to current timestamp.
Returns the updated command alist."
  (pchist2--ensure-loaded)
  (let ((cmd (cl-find old-cmd pchist2--commands)))
    (unless cmd
      (error "Command not found: %S" old-cmd))
    ;; Update fields
    (dolist (field updated-fields)
      (setf (alist-get (car field) cmd) (cdr field)))
    ;; Update timestamp
    (setf (alist-get 'last_used cmd) (format-time-string "%Y-%m-%dT%H:%M:%S.%6N"))
    (pchist2-save)
    cmd))

(defun pchist2-touch-command (cmd)
  "Update the last_used timestamp for CMD without changing any fields.
CMD is a command alist (typically obtained from pchist2-get-commands).
Returns the updated command alist."
  (pchist2--ensure-loaded)
  (let ((found-cmd (cl-find cmd pchist2--commands)))
    (unless found-cmd
      (error "Command not found: %S" cmd))
    ;; Update timestamp
    (setf (alist-get 'last_used found-cmd) (format-time-string "%Y-%m-%dT%H:%M:%S.%6N"))
    (pchist2-save)
    found-cmd))

(defun pchist2-delete-command (cmd)
  "Delete CMD from the history.
CMD is a command alist (typically obtained from pchist2-get-commands)."
  (interactive)
  (pchist2--ensure-loaded)
  (setq pchist2--commands (cl-delete cmd pchist2--commands :test #'eq))
  (pchist2-save))

(defun pchist2-clear-all ()
  "Clear all commands from history. USE WITH CAUTION."
  (interactive)
  (when (yes-or-no-p "Really clear all pchist2 command history? ")
    (setq pchist2--commands nil)
    (setq pchist2--loaded t)
    (pchist2-save)
    (message "pchist2 history cleared")))

;;; Query Helpers for Suggestions

(defun pchist2-get-unique-switches (project command-name)
  "Get all unique switches used with COMMAND-NAME in PROJECT.
Returns a list of switch strings, most recently used first."
  (pchist2--ensure-loaded)
  (let* ((cmds (cl-remove-if-not
                (lambda (cmd)
                  (and (equal (alist-get 'project cmd) project)
                       (equal (alist-get 'command cmd) command-name)))
                pchist2--commands))
         ;; Sort by last_used descending
         (sorted (cl-sort cmds #'string>
                          :key (lambda (c) (alist-get 'last_used c))))
         (all-switches (apply #'append (mapcar (lambda (c) (alist-get 'switches c))
                                                sorted))))
    ;; Remove duplicates, preserving order
    (cl-delete-duplicates all-switches :test #'equal :from-end t)))

(defun pchist2-get-unique-targets (project command-name)
  "Get all unique targets used with COMMAND-NAME in PROJECT.
Returns a list of target strings, most recently used first."
  (pchist2--ensure-loaded)
  (let* ((cmds (cl-remove-if-not
                (lambda (cmd)
                  (and (equal (alist-get 'project cmd) project)
                       (equal (alist-get 'command cmd) command-name)))
                pchist2--commands))
         (sorted (cl-sort cmds #'string>
                          :key (lambda (c) (alist-get 'last_used c))))
         (all-targets (apply #'append (mapcar (lambda (c) (alist-get 'targets c))
                                               sorted))))
    (cl-delete-duplicates all-targets :test #'equal :from-end t)))

(defun pchist2-get-unique-commands (project)
  "Get all unique command names used in PROJECT.
Returns a list of command strings, most recently used first."
  (pchist2--ensure-loaded)
  (let* ((cmds (cl-remove-if-not
                (lambda (cmd)
                  (equal (alist-get 'project cmd) project))
                pchist2--commands))
         (sorted (cl-sort cmds #'string>
                          :key (lambda (c) (alist-get 'last_used c))))
         (commands (mapcar (lambda (c) (alist-get 'command c)) sorted)))
    (cl-delete-duplicates commands :test #'equal :from-end t)))

(defun pchist2-get-unique-projects ()
  "Get all unique project paths.
Returns a list of project path strings, most recently used first."
  (pchist2--ensure-loaded)
  (let* ((sorted (cl-sort (copy-sequence pchist2--commands) #'string>
                          :key (lambda (c) (alist-get 'last_used c))))
         (projects (mapcar (lambda (c) (alist-get 'project c)) sorted)))
    (cl-delete-duplicates projects :test #'equal :from-end t)))

;;; Installer Helpers

(defun pchist2-get-installer-commands (project command-name)
  "Get all unique installer command names used with COMMAND-NAME in PROJECT.
Returns a list of installer command strings (e.g., \"scp\", \"rsync\")."
  (pchist2--ensure-loaded)
  (let* ((cmds (cl-remove-if-not
                (lambda (cmd)
                  (and (equal (alist-get 'project cmd) project)
                       (equal (alist-get 'command cmd) command-name)))
                pchist2--commands))
         (all-installers (apply #'append
                                (mapcar (lambda (c) (alist-get 'installers c))
                                        cmds)))
         (installer-cmds (mapcar (lambda (i) (alist-get 'command i))
                                 all-installers)))
    (cl-delete-duplicates installer-cmds :test #'equal)))

(defun pchist2-get-installer-hosts (project)
  "Get all unique installer hosts used in PROJECT.
Returns a list of host strings."
  (pchist2--ensure-loaded)
  (let* ((cmds (cl-remove-if-not
                (lambda (cmd)
                  (equal (alist-get 'project cmd) project))
                pchist2--commands))
         (all-installers (apply #'append
                                (mapcar (lambda (c) (alist-get 'installers c))
                                        cmds)))
         (hosts (mapcar (lambda (i) (alist-get 'host i)) all-installers)))
    (cl-delete-duplicates (delq nil hosts) :test #'equal)))

;;; Debug/Inspection Functions

(defun pchist2-dump-commands ()
  "Display all commands in a readable format."
  (interactive)
  (pchist2--ensure-loaded)
  (with-output-to-temp-buffer "*pchist2-commands*"
    (princ (format "Total commands: %d\n\n" (length pchist2--commands)))
    (dolist (cmd pchist2--commands)
      (princ (format "Project: %s\n" (alist-get 'project cmd)))
      (princ (format "Command: %s\n" (alist-get 'command cmd)))
      (princ (format "Switches: %S\n" (alist-get 'switches cmd)))
      (princ (format "Targets: %S\n" (alist-get 'targets cmd)))
      (princ (format "Installers: %S\n" (alist-get 'installers cmd)))
      (princ (format "Last used: %s\n\n" (alist-get 'last_used cmd))))))

(provide 'pchist2-data)
;;; pchist2-data.el ends here
