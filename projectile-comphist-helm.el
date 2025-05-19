;;; projectile-comphist-ui.el --- UI for interacting with structured compile history -*- lexical-binding: t; -*-

(require 'projectile-comphist-structured)
(require 'projectile-comphist-metadata)
(require 'projectile)
(require 'cl-lib)
(require 'helm)
(require 'helm-source)
(require 's)

(defun projectile-comphist--helm-insert-selection (selection)
  "Insert selected candidate into the minibuffer for editing."
  (with-helm-alive-p
    (helm-set-pattern selection)))

(defvar projectile-comphist--show-short-names-p nil
  "Determine how names are shown")

(defun projectile-comphist--fc-transform (candidates _source)
  (mapcar (lambda (cand)
            (let ((disp (if projectile-comphist--show-short-names-p
                            (file-name-nondirectory cand)
                          cand)))
              (cons disp cand)))
          candidates))

(defun projectile-comphist--fc-transform-command-entries (candidates _source)
  (mapcar (lambda (cand)
            (let ((formatted-cand (if (stringp cand)
                                      cand
                                    (projectile-comphist--format-command cand projectile-comphist--show-short-names-p))))
              (cons formatted-cand cand)))
          candidates))

(defun projectile-comphist--toggle-short-names ()
  (interactive)
  (setq projectile-comphist--show-short-names-p (not projectile-comphist--show-short-names-p))
  (helm-update)) ;; Reapplies display-transformer-fn with new logic

(defun projectile-comphist--create-keymap ()
  (let ((custom-helm-map (copy-keymap helm-map)))
    (define-key custom-helm-map (kbd "C-c C-p") 'projectile-comphist--toggle-short-names)
    custom-helm-map))

;;;;; MANAGEMENT COMMANDS ;;;;;;
(defun projectile-comphist-delete-project ()
  "Delete a project entry from projectile-comphist-structured-history."
  (interactive)
  (projectile-comphist--load-history)
  (let ((project-entries (mapcar #'car project-comphist-structured-history)))
    (if (null project-entries)
        (message "No projects.")
      (helm :sources
            (helm-build-sync-source "Delete Project Entry"
              :candidates project-entries
              :action (lambda (proj-root)
                        (setq project-comphist-structured-history
                              (cl-remove-if (lambda (entry)
                                              (string= (car entry) proj-root))
                                            project-comphist-structured-history))
                        (projectile-comphist--save-history)
                        (message "Deleted project: %s" proj-root)))
            :buffer "*Projectile Comphist Delete Project*"))))

(defun projectile-comphist-manage-commands (&optional project-root)
  "Manage previously used structured compile commands. Default action copies to kill ring."
  (interactive)
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (entry (projectile-comphist--get-project-entry project-root))
         (commands (plist-get (cdr entry) :commands)))
    (if (null commands)
        (message "No compile history found for project: %s" project-root)
      (let* ((display-strings (mapcar (lambda (cmd)
                                        (cons (projectile-comphist--format-command cmd projectile-comphist--show-short-names-p)  ; display with highlights
                                              (projectile-comphist--format-command cmd nil))) ; actual value
                                      commands)))
        (helm :sources
              (helm-build-sync-source "Manage Compile Commands"
                :candidates display-strings
                :action (list
                         (cons "Copy to kill ring" #'kill-new)
                         (cons "Delete" (lambda (selection)
                                           (let* ((index (cl-position selection display-strings :test #'string=))
                                                  (cmd (nth index commands)))
                                             (setf (plist-get (cdr entry) :commands)
                                                   (remove cmd (plist-get (cdr entry) :commands)))
                                             (projectile-comphist--save-history)
                                             (message "Deleted command."))))))
              :buffer "*Helm Manage Compile Commands*")))))


;;;;;; FORMATTING COMMANDS ;;;;;;
(defun projectile-comphist--format-command (cmd &optional format-for-view)
  "Return a human-readable string representation of CMD."
  (let ((base (plist-get cmd :command))
        (switches (string-join (plist-get cmd :switches) " "))
        (targets (string-join (plist-get cmd :targets) " "))
        (follow-ons (string-join (mapcar (lambda (follow-on)
                                           (concat "&& " (projectile-comphist--format-follow-on follow-on format-for-view)))
                                         (plist-get cmd :follow-ons)) " ")))
    (string-join (delq "" (list base switches targets follow-ons)) " ")))

(defun projectile-comphist--format-follow-on (follow-on &optional artifacts-name-only)
  "Return a human-readable string representation of the follow-on"
  (let* ((cmd (plist-get follow-on :cmd))
         (artifacts (plist-get follow-on :artifacts))
         (host (plist-get follow-on :host))
         (path (plist-get follow-on :path))
         (artifact-strs (if artifacts-name-only
                            (mapcar #'file-name-nondirectory artifacts)
                          artifacts))
         (dest-str (cond
                    ((and host path) (concat host ":" path))
                    (path path))))
    (string-join (delq nil (append (list cmd) artifact-strs (list dest-str))) " ")))

(defun projectile-comphist--command-builder-text (selection command-entry incomplete-follow-on)
  "Create the display text for the command builder, highlighting SELECTION."
  (let ((lines '())
        (command (plist-get command-entry :command))
        (switches (plist-get command-entry :switches))
        (targets (plist-get command-entry :targets))                 
        (complete-follow-ons (plist-get command-entry :follow-ons)))    

    ;; line 1
    (push (string-join
            (append '("Command Builder |")
                    (if (equal selection "command")
                        '("<<command>>")
                      (list command))
                    (if (string= selection "switch")
                        (append switches '("<<switch>>"))
                      switches)
                    (if (string= selection "target")
                        (append targets '("<<target>>"))
                      targets))
            " ")
          lines)
    
    ;; === Follow-ons ===
    (unless (null complete-follow-ons)
      ;; do all complete follow-ons
      (dolist (cfo complete-follow-ons)
        (push (concat "&& " (projectile-comphist--format-follow-on cfo t))
              lines)))
    (when incomplete-follow-on
      ;; do incomplete follow-on
      (let* ((ifo-cmd (plist-get incomplete-follow-on :cmd))
             (ifo-artifacts (mapcar #'file-name-nondirectory (plist-get incomplete-follow-on :artifacts)))
             (ifo-host (plist-get incomplete-follow-on :host))
             (ifo-path (plist-get incomplete-follow-on :path)))
        (push (string-join
               (append (list "&&")
                       (if (string= selection "fo-cmd")
                           '("<<cmd>>")
                         (list ifo-cmd))
                       (if (string= selection "fo-artifact")
                           (append ifo-artifacts '("<<artifact>>"))
                         ifo-artifacts)
                       (if (string= selection "fo-host")
                           '("<<host>>:<path>")
                         (if ifo-host (list ifo-host) '()))
                       (if (string= selection "fo-path")
                           (if ifo-host '(":<<path>>") '("<<path>>"))))
               " ")
              lines)))
    (string-join (reverse lines) " ")))



;;;;;; GET CANDIDATES ;;;;;;
(defun projectile-comphist-command-candidates ()
  "return a list of all unique base commands used in history."
  (delete-dups (mapcar (lambda (entry)
                         (plist-get entry :command))
                       (apply #'append (mapcar (lambda (e)
                                                 (plist-get (cdr e) :commands))
                                               project-comphist-structured-history)))))

(defun projectile-comphist-switch-candidates (command)
  "Return all unique switches used with the given COMMAND across all projects."
  (delete-dups (apply #'append
                      (mapcar (lambda (e)
                                (when (equal (plist-get e :command) command)
                                  (plist-get e :switches)))
                              (apply #'append (mapcar (lambda (e)
                                                        (plist-get (cdr e) :commands))
                                                      project-comphist-structured-history))))))

(defun projectile-comphist-target-candidates (command)
  "Return all unique targets used with the given COMMAND across all projects."
  (delete-dups (apply #'append
                      (mapcar (lambda (e)
                                (when (equal (plist-get e :command) command)
                                  (plist-get e :targets)))
                              (apply #'append (mapcar (lambda (e)
                                                        (plist-get (cdr e) :commands))
                                                      project-comphist-structured-history))))))

(defun projectile-comphist-follow-on-command-candidates ()
  "Return all unique follow-on command strings (:cmd) used across all projects."
  (delete-dups
   (apply #'append
          (mapcar (lambda (entry)
                    (mapcar (lambda (follow-on-command-entry)
                              (plist-get follow-on-command-entry :cmd))
                            (apply #'append
                                   (mapcar (lambda (command-entry)
                                             (plist-get command-entry :follow-ons))
                                           (plist-get (cdr entry) :commands)))))
                  project-comphist-structured-history))))

(defun projectile-comphist-follow-on-artifact-candidates (project-root)
  "Return all unique follow-on artifacts used in this project."
  (delete-dups
   (apply #'append
          (mapcar (lambda (follow-on-command-entry)
                    (plist-get follow-on-command-entry :artifacts))
                  (apply #'append
                         (mapcar (lambda (command-entry)
                                   (plist-get command-entry :follow-ons))
                                 (projectile-comphist--get-commands project-root)))))))


(defun projectile-comphist-follow-on-host-candidates ()
  "Return all unique follow-on command strings (:cmd) used across all projects."
  (delete-dups
   (apply #'append
          (mapcar (lambda (entry)
                    (mapcar (lambda (follow-on-command-entry)
                              (plist-get follow-on-command-entry :host))
                            (apply #'append
                                   (mapcar (lambda (command-entry)
                                             (plist-get command-entry :follow-ons))
                                           (plist-get (cdr entry) :commands)))))
                  project-comphist-structured-history))))

(defun projectile-comphist-follow-on-path-candidates ()
  "Return all unique follow-on command strings (:cmd) used across all projects."
  (delete-dups
   (apply #'append
          (mapcar (lambda (entry)
                    (mapcar (lambda (follow-on-entry)
                              (plist-get follow-on-entry :path))
                            (apply #'append
                                   (mapcar (lambda (command-entry)
                                             (plist-get command-entry :follow-ons))
                                           (plist-get (cdr entry) :commands)))))
                  project-comphist-structured-history))))

  
;;;;;; PROMPTS ;;;;;;
(defun projectile-comphist--command-prompt (command-entry)
  (let* ((command-candidates (projectile-comphist-command-candidates))
         (select-command-text (projectile-comphist--command-builder-text "command" command-entry '()))
         (choice (s-trim (helm-comp-read "Command: " command-candidates
                                         :must-match nil
                                         :name select-command-text
                                         :persistent-action #'projectile-comphist--helm-insert-selection))))
    (if (string-empty-p choice)
        nil
      (plist-put command-entry :command choice))))
      

(defun projectile-comphist--switch-prompt (command-entry)
  (let* ((command (plist-get command-entry :command))
         (switch-candidates (cons "<no more switches>" (projectile-comphist-switch-candidates command)))
         (select-switch-text (projectile-comphist--command-builder-text "switch" command-entry '()))
         (switches (plist-get command-entry :switches))
         (choice (s-trim (helm-comp-read "Switch: " switch-candidates
                                         :must-match nil
                                         :name select-switch-text
                                         :persistent-action #'projectile-comphist--helm-insert-selection))))
    (if (or (string-empty-p choice) (string= choice "<no more switches>"))
        nil
      (plist-put command-entry :switches (append switches (list choice))))))

(defun projectile-comphist--target-prompt (command-entry)
  (let* ((command (plist-get command-entry :command))
         (target-candidates (cons "<no more targets>" (projectile-comphist-target-candidates command)))
         (select-target-text (projectile-comphist--command-builder-text "target" command-entry '()))
         (targets (plist-get command-entry :targets))
         (choice (s-trim (helm-comp-read "Target: " target-candidates
                                         :must-match nil
                                         :name select-target-text
                                         :persistent-action #'projectile-comphist--helm-insert-selection))))
    (if (or (string-empty-p choice) (string= choice "<no more targets>"))
        nil
      (plist-put command-entry :targets (append targets (list choice))))))

(defun projectile-comphist--fo-cmd-prompt (command-entry current-follow-on)
  (let* ((fo-cmd-candidates (cons "<no more follow-ons>" (projectile-comphist-follow-on-command-candidates)))
         (select-fo-cmd-text (projectile-comphist--command-builder-text "fo-cmd" command-entry current-follow-on))
         (choice (helm-comp-read "Follow-on command: " fo-cmd-candidates
                                 :must-match nil
                                 :name select-fo-cmd-text
                                 :persistent-action #'projectile-comphist--helm-insert-selection)))
    (if (or (string-empty-p choice) (string= choice "<no more follow-ons>"))
        nil
      (plist-put current-follow-on :cmd (s-trim choice)))))

(defun projectile-comphist--fo-artifact-prompt (command-entry current-follow-on project-root)
  (let* ((fo-artifact-candidates-raw (cons "<no more artifacts>" (cons "<browse for artifacts>" (projectile-comphist-follow-on-artifact-candidates project-root))))
         (fo-artifact-candidates fo-artifact-candidates-raw)
         (select-fo-artifact-text (projectile-comphist--command-builder-text "fo-artifact" command-entry current-follow-on))
         (fo-artifacts (plist-get current-follow-on :artifacts))
         (custom-helm-map (projectile-comphist--create-keymap))
         (choice (helm-comp-read "Follow-on artifact: " fo-artifact-candidates
                                 :must-match nil
                                 :keymap custom-helm-map
                                 :name select-fo-artifact-text
                                 :fc-transformer #'projectile-comphist--fc-transform
                                 :persistent-action #'projectile-comphist--helm-insert-selection)))
    (if (string= choice "<browse for artifacts>")
        (let ((default-directory project-root))
          (setq choice (helm-read-file-name "Artifact: "))))
    (if (or (string-empty-p choice) (string= choice "<no more artifacts>"))
        nil
      (plist-put current-follow-on :artifacts
                 (append fo-artifacts (list (s-trim choice)))))))

(defun projectile-comphist--fo-host-prompt (command-entry current-follow-on)
  (let* ((fo-host-candidates (cons "<no host>" (projectile-comphist-follow-on-host-candidates)))
         (select-fo-host-text (projectile-comphist--command-builder-text "fo-host" command-entry current-follow-on))
         (choice (helm-comp-read "Follow-on host: " fo-host-candidates
                                 :must-match nil
                                 :name select-fo-host-text
                                 :persistent-action #'projectile-comphist--helm-insert-selection)))
    (if (string= choice "<no host>")
        nil
      (plist-put current-follow-on :host (s-trim choice)))))

(defun projectile-comphist--fo-path-prompt (command-entry current-follow-on)
  (let* ((fo-path-candidates (cons "<no path>" (projectile-comphist-follow-on-path-candidates)))
         (select-fo-path-text (projectile-comphist--command-builder-text "fo-path" command-entry current-follow-on))
         (choice (helm-comp-read "Follow-on path: " fo-path-candidates
                                 :must-match nil
                                 :name select-fo-path-text
                                 :persistent-action #'projectile-comphist--helm-insert-selection)))
    (if (string= choice "<no path>")
        nil
      (plist-put current-follow-on :path (s-trim choice)))))
                             

;;;;;; CREATION COMMANDS ;;;;;;
(defun projectile-comphist--create-follow-on (command-entry project-root)
  (let ((no-fo nil)
        (current-fo (list :cmd nil :artifacts '() :host nil :path nil)))
    ;; prompt for command
    (let ((fo-with-command (projectile-comphist--fo-cmd-prompt command-entry current-fo)))
      (if (null fo-with-command)
          (setq no-fo t)
        (setq current-fo fo-with-command)))

    (unless no-fo
      ;; prompt for artifacts
      (let ((artifacts-done nil))
        (while (not artifacts-done)
          ;; prompt for single artifact
          (let ((fo-with-artifacts (projectile-comphist--fo-artifact-prompt command-entry current-fo project-root)))
            (if (null fo-with-artifacts)
                (setq artifacts-done t)
              (setq current-fo fo-with-artifacts))))))
    
    (unless no-fo
      ;; prompt for host
      (let ((fo-with-host (projectile-comphist--fo-host-prompt command-entry current-fo)))
        (unless (null fo-with-host)
          (setq current-fo fo-with-host))))
    
    (unless no-fo
      ;; prompt for path
      (let ((fo-with-path (projectile-comphist--fo-path-prompt command-entry current-fo)))
        (unless (null fo-with-path)
          (setq current-fo fo-with-path))))

    (if no-fo
        nil
      current-fo)))
            
            
(defun projectile-comphist-create-command (&optional project-root)
  "Interactively create a new structured compile command and save it."
  (interactive)
  (let* ((no-command nil)
         (project-root (projectile-comphist--get-project-root project-root))
         (command-entry (list :command nil :switches '() :targets '() :follow-ons '())))

    ;; prompt for command
    (let ((command-with-cmd (projectile-comphist--command-prompt command-entry)))
      (if (null command-with-cmd)
          (setq no-command t)
        (setq command-entry command-with-cmd)))

    (unless no-command
      ;; prompt for switch
      (let ((switches-done nil))
        (while (not switches-done)
          ;; prompt for single switch
          (let ((command-with-switches (projectile-comphist--switch-prompt command-entry)))
            (if (null command-with-switches)
                (setq switches-done t)
              (setq command-entry command-with-switches))))))

    (unless no-command
      ;; prompt for targets
      (let ((targets-done nil))
        (while (not targets-done)
          ;; prompt for single target
          (let ((command-with-targets (projectile-comphist--target-prompt command-entry)))
            (if (null command-with-targets)
                (setq targets-done t)
              (setq command-entry command-with-targets))))))

    (unless no-command
      ;; prompt for follow-ons
      (let ((follow-ons-done nil))
        (while (not follow-ons-done)
          ;; prompt for single follow-on
          (let ((follow-on (projectile-comphist--create-follow-on command-entry project-root))
                (follow-ons (plist-get command-entry :follow-ons)))
            (if (null follow-on)
                (setq follow-ons-done t)
              (setq command-entry
                    (plist-put command-entry :follow-ons (append follow-ons (list follow-on)))))))))

    (unless no-command
      ;; store and run command
      (projectile-comphist-add-structured-command command-entry project-root)
      ;; command-entry)))
      (projectile-comphist--format-command command-entry))))
                

(defun projectile-comphist-compile (&rest args)
  "Select and run a saved compile command for the current project using Helm."
  (interactive "P")
  (if (not (projectile-project-p))
      (message "Not in a projectile project!")
    (let* ((project-root (projectile-comphist--get-project-root))
           (commands (cons "<build new command>" (projectile-comphist--get-commands project-root)))
           ;; (commands (cons "<build new command>" (mapcar #'projectile-comphist--format-command (projectile-comphist--get-commands project-root))))
           (custom-helm-map (projectile-comphist--create-keymap)))
      (when (null commands)
        (projectile-comphist-create-command)
        (setq commands (projectile-comphist--get-commands project-root)))
      (let ((choice (helm-comp-read "Run Compile Command: " commands
                                    :must-match t
                                    :keymap custom-helm-map
                                    :name "Compile Commands"
                                    :fc-transformer #'projectile-comphist--fc-transform-command-entries)))
        (if (and (stringp choice) (string= choice "<build new command>"))
            (projectile-comphist--create-then-compile)
          (projectile-run-compilation (projectile-comphist--format-command choice)))))))
        
(defun projectile-comphist--create-then-compile ()
  "Create a new compile command, then run projectile-comphist-compile."
  (interactive)
  (projectile-comphist-create-command)
  (projectile-comphist-compile))

(provide 'projectile-comphist-helm)
