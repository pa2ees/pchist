(require 'persist)
(require 'projectile)

(defun projectile-comphist--load-history ()
  (interactive)
  "Loads 'project-comphist-history' from persistent storage"
  (persist-defvar project-comphist-history nil
    "Contains the compile history for all projects")
  (persist-load 'project-comphist-history))

(defun projectile-comphist--get-project-root (project-root)
  (if project-root
      project-root
    (projectile-project-root)))

(defun projectile-comphist--get-project-comphist-entry (project-root)
  (assoc project-root project-comphist-history))

(defun projectile-comphist--set-project-comphist-entry (project-root project-history)
  (let ((project-history-entry (assoc project-root project-comphist-history)))
    (if project-history-entry
        (setcdr project-history-entry project-history)
      (add-to-list 'project-comphist-history (cons project-root project-history)))))

(defun projectile-comphist--get-or-create-project-comphist-entry (project-root)
  (if (not (projectile-comphist--get-project-comphist-entry project-root))
      (projectile-comphist--set-project-comphist-entry project-root nil))
  (projectile-comphist--get-project-comphist-entry project-root))

(defun projectile-comphist-get-compile-command-list (&optional project-root)
  (let* ((project-root (projectile-comphist--get-project-root project-root)))
    (cdr (projectile-comphist--get-or-create-project-comphist-entry project-root))))

(defun projectile-comphist-add-compile-command (compile-command &optional project-root)
  (let* ((project-root (projectile-comphist--get-project-root project-root))
         (comphist-entry (projectile-comphist--get-or-create-project-comphist-entry project-root)))
    (if (cdr comphist-entry)
        (setcdr comphist-entry (delete-dups (cons compile-command (cdr comphist-entry))))
      (setcdr comphist-entry (list compile-command)))
    (persist-save 'project-comphist-history)))
      
    
(defun projectile-comphist-compile (arg)
  (interactive "P")
  (if (not (projectile-project-p))
      (message "Not in a projectile project!")
    (let* ((compile-command-list (projectile-comphist-get-compile-command-list (projectile-project-root)))
           (selection (completing-read "Compile command: " compile-command-list)))
      (projectile-comphist-add-compile-command selection (projectile-project-root)) ;; puts command first in list
      (projectile-run-compilation selection projectile-compile-use-comint-mode))))


(projectile-comphist--load-history)

(provide 'projectile-comphist)
