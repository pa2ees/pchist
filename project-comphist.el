(require 'persist)
(require 'projectile)

(defun project-comphist--load-history ()
  (interactive)
  "Loads 'project-comphist-history' from persistent storage"
  (persist-defvar project-comphist-history nil
    "Contains the compile history for all projects")
  (persist-load 'project-comphist-history))

(defun project-comphist--get-project-root (project-root)
  (if project-root
      project-root
    (projectile-project-root)))

(defun project-comphist--get-project-comphist-entry (project-root)
  (assoc project-root project-comphist-history))

(defun project-comphist--set-project-comphist-entry (project-root project-history)
  (let ((project-history-entry (assoc project-root project-comphist-history)))
    (if project-history-entry
        (setcdr project-history-entry project-history)
      (add-to-list 'project-comphist-history (cons project-root project-history)))))

(defun project-comphist--get-or-create-project-comphist-entry (project-root)
  (if (not (project-comphist--get-project-comphist-entry project-root))
      (project-comphist--set-project-comphist-entry project-root nil))
  (project-comphist--get-project-comphist-entry project-root))

(defun project-comphist-get-compile-command-list (&optional project-root)
  (let* ((project-root (project-comphist--get-project-root project-root)))
    (cdr (project-comphist--get-or-create-project-comphist-entry project-root))))

(defun project-comphist-add-compile-command (compile-command &optional project-root)
  (let* ((project-root (project-comphist--get-project-root project-root))
         (comphist-entry (project-comphist--get-or-create-project-comphist-entry project-root)))
    (if (cdr comphist-entry)
        (setcdr comphist-entry (delete-dups (cons compile-command (cdr comphist-entry))))
      (setcdr comphist-entry (list compile-command)))
    (persist-save 'project-comphist-history)))
      
    
(defun project-comphist-compile (arg)
  (interactive "P")
  (if (not (projectile-project-p))
      (message "Not in a projectile project!")
    (let* ((compile-command-list (project-comphist-get-compile-command-list (projectile-project-root)))
           (selection (completing-read "Compile command: " compile-command-list)))
      (project-comphist-add-compile-command selection (projectile-project-root)) ;; puts command first in list
      (projectile-run-compilation selection projectile-compile-use-comint-mode))))


(project-comphist--load-history)

(provide 'project-comphist)
