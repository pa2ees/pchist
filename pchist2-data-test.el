;;; pchist2-data-test.el --- Tests for pchist2-data.el -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0

;;; Commentary:

;; ERT (Emacs Lisp Regression Testing) unit tests for pchist2-data.el.
;;
;; Run tests with:
;;   M-x ert RET t RET
;; Or run specific test:
;;   M-x ert RET pchist2-test-add-command RET
;; Or from command line:
;;   emacs -batch -l ert -l pchist2-data.el -l pchist2-data-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'pchist2-data)

;;; Test Fixtures

(defvar pchist2-test--temp-dir nil
  "Temporary directory for test storage files.")

(defvar pchist2-test--original-storage-file nil
  "Original storage file path to restore after tests.")

(defun pchist2-test--setup ()
  "Set up test environment with temporary storage."
  ;; Save original storage file path
  (setq pchist2-test--original-storage-file pchist2-storage-file)
  ;; Create temp directory
  (setq pchist2-test--temp-dir (make-temp-file "pchist2-test-" t))
  ;; Use temp storage file
  (setq pchist2-storage-file (expand-file-name "test-commands.json" pchist2-test--temp-dir))
  ;; Reset state
  (setq pchist2--commands nil)
  (setq pchist2--loaded nil))

(defun pchist2-test--teardown ()
  "Clean up test environment."
  ;; Restore original storage file
  (setq pchist2-storage-file pchist2-test--original-storage-file)
  ;; Clean up temp directory
  (when (and pchist2-test--temp-dir (file-exists-p pchist2-test--temp-dir))
    (delete-directory pchist2-test--temp-dir t))
  ;; Reset state
  (setq pchist2--commands nil)
  (setq pchist2--loaded nil))

(defmacro pchist2-test-with-temp-storage (&rest body)
  "Execute BODY with temporary test storage."
  `(unwind-protect
       (progn
         (pchist2-test--setup)
         ,@body)
     (pchist2-test--teardown)))

;;; Serialization Tests

(ert-deftest pchist2-test-alist-conversion ()
  "Test conversion between symbol-keyed and string-keyed alists."
  (let* ((symbol-alist '((project . "/test/")
                         (command . "make")
                         (switches . ("-j4"))))
         (string-alist '(("project" . "/test/")
                         ("command" . "make")
                         ("switches" . ("-j4"))))
         (converted-to-string (pchist2--alist-to-json-alist symbol-alist))
         (converted-to-symbol (pchist2--json-alist-to-alist string-alist)))
    (should (equal converted-to-string string-alist))
    (should (equal converted-to-symbol symbol-alist))))

;;; Empty Storage Tests

(ert-deftest pchist2-test-empty-initialization ()
  "Test loading from non-existent storage creates empty state."
  (pchist2-test-with-temp-storage
   (pchist2-load)
   (should (null pchist2--commands))
   (should pchist2--loaded)))

(ert-deftest pchist2-test-save-empty-storage ()
  "Test saving empty storage creates valid JSON file."
  (pchist2-test-with-temp-storage
   (pchist2-load)
   (pchist2-save)
   (should (file-exists-p pchist2-storage-file))
   (with-temp-buffer
     (insert-file-contents pchist2-storage-file)
     (let* ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string)
            (data (json-read)))
       (should (listp data))
       (should (null data))))))

;;; Adding Commands Tests

(ert-deftest pchist2-test-add-command ()
  "Test adding a single command."
  (pchist2-test-with-temp-storage
   (let ((cmd (pchist2-add-command
               "/test/project/"
               "./build.sh"
               '("-a" "x86_64")
               '("target1" "target2")
               nil)))
     (should (equal (alist-get 'project cmd) "/test/project/"))
     (should (equal (alist-get 'command cmd) "./build.sh"))
     (should (equal (alist-get 'switches cmd) '("-a" "x86_64")))
     (should (equal (alist-get 'targets cmd) '("target1" "target2")))
     (should (alist-get 'last_used cmd))
     (should (= (length pchist2--commands) 1)))))

(ert-deftest pchist2-test-add-command-with-installer ()
  "Test adding a command with installer information."
  (pchist2-test-with-temp-storage
   (let* ((installer '((command . "scp")
                       (switches . ("-r"))
                       (artifacts . ("build/foo.so" "build/bar.so"))
                       (host . "my_stack")
                       (dest_path . "/root/hwtools/")))
          (cmd (pchist2-add-command
                "/imsar/hwtools/"
                "./build.sh"
                '("-a" "x86_64")
                '("python-bindings")
                (list installer))))
     (should (equal (alist-get 'installers cmd) (list installer)))
     (let ((inst (car (alist-get 'installers cmd))))
       (should (equal (alist-get 'command inst) "scp"))
       (should (equal (alist-get 'host inst) "my_stack"))
       (should (equal (alist-get 'dest_path inst) "/root/hwtools/"))))))

(ert-deftest pchist2-test-add-multiple-commands ()
  "Test adding multiple different commands."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" '("-j4") '("all") nil)
   (pchist2-add-command "/proj1/" "make" '("-j8") '("test") nil)
   (pchist2-add-command "/proj2/" "./build.sh" nil nil nil)
   (should (= (length pchist2--commands) 3))))

;;; Deduplication Tests

(ert-deftest pchist2-test-deduplication-exact-match ()
  "Test that adding an exact duplicate updates last_used instead of duplicating."
  (pchist2-test-with-temp-storage
   (let* ((cmd1 (pchist2-add-command "/test/" "make" '("-j4") '("all") nil))
          (timestamp1 (alist-get 'last_used cmd1)))
     (sleep-for 0.1) ; Ensure different timestamp
     (let* ((cmd2 (pchist2-add-command "/test/" "make" '("-j4") '("all") nil))
            (timestamp2 (alist-get 'last_used cmd2)))
       (should (= (length pchist2--commands) 1))
       (should (string< timestamp1 timestamp2))
       ;; cmd2 should be the same object as cmd1 (updated)
       (should (eq cmd1 cmd2))))))

(ert-deftest pchist2-test-deduplication-different-switches ()
  "Test that commands with different switches are not deduplicated."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/test/" "make" '("-j4") '("all") nil)
   (pchist2-add-command "/test/" "make" '("-j8") '("all") nil)
   (should (= (length pchist2--commands) 2))))

(ert-deftest pchist2-test-deduplication-different-targets ()
  "Test that commands with different targets are not deduplicated."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/test/" "make" '("-j4") '("all") nil)
   (pchist2-add-command "/test/" "make" '("-j4") '("test") nil)
   (should (= (length pchist2--commands) 2))))

(ert-deftest pchist2-test-deduplication-different-project ()
  "Test that same command in different projects are not deduplicated."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" '("-j4") '("all") nil)
   (pchist2-add-command "/proj2/" "make" '("-j4") '("all") nil)
   (should (= (length pchist2--commands) 2))))

;;; Query Tests

(ert-deftest pchist2-test-get-commands-all ()
  "Test getting all commands across all projects."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" nil nil nil)
   (pchist2-add-command "/proj2/" "./build.sh" nil nil nil)
   (let ((all-cmds (pchist2-get-commands)))
     (should (= (length all-cmds) 2)))))

(ert-deftest pchist2-test-get-commands-by-project ()
  "Test getting commands filtered by project."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" nil nil nil)
   (pchist2-add-command "/proj1/" "./test.sh" nil nil nil)
   (pchist2-add-command "/proj2/" "./build.sh" nil nil nil)
   (let ((proj1-cmds (pchist2-get-commands "/proj1/")))
     (should (= (length proj1-cmds) 2))
     (should (cl-every (lambda (c) (equal (alist-get 'project c) "/proj1/"))
                       proj1-cmds)))))

(ert-deftest pchist2-test-get-unique-commands ()
  "Test getting unique command names for a project."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" '("-j4") nil nil)
   (pchist2-add-command "/proj1/" "make" '("-j8") nil nil)
   (pchist2-add-command "/proj1/" "./test.sh" nil nil nil)
   (let ((cmds (pchist2-get-unique-commands "/proj1/")))
     (should (= (length cmds) 2))
     (should (member "make" cmds))
     (should (member "./test.sh" cmds)))))

(ert-deftest pchist2-test-get-unique-switches ()
  "Test getting unique switches for a command."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" '("-j4" "--verbose") nil nil)
   (pchist2-add-command "/proj1/" "make" '("-j8") nil nil)
   (pchist2-add-command "/proj1/" "make" '("-j4") nil nil) ; Duplicate
   (let ((switches (pchist2-get-unique-switches "/proj1/" "make")))
     (should (member "-j4" switches))
     (should (member "-j8" switches))
     (should (member "--verbose" switches))
     ;; Should not have duplicates
     (should (= (length switches) 3)))))

(ert-deftest pchist2-test-get-unique-targets ()
  "Test getting unique targets for a command."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" nil '("all" "test") nil)
   (pchist2-add-command "/proj1/" "make" nil '("clean" "all") nil)
   (let ((targets (pchist2-get-unique-targets "/proj1/" "make")))
     (should (member "all" targets))
     (should (member "test" targets))
     (should (member "clean" targets))
     (should (= (length targets) 3)))))

(ert-deftest pchist2-test-get-unique-projects ()
  "Test getting unique project paths."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" nil nil nil)
   (pchist2-add-command "/proj2/" "make" nil nil nil)
   (pchist2-add-command "/proj1/" "./test.sh" nil nil nil)
   (let ((projects (pchist2-get-unique-projects)))
     (should (= (length projects) 2))
     (should (member "/proj1/" projects))
     (should (member "/proj2/" projects)))))

;;; Update Tests

(ert-deftest pchist2-test-update-command ()
  "Test updating an existing command."
  (pchist2-test-with-temp-storage
   (let* ((cmd (pchist2-add-command "/test/" "make" '("-j4") '("all") nil))
          (old-timestamp (alist-get 'last_used cmd)))
     (sleep-for 0.1)
     (pchist2-update-command cmd '((switches . ("-j8" "--verbose"))))
     (should (equal (alist-get 'switches cmd) '("-j8" "--verbose")))
     (should (string< old-timestamp (alist-get 'last_used cmd)))
     (should (= (length pchist2--commands) 1)))))

(ert-deftest pchist2-test-update-nonexistent-command ()
  "Test that updating a non-existent command signals an error."
  (pchist2-test-with-temp-storage
   (let ((fake-cmd '((project . "/fake/") (command . "fake"))))
     (should-error (pchist2-update-command fake-cmd '((switches . ("-x"))))))))

;;; Delete Tests

(ert-deftest pchist2-test-delete-command ()
  "Test deleting a command."
  (pchist2-test-with-temp-storage
   (let ((cmd1 (pchist2-add-command "/test/" "make" nil nil nil)))
     (pchist2-add-command "/test/" "./build.sh" nil nil nil)
     (should (= (length pchist2--commands) 2))
     (pchist2-delete-command cmd1)
     (should (= (length pchist2--commands) 1))
     (should (equal (alist-get 'command (car pchist2--commands)) "./build.sh")))))

;;; File I/O Tests

(ert-deftest pchist2-test-save-and-load-roundtrip ()
  "Test that data survives save/load cycle."
  (pchist2-test-with-temp-storage
   (let* ((installer '((command . "scp")
                       (switches . ("-r"))
                       (artifacts . ("build/foo.so"))
                       (host . "my_stack")
                       (dest_path . "/root/")))
          (_cmd (pchist2-add-command
                 "/proj1/"
                 "./build.sh"
                 '("-a" "x86_64" "-c release")
                 '("target1" "target2")
                 (list installer))))
     ;; Reset in-memory state
     (setq pchist2--commands nil)
     (setq pchist2--loaded nil)
     ;; Load back
     (pchist2-load)
     (should (= (length pchist2--commands) 1))
     (let ((loaded-cmd (car pchist2--commands)))
       (should (equal (alist-get 'project loaded-cmd) "/proj1/"))
       (should (equal (alist-get 'command loaded-cmd) "./build.sh"))
       (should (equal (alist-get 'switches loaded-cmd) '("-a" "x86_64" "-c release")))
       (should (equal (alist-get 'targets loaded-cmd) '("target1" "target2")))
       (should (equal (alist-get 'installers loaded-cmd) (list installer)))))))

(ert-deftest pchist2-test-multiple-projects-persistence ()
  "Test saving and loading commands from multiple projects."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" '("-j4") '("all") nil)
   (pchist2-add-command "/proj2/" "./build.sh" nil '("debug") nil)
   (pchist2-add-command "/proj3/" "cmake" '("-G" "Ninja") nil nil)
   ;; Reset and reload
   (setq pchist2--commands nil)
   (setq pchist2--loaded nil)
   (pchist2-load)
   (should (= (length pchist2--commands) 3))
   (should (= (length (pchist2-get-commands "/proj1/")) 1))
   (should (= (length (pchist2-get-commands "/proj2/")) 1))
   (should (= (length (pchist2-get-commands "/proj3/")) 1))))

;;; File Locking Tests

(ert-deftest pchist2-test-lock-acquisition ()
  "Test that lock can be acquired and released."
  (pchist2-test-with-temp-storage
   (should (pchist2--acquire-lock))
   (pchist2--release-lock)
   ;; Should be able to acquire again
   (should (pchist2--acquire-lock))
   (pchist2--release-lock)))

(ert-deftest pchist2-test-lock-prevents-concurrent-access ()
  "Test that holding a lock prevents another acquisition."
  (pchist2-test-with-temp-storage
   (should (pchist2--acquire-lock))
   ;; Temporarily reduce timeout for faster test
   (let ((pchist2-lock-timeout 0.5))
     (should-not (pchist2--acquire-lock)))
   (pchist2--release-lock)))

;;; Installer Query Tests

(ert-deftest pchist2-test-get-installer-commands ()
  "Test getting unique installer command names."
  (pchist2-test-with-temp-storage
   (let* ((scp-installer '((command . "scp")
                           (switches . ())
                           (artifacts . ("a.so"))
                           (host . "host1")
                           (dest_path . "/tmp/")))
          (rsync-installer '((command . "rsync")
                             (switches . ("-av"))
                             (artifacts . ("b.so"))
                             (host . "host2")
                             (dest_path . "/tmp/"))))
     (pchist2-add-command "/proj1/" "make" nil nil (list scp-installer))
     (pchist2-add-command "/proj1/" "make" nil nil (list rsync-installer))
     (let ((installer-cmds (pchist2-get-installer-commands "/proj1/" "make")))
       (should (= (length installer-cmds) 2))
       (should (member "scp" installer-cmds))
       (should (member "rsync" installer-cmds))))))

(ert-deftest pchist2-test-get-installer-hosts ()
  "Test getting unique installer hosts."
  (pchist2-test-with-temp-storage
   (let* ((inst1 '((command . "scp")
                   (switches . ())
                   (artifacts . ("a.so"))
                   (host . "host1")
                   (dest_path . "/tmp/")))
          (inst2 '((command . "scp")
                   (switches . ())
                   (artifacts . ("b.so"))
                   (host . "host2")
                   (dest_path . "/tmp/")))
          (inst3 '((command . "scp")
                   (switches . ())
                   (artifacts . ("c.so"))
                   (host . "host1")
                   (dest_path . "/tmp/"))))
     (pchist2-add-command "/proj1/" "make" nil nil (list inst1))
     (pchist2-add-command "/proj1/" "./build.sh" nil nil (list inst2 inst3))
     (let ((hosts (pchist2-get-installer-hosts "/proj1/")))
       (should (= (length hosts) 2))
       (should (member "host1" hosts))
       (should (member "host2" hosts))))))

;;; Edge Cases

(ert-deftest pchist2-test-command-with-spaces-in-switches ()
  "Test that switches containing spaces are handled correctly."
  (pchist2-test-with-temp-storage
   (let ((cmd (pchist2-add-command
               "/test/"
               "./build.sh"
               '("-a x86_64" "-c debug" "--flag")
               nil
               nil)))
     (should (equal (alist-get 'switches cmd) '("-a x86_64" "-c debug" "--flag")))
     ;; Save and reload
     (setq pchist2--commands nil)
     (setq pchist2--loaded nil)
     (pchist2-load)
     (let ((loaded-cmd (car pchist2--commands)))
       (should (equal (alist-get 'switches loaded-cmd) '("-a x86_64" "-c debug" "--flag")))))))

(ert-deftest pchist2-test-empty-lists ()
  "Test handling of empty lists for switches, targets, installers."
  (pchist2-test-with-temp-storage
   (let ((cmd (pchist2-add-command "/test/" "make" nil nil nil)))
     (should (null (alist-get 'switches cmd)))
     (should (null (alist-get 'targets cmd)))
     (should (null (alist-get 'installers cmd)))
     ;; Save and reload
     (setq pchist2--commands nil)
     (setq pchist2--loaded nil)
     (pchist2-load)
     (let ((loaded-cmd (car pchist2--commands)))
       (should (null (alist-get 'switches loaded-cmd)))
       (should (null (alist-get 'targets loaded-cmd)))
       (should (null (alist-get 'installers loaded-cmd)))))))

(ert-deftest pchist2-test-nil-host-in-installer ()
  "Test that installers with nil host are handled correctly."
  (pchist2-test-with-temp-storage
   (let* ((installer '((command . "cp")
                       (switches . ())
                       (artifacts . ("file.txt"))
                       (host . nil)
                       (dest_path . "/tmp/")))
          (cmd (pchist2-add-command "/test/" "make" nil nil (list installer))))
     (should (null (alist-get 'host (car (alist-get 'installers cmd)))))
     ;; Save and reload
     (setq pchist2--commands nil)
     (setq pchist2--loaded nil)
     (pchist2-load)
     (let* ((loaded-cmd (car pchist2--commands))
            (loaded-inst (car (alist-get 'installers loaded-cmd))))
       (should (null (alist-get 'host loaded-inst)))))))

;;; Clear All Test

(ert-deftest pchist2-test-clear-all ()
  "Test clearing all commands."
  (pchist2-test-with-temp-storage
   (pchist2-add-command "/proj1/" "make" nil nil nil)
   (pchist2-add-command "/proj2/" "./build.sh" nil nil nil)
   (should (= (length pchist2--commands) 2))
   ;; Clear (non-interactively by calling directly after confirming)
   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
     (pchist2-clear-all))
   (should (= (length pchist2--commands) 0))
   ;; Verify persistence
   (setq pchist2--loaded nil)
   (pchist2-load)
   (should (= (length pchist2--commands) 0))))

;;; Test Summary

(defun pchist2-run-all-tests ()
  "Run all pchist2-data tests and display results."
  (interactive)
  (ert-run-tests-interactively "^pchist2-test-"))

(provide 'pchist2-data-test)
;;; pchist2-data-test.el ends here
