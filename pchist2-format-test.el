;;; pchist2-format-test.el --- Tests for pchist2 formatting layer -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0

;;; Commentary:

;; Unit tests for pchist2 formatting functions.
;; These tests run in batch mode without requiring Helm or user interaction.

;;; Code:

(require 'ert)
(require 'pchist2-data)
(require 'pchist2-format)

;;; Module Loading Tests

(ert-deftest pchist2-ui-test-format-module-loads ()
  "Test that format module loads without errors."
  (should (featurep 'pchist2-format)))

;;; pchist2-format.el Command Formatting Tests

(ert-deftest pchist2-ui-test-format-command-basic ()
  "Test basic command formatting."
  (let ((cmd '((project . "/test/project/")
               (command . "./build.sh")
               (switches . ("-a" "x86_64"))
               (targets . ("target1"))
               (installers . nil)
               (last_used . "2024-01-01T12:00:00.000000"))))
    (should (string-match-p "\\[.*\\].*build\\.sh.*-a.*x86_64.*target1"
                            (pchist2-format-command cmd nil)))))

(ert-deftest pchist2-ui-test-format-command-short-paths ()
  "Test command formatting with short paths."
  (let ((cmd '((project . "/test/project/")
               (command . "./build.sh")
               (switches . nil)
               (targets . nil)
               (installers . nil)
               (last_used . "2024-01-01T12:00:00.000000"))))
    (let ((formatted (pchist2-format-command cmd t)))
      ;; Should show just "project" not full path
      (should (string-match-p "\\[project\\]" formatted))
      (should-not (string-match-p "/test/project/" formatted)))))

(ert-deftest pchist2-ui-test-format-command-with-installers ()
  "Test command formatting with installers including artifacts."
  (let ((cmd '((project . "/test/project/")
               (command . "./build.sh")
               (switches . nil)
               (targets . nil)
               (installers . (((command . "scp")
                               (switches . ("-r"))
                               (artifacts . ("build/foo.so"))
                               (host . "my_host")
                               (dest_path . "/remote/path/"))))
               (last_used . "2024-01-01T12:00:00.000000"))))
    (let ((formatted (pchist2-format-command cmd nil)))
      (should (string-match-p "&&.*scp.*build/foo\\.so.*my_host:/remote/path/" formatted)))))

(ert-deftest pchist2-ui-test-format-command-with-local-installer ()
  "Test command formatting with local installer (no host)."
  (let ((cmd '((project . "/test/project/")
               (command . "./build.sh")
               (switches . nil)
               (targets . nil)
               (installers . (((command . "cp")
                               (switches . nil)
                               (artifacts . ("build/foo.so"))
                               (host . nil)
                               (dest_path . "/local/path/"))))
               (last_used . "2024-01-01T12:00:00.000000"))))
    (let ((formatted (pchist2-format-command cmd nil)))
      (should (string-match-p "&&.*cp.*/local/path/" formatted))
      ;; Should not have a colon since no host
      (should-not (string-match-p ":" formatted)))))

(ert-deftest pchist2-ui-test-format-command-empty-fields ()
  "Test command formatting with empty optional fields."
  (let ((cmd '((project . "/test/project/")
               (command . "./build.sh")
               (switches . nil)
               (targets . nil)
               (installers . nil)
               (last_used . "2024-01-01T12:00:00.000000"))))
    (let ((formatted (pchist2-format-command cmd nil)))
      (should (string-match-p "\\[/test/project/\\].*build\\.sh" formatted)))))

(ert-deftest pchist2-ui-test-get-filter-description ()
  "Test filter description generation."
  ;; Test current-project mode
  (should (string-match-p "Current Project:"
                          (pchist2-format-filter-description 'current-project nil "/test/project/")))

  ;; Test global mode
  (should (string= "All Projects"
                   (pchist2-format-filter-description 'global nil nil)))

  ;; Test specific-project mode
  (should (string-match-p "Specific Project:.*test/project"
                          (pchist2-format-filter-description 'specific-project "/test/project/" nil))))

;;; pchist2-format.el Builder State Formatting Tests

(ert-deftest pchist2-builder-test-format-current-state-empty ()
  "Test builder state formatting with empty state."
  (should (string= "<<command>>" (pchist2-format-builder-state nil nil nil nil))))

(ert-deftest pchist2-builder-test-format-current-state-command-only ()
  "Test builder state formatting with command only."
  (should (string= "./build.sh" (pchist2-format-builder-state "./build.sh" nil nil nil))))

(ert-deftest pchist2-builder-test-format-current-state-with-switches ()
  "Test builder state formatting with switches."
  (should (string= "./build.sh -a x86_64"
                   (pchist2-format-builder-state "./build.sh" '("-a" "x86_64") nil nil))))

(ert-deftest pchist2-builder-test-format-current-state-with-targets ()
  "Test builder state formatting with targets."
  (should (string= "./build.sh -a x86_64 target1 target2"
                   (pchist2-format-builder-state "./build.sh" '("-a" "x86_64") '("target1" "target2") nil))))

(ert-deftest pchist2-builder-test-format-current-state-with-installers ()
  "Test builder state formatting with installers including artifacts."
  (let ((formatted (pchist2-format-builder-state "./build.sh" nil nil
                                                 '(((command . "scp")
                                                    (artifacts . ("build/foo.so"))
                                                    (host . "my_host")
                                                    (dest_path . "/remote/path/"))))))
    (should (string-match-p "./build.sh.*&&.*scp.*build/foo\\.so.*my_host:/remote/path/" formatted))))

(ert-deftest pchist2-builder-test-format-current-state-multiple-installers ()
  "Test builder state formatting with multiple installers."
  (let ((formatted (pchist2-format-builder-state "./build.sh" nil nil
                                                 '(((command . "scp")
                                                    (host . "host1")
                                                    (dest_path . "/path1/"))
                                                   ((command . "rsync")
                                                    (host . "host2")
                                                    (dest_path . "/path2/"))))))
    (should (string-match-p "&&.*scp.*host1:/path1/" formatted))
    (should (string-match-p "&&.*rsync.*host2:/path2/" formatted))))

(ert-deftest pchist2-builder-test-format-current-state-local-installer ()
  "Test builder state formatting with local installer (no host)."
  (let ((formatted (pchist2-format-builder-state "./build.sh" nil nil
                                                 '(((command . "cp")
                                                    (host . nil)
                                                    (dest_path . "/local/path/"))))))
    (should (string-match-p "&&.*cp.*/local/path/" formatted))
    ;; Should not have a colon for local
    (should-not (string-match-p ":/" formatted))))

(ert-deftest pchist2-builder-test-format-current-state-complete ()
  "Test builder state formatting with all fields populated."
  (let ((formatted (pchist2-format-builder-state "./build.sh"
                                                 '("-a" "x86_64" "-c" "release")
                                                 '("target1" "target2")
                                                 '(((command . "scp")
                                                    (host . "my_host")
                                                    (dest_path . "/remote/path/"))))))
    (should (string-match-p "./build.sh" formatted))
    (should (string-match-p "-a" formatted))
    (should (string-match-p "x86_64" formatted))
    (should (string-match-p "-c" formatted))
    (should (string-match-p "release" formatted))
    (should (string-match-p "target1" formatted))
    (should (string-match-p "target2" formatted))
    (should (string-match-p "&&.*scp.*my_host:/remote/path/" formatted))))

(ert-deftest pchist2-ui-test-format-command-artifacts-order ()
  "Test that artifacts appear between command and destination.
Regression test for bug where artifacts were omitted."
  (let ((cmd '((project . "/home/erik/projects/pchist/")
               (command . "echo")
               (switches . nil)
               (targets . ("hello > /tmp/hellohello.txt"))
               (installers . (((command . "cp")
                               (switches . nil)
                               (artifacts . ("/tmp/hellohello.txt"))
                               (host . nil)
                               (dest_path . "/tmp/plop.txt"))))
               (last_used . "2026-08-27T11:13:36.410066"))))
    (let ((formatted (pchist2-format-command cmd nil)))
      ;; Should be: echo hello > /tmp/hellohello.txt && cp /tmp/hellohello.txt /tmp/plop.txt
      (should (string-match-p "echo.*hello.*&&.*cp.*/tmp/hellohello\\.txt.*/tmp/plop\\.txt" formatted))
      ;; Should NOT have installer before main command
      (should-not (string-match-p "^\\[.*\\].*cp.*echo" formatted)))))

;;; Command Execution String Tests

(ert-deftest pchist2-ui-test-format-command-for-execution-no-prefix ()
  "Test that execution format does not include project prefix."
  (let ((cmd '((project . "/home/erik/projects/pchist/")
               (command . "echo")
               (switches . nil)
               (targets . ("hello > /tmp/hellohello.txt"))
               (installers . (((command . "cp")
                               (switches . nil)
                               (artifacts . ("/tmp/hellohello.txt"))
                               (host . nil)
                               (dest_path . "/tmp/plop.txt"))))
               (last_used . "2026-08-27T11:13:36.410066"))))
    (let ((exec-string (pchist2-format-command-for-execution cmd)))
      ;; Should NOT have project prefix
      (should-not (string-match-p "^\\[.*\\]" exec-string))
      ;; Should have the actual command
      (should (string-match-p "^echo" exec-string))
      ;; Should be executable
      (should (string= "echo hello > /tmp/hellohello.txt && cp /tmp/hellohello.txt /tmp/plop.txt"
                       exec-string)))))

(ert-deftest pchist2-ui-test-format-command-display-has-prefix ()
  "Test that display format DOES include project prefix."
  (let ((cmd '((project . "/home/erik/projects/pchist/")
               (command . "echo")
               (switches . nil)
               (targets . nil)
               (installers . nil)
               (last_used . "2026-08-27T11:13:36.410066"))))
    (let ((display-string (pchist2-format-command cmd nil)))
      ;; Should have project prefix for display
      (should (string-match-p "^\\[.*\\]" display-string)))))

;;; Integration Tests with Data Layer

(ert-deftest pchist2-format-test-command-from-data-layer ()
  "Test formatting commands retrieved from data layer."
  (let ((pchist2--commands nil)
        (pchist2--loaded t)
        (pchist2-storage-file "/tmp/pchist2-test-commands.json"))
    ;; Add a test command (will save to test file)
    (pchist2-add-command "/test/project/"
                         "./build.sh"
                         '("-a" "x86_64")
                         '("test-target")
                         nil)
    ;; Get and format it
    (let* ((cmds (pchist2-get-commands "/test/project/"))
           (cmd (car cmds))
           (formatted (pchist2-format-command cmd nil)))
      (should (string-match-p "\\[/test/project/\\].*build\\.sh.*-a.*x86_64.*test-target" formatted)))
    ;; Cleanup
    (setq pchist2--commands nil)
    (when (file-exists-p "/tmp/pchist2-test-commands.json")
      (delete-file "/tmp/pchist2-test-commands.json"))
    (when (file-exists-p "/tmp/pchist2-test-commands.json.lock")
      (delete-directory "/tmp/pchist2-test-commands.json.lock" t))))

(provide 'pchist2-format-test)
;;; pchist2-format-test.el ends here
