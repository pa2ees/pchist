;;; pchist2-ui-completion.el --- Completion wrapper for pchist v2 -*- lexical-binding: t; -*-

;; Author: Erik van Zwol
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides a simple wrapper around completing-read that works
;; with any completion framework (Helm, Ivy, Vertico, Ido, or default).
;;
;; By using completing-read instead of framework-specific functions, we:
;; - Remove hard dependencies on completion frameworks
;; - Let users choose their preferred completion UI
;; - Keep completion in the minibuffer (no jarring screen takeovers)

;;; Code:

(require 'cl-lib)

(defun pchist2-complete (prompt candidates &optional initial-input)
  "Prompt user with PROMPT to complete from CANDIDATES.
INITIAL-INPUT is the default/initial value.
Uses completing-read so it works with any completion framework."
  (let* ((collection (if (listp candidates)
                        candidates
                      (list candidates)))
         (default (when (and initial-input (not (string-empty-p initial-input)))
                   initial-input))
         (prompt-with-default (if default
                                 (format "%s[%s] " prompt default)
                               prompt)))
    (completing-read prompt-with-default
                    collection
                    nil        ; predicate
                    nil        ; require-match (allow free input)
                    nil        ; initial-input (use default instead)
                    nil        ; hist
                    default))) ; default

(provide 'pchist2-ui-completion)
;;; pchist2-ui-completion.el ends here
