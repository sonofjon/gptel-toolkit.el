;;; gptel-toolkit.el --- Toolkit and framework for gptel tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Andreas Jonsson
;;
;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/gptel-toolkit
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.9.5"))
;; Keywords: tools, ai, development
;;
;;; Commentary:
;;
;; gptel-toolkit provides 35+ built-in tools and a framework that extends
;; gptel with enhanced tool capabilities.
;;
;; Framework:
;; - Tool definition macro that adds logging, messaging, and error handling
;; - Configurable tool naming with prefix support
;;
;; Built-in Tools (35+ tools across 5 categories):
;; - Buffer operations: list, search, read, edit, batch edits with review
;; - File operations: create files and directories
;; - Emacs integration: documentation lookup, function evaluation, library
;;   loading
;; - Project management: file listing, searching, root detection
;; - Testing support: ERT test execution and listing
;;
;;; Code:

(require 'gptel)

;; Load the framework components
(require 'gptel-toolkit-core)
(require 'gptel-toolkit-tools)

;; Load built-in tools (optional)
;; (require 'gptel-toolkit-tools)

(defgroup gptel-toolkit nil
  "Toolkit and framework for gptel tools."
  :group 'tools
  :prefix "gptel-tk-")

(defun gptel-tk--is-toolkit-tool-p (tool)
  "Return non-nil if TOOL is a gptel-toolkit tool.
A toolkit tool is one whose function name starts with \='gptel-tk-tool-\='."
  (let ((func (gptel-tool-function tool)))
    (and func
         (symbolp func)
         (string-prefix-p "gptel-tk-tool-" (symbol-name func)))))

(defun gptel-tk--filter-toolkit-tools (predicate)
  "Remove toolkit tools from `gptel-tools' based on PREDICATE.
PREDICATE is called with each toolkit tool and should return non-nil
for tools to be removed. Returns the number of tools removed."
  (let ((removed-count 0))
    (setq gptel-tools
          (seq-remove (lambda (tool)
                        (when (and (gptel-tk--is-toolkit-tool-p tool)
                                   (funcall predicate tool))
                          (cl-incf removed-count)
                          t))
                      gptel-tools))
    removed-count))

;;;###autoload
(defun gptel-tk-enable-builtin-tools ()
  "Enable all built-in tools for gptel use.

Populates `gptel-tools' with toolkit tools from `gptel--known-tools',
excluding any tools listed in `gptel-tk-excluded-tools'.

This function affects tools whose function names start with
\='gptel-tk-tool-\='.  This includes the built-in toolkit tools, and any
custom tools that users have created using the same naming
convention. Custom tools created with `gptel-tk-define' using other
names are unaffected."
  (interactive)
  (let* ((all-tools (mapcan (lambda (entry) (mapcar #'cdr (cdr entry))) gptel--known-tools))
         (enabled-tools (seq-remove (lambda (tool)
                                      (member (gptel-tool-name tool) gptel-tk-excluded-tools))
                                    all-tools))
         (excluded-count (- (length all-tools) (length enabled-tools))))
    (setq gptel-tools enabled-tools)
    (if gptel-tk-excluded-tools
        (message "gptel-toolkit: Built-in tools enabled (%d excluded)" excluded-count)
      (message "gptel-toolkit: Built-in tools enabled"))))

;;;###autoload
(defun gptel-tk-disable-builtin-tools ()
  "Disable all built-in tools for gptel use.
Removes all tools whose function names start with \='gptel-tk-tool-\='
from the gptel tools list.  This includes the built-in toolkit tools and
any custom tools that use the same naming convention.  Custom tools
created with `gptel-tk-define' using other names are unaffected."
  (interactive)
  (let ((removed-count (gptel-tk--filter-toolkit-tools (lambda (_tool) t))))
    (message "gptel-toolkit: %d built-in tools disabled" removed-count)))

;;;###autoload
(defun gptel-tk-get-tools ()
  "Return a list of gptel-toolkit tools.
Returns all known toolkit tools minus those in
`gptel-tk-excluded-tools'."
  (seq-filter (lambda (tool)
                (and (gptel-tk--is-toolkit-tool-p tool)
                     (not (member (gptel-tool-name tool) gptel-tk-excluded-tools))))
              (mapcan (lambda (entry) (mapcar #'cdr (cdr entry))) gptel--known-tools)))

;;;###autoload
(defun gptel-tk-get-tool-names ()
  "Return a list of gptel-toolkit tool names.
Returns all known toolkit tool names minus those in
`gptel-tk-excluded-tools'."
  (mapcar #'gptel-tool-name (gptel-tk-get-tools)))

(provide 'gptel-toolkit)

;;; gptel-toolkit.el ends here
