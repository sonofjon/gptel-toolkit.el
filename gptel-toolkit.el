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

;; Load built-in tools (optional)
;; (require 'gptel-toolkit-tools)

(defgroup gptel-toolkit nil
  "Toolkit and framework for gptel tools."
  :group 'tools
  :prefix "gptel-tk-")

(defcustom gptel-tk-auto-load-builtin-tools nil
  "Whether to automatically load built-in tools.
When non-nil, the built-in tools will be loaded and registered with gptel."
  :type 'boolean
  :group 'gptel-toolkit)

;;;###autoload
(defun gptel-toolkit-enable-builtin-tools ()
  "Enable and register all built-in tools with gptel."
  (interactive)
  (require 'gptel-toolkit-tools)
  (message "gptel-toolkit: Built-in tools enabled"))

;;;###autoload
(defun gptel-toolkit-disable-builtin-tools ()
  "Disable and unregister all built-in tools from gptel."
  (interactive)
  (when (featurep 'gptel-toolkit-tools)
    ;; Implementation for tool removal will be added
    (message "gptel-toolkit: Built-in tools disabled")))

;; Auto-load built-in tools if configured
(when gptel-tk-auto-load-builtin-tools
  (require 'gptel-toolkit-tools))

(provide 'gptel-toolkit)

;;; gptel-toolkit.el ends here
