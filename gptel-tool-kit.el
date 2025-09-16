;;; gptel-tool-kit.el --- Development framework for gptel tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andreas Jonsson

;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/gptel-tool-kit
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.8.0"))
;; Keywords: tools, ai, development, gptel

;;; Commentary:
;;
;; gptel-tool-kit provides a comprehensive framework for building robust
;; tools that integrate with the gptel package. It includes advanced tool
;; macros, error handling, logging, and development utilities.
;;
;; Features:
;; - Advanced tool definition macros with automatic logging
;; - Comprehensive error handling and reporting
;; - Built-in tool templates and examples
;; - Development utilities for testing and debugging
;;
;;; Code:

(require 'gptel)

;; Load the framework components
(require 'gptel-tool-framework)
(require 'gptel-tool-utils)

;; Load built-in tools (optional)
;; (require 'gptel-builtin-tools)

;; Load templates and examples
;; (require 'gptel-tool-templates)

(defgroup gptel-tool-kit nil
  "Development framework for gptel tools."
  :group 'tools
  :prefix "gptel-tk-")

(defcustom gptel-tk-auto-load-builtin-tools nil
  "Whether to automatically load built-in tools.
When non-nil, the built-in tools will be loaded and registered with gptel."
  :type 'boolean
  :group 'gptel-tool-kit)

;;;###autoload
(defun gptel-tool-kit-enable-builtin-tools ()
  "Enable and register all built-in tools with gptel."
  (interactive)
  (require 'gptel-builtin-tools)
  (message "gptel-tool-kit: Built-in tools enabled"))

;;;###autoload
(defun gptel-tool-kit-disable-builtin-tools ()
  "Disable and unregister all built-in tools from gptel."
  (interactive)
  (when (featurep 'gptel-builtin-tools)
    ;; Implementation for tool removal will be added
    (message "gptel-tool-kit: Built-in tools disabled")))

;; Auto-load built-in tools if configured
(when gptel-tk-auto-load-builtin-tools
  (require 'gptel-builtin-tools))

(provide 'gptel-tool-kit)

;;; gptel-tool-kit.el ends here
