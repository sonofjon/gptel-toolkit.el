;;; gptel-toolkit.el --- Development framework for gptel tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Andreas Jonsson

;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/gptel-toolkit
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.8.0"))
;; Keywords: tools, ai, development, gptel

;;; Commentary:
;;
;; gptel-toolkit provides a comprehensive framework for building robust
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
(require 'gptel-toolkit-core)
(require 'gptel-toolkit-utils)

;; Load built-in tools (optional)
;; (require 'gptel-toolkit-tools)

;; Load templates and examples
;; (require 'gptel-toolkit-templates)

(defgroup gptel-toolkit nil
  "Development framework for gptel tools."
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
