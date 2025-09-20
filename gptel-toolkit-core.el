;;; gptel-toolkit-core.el --- Core framework for gptel-toolkit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Andreas Jonsson
;;
;;; Commentary:
;;
;; This package provides the core framework that extends gptel with enhanced
;; tool capabilities.
;;
;; Features:
;; - Tool definition macro (gptel-tk-define) that provides:
;;   - Logging with optional suppression
;;   - Mini-buffer messaging with filtering and truncation
;;   - Error handling (tools return strings or re-signal errors)
;;   - JSON compatibility with :json-false to nil normalization
;; - Configurable tool naming with prefix support
;;
;; The framework handles boilerplate around tool definition, providing
;; consistent logging, messaging, and error handling across all tools.
;;
;;; Code:

;;; Configuration Variables

(defcustom gptel-tk-log-buffer "*gptel-tool-log*"
  "Name of the buffer used for tool logging."
  :type 'string
  :group 'gptel-toolkit)

(defcustom gptel-tk-suppress-logging nil
  "When non-nil, suppress all tool logging."
  :type 'boolean
  :group 'gptel-toolkit)

(defcustom gptel-tk-catch-errors t
  "When non-nil, catch tool errors and return as strings instead of signaling."
  :type 'boolean
  :group 'gptel-toolkit)

(defcustom gptel-tk-tool-prefix ""
  "Prefix to add to all tool registration names.
If empty, tools are registered with clean names like 'buffer_search'.
If set to 'tk_', tools become 'tk_buffer_search', etc.  Useful to avoid
conflicts with other tool packages."
  :type 'string
  :group 'gptel-toolkit)

;;; Utility Macros

(defmacro gptel-tk--with-suppressed-messages (&rest body)
  "Execute BODY while suppressing all messages.
This macro temporarily sets the variable `inhibit-message' to t and
`message-log-max' to nil, preventing messages from appearing in the echo
area or being logged to the *Messages* buffer."
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(defmacro gptel-tk--with-normalized-bools (vars &rest body)
  "Re-bind VARS to normalized boolean values (t or nil) within BODY.
Handles `:json-false` and `:false`."
  (let ((bindings (mapcar (lambda (var)
                            `(,var (let ((val ,var))
                                     (if (memq val '(:json-false :false))
                                         nil
                                       val))))
                          vars)))
    `(let ,bindings
       ,@body)))

(defmacro gptel-tk--with-tool (tool-name args &rest body)
  "Run BODY for TOOL-NAME and message/log the action.

TOOL-NAME is display name of the tool.  ARGS is a property list of
keyword/value pairs describing the parameters passed to the tool
function.

If ARGS is nil the minibuffer will show only the tool name (no argument
summary is displayed).

The macro binds local variables `tool-name' and `args' and then:

- Messages the running tool name and a display-safe summary of filtered
  ARGS in the minibuffer (using `gptel-tk--truncate-for-display').
- Executes BODY and on success logs the full ARGS and the RESULT to the
  `*gptel-tool-log*' buffer and returns RESULT.
- On error it delegates to `gptel-tk--report-and-return-or-signal',
  which messages/logs and returns or re-signals depending on
  `gptel-tk-catch-errors'."
  `(let* ((tool-name ,tool-name)
          (args ,args)
          (display-args (gptel-tk--filter-display-args args)))
     (message "%s%s" tool-name
              (if display-args
                  (concat " " (prin1-to-string
                               (gptel-tk--truncate-for-display display-args)))
                ""))
     (condition-case err
         (let ((result (progn ,@body)))
           (gptel-tk--log-tool-call tool-name args result)
           result)
       (error (gptel-tk--report-and-return-or-signal tool-name args err)))))

;;; Main Framework Macro

;;;###autoload
(defmacro gptel-tk-define (name args docstring &rest body)
  "Define a gptel tool with automatic logging and error handling.

NAME is the function name, ARGS are the function arguments,
DOCSTRING is the tool documentation, and BODY contains the tool implementation.

Automatically normalizes any :json-false values to nil in the tool body."
  (let* ((tool-name (symbol-name name))
         (all-args (seq-remove (lambda (arg) (string-prefix-p "&" (symbol-name arg))) args))
         (raw-args-pairs (apply 'append
                               (mapcar (lambda (arg)
                                        `(,(intern (concat ":" (symbol-name arg))) ,arg))
                                      all-args))))
    `(defun ,name ,args
       ,docstring
       (let ((raw-args (list ,@raw-args-pairs)))
         (gptel-tk--with-normalized-bools ,all-args
           (gptel-tk--with-tool ,tool-name raw-args
             ,@body))))))

;;; Helper Functions

(defun gptel-tk--make-tool-name (base-name)
  "Create a tool registration name with appropriate prefix.
BASE-NAME should be the clean tool name (e.g., 'buffer_search').
Returns the name with gptel-tk-tool-prefix applied."
  (concat gptel-tk-tool-prefix base-name))

;;; Filtering and Truncation Functions

(defun gptel-tk--truncate-for-display (obj)
  "Return a truncated, display-safe copy of OBJ for minibuffer messages.

OBJ is the source object to convert; it may be nil, a string, a list, a
vector, or a list or vector of property lists (plists).  The conversion
rules are:

- nil: returned as nil.
- string: if the string contains a newline, return only the text up to
  the first newline followed by the suffix \"...(+N more)\" where N is
  the number of remaining lines; otherwise return the original string.
- list: recursively process each element and return a new list.
- list of plists: return a list whose first element is the processed
  first plist and, if there are more elements, a second element that is
  the string \"...(+N more)\" where N is the number of remaining plists.
- vector: recursively process each element and return a new vector.
- vector of plists: see \"list of plists\" above; the same truncation is
  applied, but the result is a vector, not a list.

The original OBJ is not mutated; the result is a fresh structure intended
for use with `prin1-to-string' for concise minibuffer display."
  (cond
   ((null obj) nil)
   ;; Strings
   ((stringp obj)
    (let ((lines (split-string obj "\n")))
      (if (> (length lines) 1)
          (concat (car lines) (format "...(+%d more)" (1- (length lines))))
        obj)))
   ;; Lists of plists
   ((and (listp obj)
         ;; Each element is a list whose car is a keyword
         (cl-every (lambda (e) (and (listp e) (keywordp (car e)))) obj))
    (let ((len (length obj)))
      (if (= len 0)
          '()
        (let ((first (gptel-tk--truncate-for-display (car obj)))
              (rest-count (1- len)))
          (if (> rest-count 0)
              (list first (format "...(+%d more)" rest-count))
            (list first))))))
   ;; Lists
   ((listp obj)
    (mapcar #'gptel-tk--truncate-for-display obj))
   ;; Vectors of plists
   ((and (vectorp obj)
         ;; Each element is a list whose car is a keyword
         (cl-every (lambda (e) (and (listp e) (keywordp (car e)))) obj))
    (let ((len (length obj)))
      (if (= len 0)
          (vector)
        (let ((first (gptel-tk--truncate-for-display (aref obj 0)))
              (rest-count (1- len)))
          (if (> rest-count 0)
              (vector first (format "...(+%d more)" rest-count))
            (vector first))))))
   ;; Vectors
   ((vectorp obj)
    (let* ((len (length obj))
           (out (make-vector len nil)))
      (dotimes (i len)
        (aset out i (gptel-tk--truncate-for-display (aref obj i))))
      out))
   (t obj)))

(defun gptel-tk--filter-display-args (args)
  "Filter ARGS property list for display in minibuffer messages.

Remove keyword-value pairs where the value is `:json-false' (indicating
no value was provided by the caller), but preserve pairs where the value
is explicitly nil (indicating the caller explicitly set it to nil).

ARGS is a property list of keyword-value pairs.

Returns a new property list with only the desired pairs for display."
  (when args
    (let (result)
      (while args
        (let ((keyword (pop args))
              (value (pop args)))
          (unless (eq value :json-false)
            (push keyword result)
            (push value result))))
      (nreverse result))))

;;; Logging Functions

(defun gptel-tk--log-tool-call (tool-name args result)
  "Log a tool call to the tool log buffer.
Appends to `gptel-tk-log-buffer', recording TOOL-NAME, ARGS and RESULT.
Does nothing if `gptel-tk-suppress-logging' is non-nil."
  (unless gptel-tk-suppress-logging
    (let ((buf (get-buffer-create gptel-tk-log-buffer))
          (ts (format-time-string "%Y-%m-%d %T")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "%s | %s | args=%s | result=%s\n"
                        ts
                        tool-name
                        (prin1-to-string args)
                        (prin1-to-string (or result "<nil>"))))
        (force-window-update (get-buffer-window buf))))))

(defun gptel-tk--log-tool-error (tool-name args error-message)
"Log a tool error to the tool log buffer.
Appends to `gptel-tk-log-buffer', recording TOOL-NAME, ARGS and
ERROR-MESSAGE.  Does nothing if `gptel-tk-suppress-logging' is non-nil."
  (unless gptel-tk-suppress-logging
    (let ((buf (get-buffer-create gptel-tk-log-buffer))
          (ts (format-time-string "%Y-%m-%d %T")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "%s | %s | args=%s | error=%s\n"
                        ts
                        tool-name
                        (prin1-to-string args)
                        (prin1-to-string error-message)))
        (force-window-update (get-buffer-window buf))))))

;;; Error Handling

(defun gptel-tk--report-and-return-or-signal (tool-name args err)
  "Report ERR for TOOL-NAME with ARGS, then return or re-signal.

This builds the exact minibuffer message string for ERR, messages it,
and logs it.  If `gptel-tk-catch-errors' is non-nil, it returns that
string; otherwise it re-signals the original error."
  (let ((msg (format "%s: Error: %s" tool-name (error-message-string err))))
    (message "%s" msg)
    (gptel-tk--log-tool-error tool-name args (error-message-string err))
    (if gptel-tk-catch-errors
        msg
      (signal (car err) (cdr err)))))

(provide 'gptel-toolkit-core)

;;; gptel-toolkit-core.el ends here
