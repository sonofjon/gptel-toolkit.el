;;; gptel-toolkit-tools.el --- Tools for gptel-toolkit  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Andreas Jonsson
;;
;;; Commentary:
;;
;; This file contains 35+ tool implementations for gptel-toolkit, organized
;; into five categories.  All tools are defined using the gptel-tk-define
;; macro and registered with gptel.
;;
;; Tool Categories:
;;
;; 1. Buffer Operations (20 tools):
;;    - Search: regexp search with optional column numbers
;;    - Read: line-based reading with configurable limits
;;    - Edit: string/line replacement, insertion, deletion, append, replace
;;    - Batch edits: multiple edits with optional Ediff-based review
;;    - Management: listing, file association
;;
;; 2. File Operations (2 tools):
;;    - File creation
;;    - Directory creation with recursive support
;;
;; 3. Emacs Integration (8 tools):
;;    - Documentation: function docs, info nodes, symbol lookup
;;    - Library loading: load libraries into buffers for viewing
;;    - Code execution: evaluation of functions, buffers, and expressions
;;
;; 4. Project Management (4 tools):
;;    - Root detection
;;    - File listing with optional line counts
;;    - Pattern-based file finding (glob support)
;;    - Content searching with ripgrep/git grep integration
;;
;; 5. Testing Support (4 tools):
;;    - ERT test listing
;;    - Test execution and result reporting
;;
;; All tools include error handling and consistent output formatting.
;; Tools that create files or execute code include confirmation prompts.
;;
;;; Code:

(require 'gptel-toolkit-core)
(require 'gptel)

;;; Function Declarations

;; Backtrace functions
(declare-function backtrace-to-string "backtrace" (backtrace))

;; ERT functions
(declare-function ert--print-test-for-ewoc "ert" (ewoc-entry))
(declare-function ert--stats-failed-expected "ert" (stats))
(declare-function ert--stats-failed-unexpected "ert" (stats))
(declare-function ert--stats-passed-expected "ert" (stats))
(declare-function ert--stats-passed-unexpected "ert" (stats))
(declare-function ert--stats-skipped "ert" (stats))
(declare-function ert--stats-test-results "ert" (stats))
(declare-function ert--stats-tests "ert" (stats))
(declare-function ert-select-tests "ert" (selector universe))
(declare-function ert-stats-total "ert" (stats))
(declare-function ert-test-name "ert" (test))
(declare-function ert-test-result-expected-p "ert" (test result))
(declare-function ert-test-result-should-forms "ert" (test-result))
(declare-function ert-test-result-with-condition-backtrace "ert" (test-result))
(declare-function ert-test-result-with-condition-condition "ert" (test-result))
(declare-function ert-test-result-with-condition-p "ert" (test-result))
(declare-function make-ert--ewoc-entry "ert" (&key test hidden-p))

;; Info functions
(declare-function Info-goto-node "info" (node &optional no-going-back tryfile))

;; Library functions
(declare-function find-library-name "find-func" (library))

;; Project functions
(declare-function project-files "project" (project &optional dirs))
(declare-function project-root "project" (project))

;;; Customization

(defcustom gptel-tk-max-lines 100
  "Maximum number of lines the buffer reading tool will return per call."
  :type 'integer
  :group 'gptel-toolkit)

(defcustom gptel-tk-excluded-tools nil
  "Tool names to exclude when building a filtered list of GPTel tools.
This list is used by `gptel-tk--get-minimal-tools' to filter out
specific tools from the complete set of available GPTel tools."
  :type '(repeat string)
  :group 'gptel-toolkit)

;;; Tool definitions

;; Buffers

(gptel-tk-define gptel-tk-tool-buffer-search-regexp (buffer-name regexp &optional include-columns)
  "Search for content matching REGEXP in BUFFER-NAME.
Returns a newline-separated string of matching lines.  Each match is
formatted as \"LINE:TEXT\" or, if INCLUDE-COLUMNS is non-nil,
\"LINE:COLUMN:TEXT\" where LINE is the 1-based line number, COLUMN is
the 0-based column number, and TEXT is the full text of the matching
line."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' does not exist" buffer-name))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((results '()))
          (while (re-search-forward regexp nil t)
            (let* ((match-pos (match-beginning 0))
                   (line (line-number-at-pos match-pos))
                   (col (save-excursion (goto-char match-pos) (current-column)))
                   (line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (push (if include-columns
                        (format "%d:%d:%s" line col line-str)
                      (format "%d:%s" line line-str))
                    results)))
          (if results
              (mapconcat #'identity (nreverse results) "\n")
            (format "No matches found for regexp: %s" regexp)))))))

(gptel-tk-define gptel-tk-tool-open-file-in-buffer (file-path)
  "Open FILE-PATH into a visiting buffer."
  (unless (file-exists-p file-path)
    (error "No such file: %s" file-path))
  (when (file-directory-p file-path)
    (error "'%s' is a directory." file-path))
  (let ((buf (gptel-tk--with-suppressed-messages
              (find-file-noselect file-path))))
    (format "File '%s' opened in buffer '%s'." file-path (buffer-name buf))))

;; (gptel-tk-define gptel-tk-tool-read-buffer (buffer-name)
;;   "Return the contents of BUFFER-NAME."
;;   (unless (buffer-live-p (get-buffer buffer))
;;     (error "Buffer %s is not live." buffer))
;;   (with-current-buffer buffer
;;     (buffer-substring-no-properties (point-min) (point-max))))

;; (gptel-tk-define gptel-tk-tool-read-buffer-lines (buffer-name &optional start-line end-line)
;;   "Read lines from BUFFER-NAME between START-LINE and END-LINE.
;; When START-LINE is nil it defaults to 1.  When END-LINE is nil it
;; defaults to the end of the buffer.  If the length of the requested range
;; exceeds `gptel-tk-max-lines' an error is signaled.  If START-LINE
;; or END-LINE fall outside the buffer bounds they are silently truncated
;; to the valid range (1..buffer length)."
;;   (let ((buffer (get-buffer buffer-name)))
;;     (unless buffer
;;       (error "Buffer '%s' not found." buffer-name))
;;     (with-current-buffer buffer
;;       (save-excursion
;;         (let* ((total-lines (count-lines (point-min) (point-max)))
;;                (requested-start (or start-line 1))
;;                (requested-end   (or end-line total-lines)))
;;           (when (< requested-end requested-start)
;;             (error "END-LINE must be >= START-LINE"))
;;           (let* ((start-line (max 1 requested-start))
;;                  (end-line   (min total-lines requested-end))
;;                  (requested (1+ (- end-line start-line))))
;;             (when (> requested gptel-tk-max-lines)
;;               (error "Requested range length (%d) exceeds maximum allowed (%d)."
;;                      requested gptel-tk-max-lines))
;;             (goto-line start-line)
;;             (let ((start-pos (point)))
;;               (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
;;                 (goto-char (point-min))
;;                 (forward-line (1- end-line)))
;;               (let ((end-pos (line-end-position)))
;;                 (buffer-substring-no-properties start-pos end-pos)))))))))

(gptel-tk-define gptel-tk-tool-read-buffer-lines-count (buffer-name &optional start-line count)
  "Read COUNT lines from BUFFER-NAME starting at line START-LINE.
When START-LINE is nil it defaults to 1.  When COUNT is nil it defaults
to `gptel-tk-max-lines'.  If COUNT is greater than
`gptel-tk-max-lines' an error is signaled.  If the requested range
extends past the end of the buffer, the function returns only the
available lines."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buffer
      (save-excursion
        (let* ((total-lines (count-lines (point-min) (point-max)))
               (requested-start (or start-line 1))
               (requested-count (or count gptel-tk-max-lines)))
          (when (< requested-count 1)
            (error "COUNT must be >= 1"))
          (when (> requested-count gptel-tk-max-lines)
            (error "Requested COUNT (%d) exceeds maximum allowed (%d)."
                   requested-count gptel-tk-max-lines))
          (when (< requested-start 1)
            (error "START-LINE must be >= 1"))
          (when (> requested-start total-lines)
            (error "START-LINE (%d) exceeds buffer length (%d)."
                   requested-start total-lines))
          (let* ((start-line requested-start)
                 (end-line (min total-lines (+ start-line (1- requested-count)))))
            (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
              (let ((start-pos
                     (save-excursion
                       (goto-char (point-min))
                       (forward-line (1- start-line))
                       (point)))
                    (end-pos
                     (save-excursion
                       (goto-char (point-min))
                       (forward-line (1- end-line))
                       (line-end-position))))
                (buffer-substring-no-properties start-pos end-pos)))))))))

(gptel-tk-define gptel-tk-tool-read-buffer-definition (buffer-name definition-name)
  "Return the definition of DEFINITION-NAME from BUFFER-NAME.
Extracts any top-level definition (function, macro, variable, etc.)
using Emacs's structural parsing. Includes any unsaved changes. Works
language-agnostically wherever `beginning-of-defun` and `end-of-defun`
work."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer '%s' not found" buffer-name))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        ;; Search for the definition
        (unless (re-search-forward (concat "^(.*\\b" (regexp-quote definition-name) "\\b") nil t)
          (error "Definition '%s' not found in buffer" definition-name))
        ;; Move to beginning of definition
        (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
          (beginning-of-defun)
          (let ((beg (point)))
            ;; Move to end of definition
            (end-of-defun)
            (buffer-substring-no-properties beg (point))))))))

(gptel-tk-define gptel-tk-tool-list-buffers (&optional include-counts)
  "Return a newline-separated string of open file-backed buffers.
Each line is of the form \"NAME: PATH\", where NAME is the buffer name
and PATH is the file path relative to the current project root.  When
the file is outside the current project, PATH is the absolute file path.
If INCLUDE-COUNTS is non-nil, append the number of lines as \" (N
lines)\"."
  (let ((lines '())
        (proj (project-current)))
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (with-current-buffer buffer
          (let* ((buf-name (buffer-name buffer))
                 (file (buffer-file-name buffer))
                 (path (if (and file
                                proj
                                (file-in-directory-p file (project-root proj)))
                           (file-relative-name file (project-root proj))
                         file)))
            (if include-counts
                (push (format "%s: %s (%d lines)" buf-name path (count-lines (point-min) (point-max))) lines)
              (push (format "%s: %s" buf-name path) lines))))))
    (mapconcat #'identity (nreverse lines) "\n")))

(gptel-tk-define gptel-tk-tool-list-all-buffers (&optional include-counts)
  "Return a newline-separated string of all open buffers.
Each line is either of the form \"NAME: PATH\" for file-backed buffers
or just \"NAME\" for non-file buffers.  NAME is the buffer name and PATH
is the file path relative to the current project root.  When the file is
outside the current project, PATH is the absolute file path.  If
INCLUDE-COUNTS is non-nil, append the number of lines as \" (N lines)\"."
  (let ((lines '())
        (proj (project-current)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let* ((buf-name (buffer-name buffer))
               (file (buffer-file-name buffer))
               (path (when file (if (and
                                     proj
                                     (file-in-directory-p file (project-root proj)))
                                    (file-relative-name file (project-root proj))
                                  file))))
          (if file
              (if include-counts
                  (push (format "%s: %s (%d lines)" buf-name path (count-lines (point-min) (point-max))) lines)
                (push (format "%s: %s" buf-name path) lines))
            (if include-counts
                (push (format "%s (%d lines)" buf-name (count-lines (point-min) (point-max))) lines)
              (push buf-name lines))))))
    (mapconcat #'identity (nreverse lines) "\n")))

(gptel-tk-define gptel-tk-tool-buffer-to-file (buffer-name)
  "Return the file path for BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (unless (and buffer (buffer-file-name buffer))
      (error "Buffer '%s' not found or not associated with a file." buffer-name))
    (buffer-file-name buffer)))

(gptel-tk-define gptel-tk-tool-file-to-buffer (file-path)
  "Return the buffer name for FILE-PATH."
  (let ((buffer (find-buffer-visiting file-path)))
    (unless buffer
      (error "No buffer is visiting the file '%s'." file-path))
    (buffer-name buffer)))

(gptel-tk-define gptel-tk-tool-append-to-buffer (buffer-name text)
  "Append TEXT to BUFFER-NAME."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (insert text)))
    (format "Text successfully appended to buffer '%s'." buffer-name)))

(gptel-tk-define gptel-tk-tool-insert-in-buffer (buffer-name text line-number)
  "Insert TEXT in BUFFER-NAME at LINE-NUMBER.
The text is inserted at the beginning of the specified line."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buf
      (save-excursion
        (let ((total-lines (count-lines (point-min) (point-max))))
          (when (< line-number 1)
            (error "LINE-NUMBER must be >= 1"))
          (when (> line-number total-lines)
            (error "LINE-NUMBER (%d) exceeds buffer length (%d)."
                   line-number total-lines))
          (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
            (goto-char (point-min))
            (forward-line (1- line-number)))
          (insert text))))
    (format "Text successfully inserted into buffer '%s' at line %d." buffer-name line-number)))

(gptel-tk-define gptel-tk-tool-replace-buffer (buffer-name content)
  "Overwrite BUFFER-NAME with CONTENT."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buf
      (erase-buffer)
      (insert content))
    (format "Buffer '%s' successfully modified." buffer-name)))

(gptel-tk-define gptel-tk-tool-edit-buffer-string (buffer-name old-string new-string)
  "Replace a single instance of OLD-STRING with NEW-STRING in BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buffer
      (save-excursion
        (let ((count (count-matches (regexp-quote old-string)
                                    (point-min) (point-max))))
          (cond
           ((= count 0)
            (error "String '%s' not found in buffer '%s'." old-string buffer-name))
           ((> count 1)
            (error "String '%s' is not unique in buffer '%s'. Found %d occurrences." old-string buffer-name count))
           (t
            (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
              (goto-char (point-min)))
            (search-forward old-string)
            (replace-match new-string nil t)
            (format "String in buffer '%s' successfully replaced." buffer-name))))))))

(gptel-tk-define gptel-tk-tool-replace-buffer-line (buffer-name line-number content)
  "Replace line LINE-NUMBER in file BUFFER-NAME with CONTENT.
This wrapper function delegates replacement to
`gptel-tk-tool-replace-buffer-lines' with START-LINE and END-LINE both
equal to LINE-NUMBER."
  (let ((result (gptel-tk-tool-replace-buffer-lines buffer-name line-number line-number content)))
    ;; If the result is an error string (when gptel-tk-catch-errors
    ;; is t), replace the tool name to match this wrapper function
    (if (and gptel-tk-catch-errors (string-match "^tool: replace_buffer_lines: " result))
        (replace-regexp-in-string "^tool: replace_buffer_lines: " "tool: replace_buffer_line: "  result)
      ;; If not an error, return success message
      (format "Line %d in buffer '%s' successfully replaced." line-number buffer-name))))

(gptel-tk-define gptel-tk-tool-replace-buffer-lines (buffer-name start-line end-line content)
  "Replace lines START-LINE through END-LINE in BUFFER-NAME with CONTENT."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buf
      (save-excursion
        (let ((total-lines (count-lines (point-min) (point-max))))
          (when (< start-line 1)
            (error "START-LINE must be >= 1"))
          (when (< end-line start-line)
            (error "END-LINE must be >= START-LINE"))
          (when (> end-line total-lines)
            (error "END-LINE exceeds buffer length (%d)." total-lines))
          (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
            (goto-char (point-min))
            (forward-line (1- start-line))
            (let ((beg (point)))
              (goto-char (point-min))
              (forward-line (1- end-line))
              (end-of-line)
              (delete-region beg (point))
              (insert content))))))
    (format "Line range %d-%d in buffer '%s' successfully replaced."
            start-line end-line buffer-name)))

(gptel-tk-define gptel-tk-tool-delete-buffer-string (buffer-name old-string)
  "Delete a single instance of OLD-STRING in BUFFER-NAME.
This wrapper function delegates deletion to
`gptel-tk-tool-edit-buffer-string' by setting NEW-STRING to an empty
string."
  (let ((result (gptel-tk-tool-edit-buffer-string buffer-name old-string "")))
    ;; If the result is an error string (when gptel-tk-catch-errors
    ;; is t), replace the tool name to match this wrapper function
    (if (and gptel-tk-catch-errors (string-match "^tool: edit-buffer-string: " result))
        (replace-regexp-in-string "^tool: edit-buffer-string: " "tool: delete-buffer-string: " result)
      ;; If not an error, return success message
      (format "String in buffer '%s' successfully deleted." buffer-name))))

(gptel-tk-define gptel-tk-tool-delete-buffer-line (buffer-name line-number)
  "Delete line LINE-NUMBER in BUFFER-NAME.
This wrapper function delegates deletion to
`gptel-tk-tool-replace-buffer-line' by setting CONTENT to an empty
string."
  (let ((result (gptel-tk-tool-replace-buffer-line buffer-name line-number "")))
    ;; If the result is an error string (when gptel-tk-catch-errors
    ;; is t), replace the tool name to match this wrapper function
    (if (and gptel-tk-catch-errors (string-match "^tool: replace-buffer-line: " result))
        (replace-regexp-in-string "^tool: replace-buffer-line: " "tool: delete-buffer-line: " result)
      ;; If not an error, return success message
      (format "Line %d in buffer '%s' successfully deleted." line-number buffer-name))))

(gptel-tk-define gptel-tk-tool-delete-buffer-lines (buffer-name start-line end-line)
  "Delete lines START-LINE through END-LINE in BUFFER-NAME.
This wrapper function delegates deletion to
`gptel-tk-tool-replace-buffer-lines' by setting CONTENT to an empty
string."
  (let ((result (gptel-tk-tool-replace-buffer-lines buffer-name start-line end-line "")))
    ;; If the result is an error string (when gptel-tk-catch-errors
    ;; is t), replace the tool name to match this wrapper function
    (if (and gptel-tk-catch-errors (string-match "^tool: replace-buffer-lines: " result))
        (replace-regexp-in-string "^tool: replace-buffer-lines: " "tool: delete-buffer-lines: " result)
      ;; If not an error, return success message
      (format "Line range %d-%d in buffer '%s' successfully deleted."
              start-line end-line buffer-name))))

(defun gptel-tk--apply-buffer-edits (buffer-name buffer-edits edit-type)
  "Apply a list of edits to BUFFER-NAME.

BUFFER-EDITS is a list of property lists describing edits.  Each edit is
a plist with the following keys:
- :line-number (integer) -- The line to edit.
- :old-string (string) -- The text to replace.  Must not contain newline
                          characters.
- :new-string (string) -- The replacement text to insert.

EDIT-TYPE can be \='line or \='string.  For \='line edits the
:old-string is compared against the entire line; when equal the entire
line is replaced with :new-string.  For \='string edits the function
searches from the beginning of the specified line to the end of the line
for the first occurrence of :old-string and replaces that occurrence
with :new-string.

Edits are applied in descending order of :line-number to avoid shifting
subsequent line numbers."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buffer
      (let* ((edits (if (vectorp buffer-edits) (append buffer-edits nil) buffer-edits))
             (sorted-edits (sort edits #'(lambda (a b)
                                           (> (plist-get a :line-number)
                                              (plist-get b :line-number)))))
             (total 0)
             (applied 0)
             (failures nil))
        ;; Check for duplicate line numbers
        (let ((line-numbers (mapcar (lambda (edit) (plist-get edit :line-number)) sorted-edits))
              (seen-lines '())
              (duplicates '()))
          (dolist (line-num line-numbers)
            (if (member line-num seen-lines)
                (unless (member line-num duplicates)
                  (push line-num duplicates))
              (push line-num seen-lines)))
          (when duplicates
            (error "Duplicate line numbers found in edits: %s"
                   (mapconcat #'number-to-string (nreverse duplicates) ", "))))
        (dolist (edit sorted-edits)
          (setq total (1+ total))
          (let ((line-number (plist-get edit :line-number))
                (old-string (plist-get edit :old-string))
                (new-string (plist-get edit :new-string))
                (success nil))
            ;; Verify that old-string is single-line.
            (if (string-match-p "\n" old-string)
                (push (list line-number
                            "old-string contains newline"
                            old-string)
                      failures)
              ;; Check if line number is within buffer bounds
              (let ((buffer-line-count (count-lines (point-min) (point-max))))
                (if (> line-number buffer-line-count)
                    (push (list line-number
                                (format "line number exceeds buffer length (%d)" buffer-line-count)
                                old-string)
                          failures)
                  (save-excursion
                    (let ((transient-mark-mode nil))   ; suppress "Mark set" messages
                      (goto-char (point-min))
                      (forward-line (1- line-number)))
                    (cond
                     ((eq edit-type 'line)
                      (let ((line-start (point)))
                        (when (string-equal (buffer-substring-no-properties line-start (line-end-position)) old-string)
                          (delete-region line-start (line-end-position))
                          (insert new-string)
                          (setq success t))))
                     ((eq edit-type 'string)
                      (when (search-forward old-string (line-end-position) t)
                        (replace-match new-string nil nil)
                        (setq success t))))
                    (when success
                      (setq applied (1+ applied)))
                    (unless success
                      (push (list line-number
                                  (if (eq edit-type 'line)
                                      "entire line did not equal old-string"
                                    "old-string not found on the line")
                                  old-string)
                            failures))))))))
        (when failures
          (let ((failed (length failures))
                (details (mapconcat
                          (lambda (f)
                            (format " - line %d: %s (old-string: %S)"
                                    (nth 0 f) (nth 1 f) (nth 2 f)))
                          (nreverse failures)
                          "\n")))
            (error "Could not apply edits to buffer '%s': %d (out of %d) failed.\n%s"
                   buffer-name failed total details)))))))

(defun gptel-tk--review-buffer-edits (buffer-name buffer-edits edit-type)
  "Review a list of buffer edits in Ediff.

Creates a temporary buffer containing the content of the original buffer
BUFFER-NAME, applies the proposed edits, and then launches an Ediff
session to visually compare the original buffer against the edited
version.

BUFFER-EDITS is a list of property lists with the same shape as
described for `gptel-tk--apply-buffer-edits': each edit should contain
:line-number, :old-string, and :new-string.

EDIT-TYPE can be \='line or \='string, as described in
`gptel-tk--apply-buffer-edits'."
  (when buffer-edits
    ;; Only proceed if we have edits to apply
    (let ((original-buffer (get-buffer buffer-name)))
      (unless original-buffer
        (error "Buffer '%s' not found." buffer-name))

      (let* ((temp-buffer-name (format "*%s-edits*"
                                       (string-trim buffer-name "*" "*")))
             (temp-buffer (get-buffer-create temp-buffer-name)))
        (condition-case err
            (progn
              ;; Prepare the edited version in a temporary buffer
              (with-current-buffer temp-buffer
                (erase-buffer)
                (insert-buffer-substring original-buffer)
                (condition-case err
                    (gptel-tk--apply-buffer-edits temp-buffer-name buffer-edits edit-type)
                  (error
                   (error "%s\nNote: No review was started and no changes were applied to buffer '%s'. Any details above refer only to the temporary review buffer."
                          (error-message-string err) buffer-name))))

              ;; Start Ediff with a startup hook that cleans up the temp buffer on quit
              (let ((startup-hooks
                     (list (lambda ()
                             (let ((tb temp-buffer))
                               (add-hook 'ediff-quit-hook
                                         (lambda ()
                                           (when (buffer-live-p tb)
                                             (kill-buffer tb)))
                                         nil t))))))
                (ediff-buffers original-buffer temp-buffer startup-hooks)))
          (error
           ;; If anything failed before Ediff properly started, clean up the temp buffer
           (when (buffer-live-p temp-buffer)
             (kill-buffer temp-buffer))
           (signal (car err) (cdr err))))))))

(gptel-tk-define gptel-tk-tool-apply-buffer-string-edits (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of string edits.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  The function searches from the start of the
specified line to the end of the line for the first occurrence of
:old-string and replaces it with :new-string.  Edits are applied in
descending order of :line-number."
  (gptel-tk--apply-buffer-edits buffer-name buffer-edits 'string)
  (format "String edits successfully applied to buffer '%s'." buffer-name))

(gptel-tk-define gptel-tk-tool-apply-buffer-string-edits-with-review (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of string edits and review with Ediff.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  This function prepares a temporary buffer with
the string edits applied and launches `ediff-buffers' to let the user
review the changes interactively.  Edits are applied to the temporary
buffer only; the original buffer is not modified by this command."
  (gptel-tk--review-buffer-edits buffer-name buffer-edits 'string)
  (format "Ediff session started for %s. Please complete the review." buffer-name))

(gptel-tk-define gptel-tk-tool-apply-buffer-line-edits (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of line edits.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  The :old-string is compared against the entire
line; when it matches, the line is replaced with :new-string.  Edits are
applied in descending order of :line-number."
  (gptel-tk--apply-buffer-edits buffer-name buffer-edits 'line)
  (format "Line edits successfully applied to buffer '%s'." buffer-name))

(gptel-tk-define gptel-tk-tool-apply-buffer-line-edits-with-review (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of line edits and review with Ediff.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  This function prepares a temporary buffer with
the line edits applied and launches `ediff-buffers' to let the user
review the changes interactively.  Edits are applied to the temporary
buffer only; the original buffer is not modified by this command."
  (gptel-tk--review-buffer-edits buffer-name buffer-edits 'line)
  (format "Ediff session started for %s. Please complete the review." buffer-name))

;; Files

(gptel-tk-define gptel-tk-tool-create-file (file-path content)
  "Create a new file at FILE-PATH with CONTENT."
  (let ((full-path (expand-file-name file-path)))
    (when (file-exists-p full-path)
      (error "File already exists: %s" full-path))
    (with-temp-buffer
      (insert content)
      (gptel-tk--with-suppressed-messages
       (let ((require-final-newline nil)
             (mode-require-final-newline nil))
         (write-file full-path))))
    (format "Successfully created file: %s" full-path)))

(gptel-tk-define gptel-tk-tool-create-directory (dir-path)
  "Create a new directory at DIR-PATH."
  (let ((full-path (expand-file-name dir-path)))
    (when (file-exists-p full-path)
      (error "Directory already exists: %s" full-path))
    (make-directory full-path t)
    (format "Successfully created directory: %s" full-path)))

;; Emacs

(gptel-tk-define gptel-tk-tool-read-documentation (symbol-name)
  "Read the documentation for SYMBOL-NAME."
  (let* ((sym (intern-soft symbol-name))
         (doc (if (fboundp sym)
                  ;; Functions
                  (documentation sym)
                ;; Variables
                (documentation-property sym 'variable-documentation))))
    (or doc (format "No documentation found for symbol '%s'." symbol-name))))

(gptel-tk-define gptel-tk-tool-read-function (function-name)
  "Return the definition of FUNCTION-NAME."
  (let ((func-symbol (intern-soft function-name)))
    (unless (and func-symbol (fboundp func-symbol))
      (error "Symbol's function definition is void: %s" function-name))
    ;; Try to find the source location
    (let ((location (gptel-tk--with-suppressed-messages
                     (find-function-noselect func-symbol t))))
      (with-current-buffer (car location)
        (save-excursion
          (goto-char (cdr location))
          (let ((beg (point)))
            (forward-sexp 1)
            (buffer-substring-no-properties beg (point))))))))

(gptel-tk-define gptel-tk-tool-load-library (library-name &optional include-counts)
  "Load LIBRARY-NAME into a buffer.
If INCLUDE-COUNTS is non-nil, append the number of lines as \" (N
lines)\"."
  (let ((file (gptel-tk--with-suppressed-messages
               (find-library-name library-name))))
    (let* ((buffer (gptel-tk--with-suppressed-messages
                    (find-file-noselect file)))
           (original-name (buffer-name buffer))
           (clean-name (replace-regexp-in-string "\\.gz$" "" original-name)))
      ;; Rename buffer to remove .gz extension if present
      (unless (string= original-name clean-name)
        (with-current-buffer buffer
          (rename-buffer clean-name t)))
      ;; Return confirmation with buffer info
      (let ((base-message (format "Library '%s' loaded into buffer '%s'"
                                  library-name clean-name)))
        (if include-counts
            (format "%s (%d lines)." base-message
                    (with-current-buffer buffer
                      (count-lines (point-min) (point-max))))
          (format "%s." base-message))))))

;; (gptel-tk-define gptel-tk-tool-read-library (library-name)
;;   "Return the source code of LIBRARY-NAME."
;;   (let ((file (gptel-tk--with-suppressed-messages
;;                (find-library-name library-name))))
;;     (let* ((buffer (gptel-tk--with-suppressed-messages
;;                     (find-file-noselect file)))
;;            (original-name (buffer-name buffer))
;;            (clean-name (replace-regexp-in-string "\\.gz$" "" original-name)))
;;       ;; Rename buffer to remove .gz extension if present
;;       (unless (string= original-name clean-name)
;;         (with-current-buffer buffer
;;           (rename-buffer clean-name t)))
;;       ;; Return the buffer contents
;;       (with-current-buffer buffer
;;         (buffer-string)))))

(gptel-tk-define gptel-tk-tool-read-info-symbol (symbol-name)
  "Return the contents of the Info node for SYMBOL-NAME.
SYMBOL-NAME should be the name of an Emacs Lisp function, macro, or
variable (e.g., \"defun\", \"let\", \"buffer-string\").  The function
uses `info-lookup-symbol` to find where the symbol is documented and
returns the content of that Info page."
  ;; Track existing Info buffers before lookup to enable targeted cleanup
  (let ((info-buffer-names-before
         (mapcar #'buffer-name
                 (cl-remove-if-not
                  (lambda (buf)
                    (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buf)))
                  (buffer-list)))))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (condition-case err
              (progn
                (info-lookup-symbol (intern symbol-name))
                (buffer-string))
            (error (signal (car err) (cdr err)))))
      ;; Cleanup: kill any new Info buffers that were created
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buffer))
                   (not (member (buffer-name buffer) info-buffer-names-before)))
          (kill-buffer buffer))))))

(gptel-tk-define gptel-tk-tool-read-info-node (node-name)
  "Return the contents of the Info node NODE-NAME.
NODE-NAME should be the name of a section in the Elisp manual, such as
\"Control Structures\", \"Variables\", or \"Functions\".  The function
navigates to (elisp)NODE-NAME and returns the content from that Info
page."
  ;; Track existing Info buffers  before lookup to enable targeted cleanup
  (let ((info-buffer-names-before
         (mapcar #'buffer-name
                 (cl-remove-if-not
                  (lambda (buf)
                    (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buf)))
                  (buffer-list)))))
    (unwind-protect
        (condition-case err
            (progn
              (Info-goto-node (format "(elisp)%s" node-name))
              (let ((info-buffer (current-buffer)))
                (unless info-buffer (error "Not documented as a node: %s" node-name))
                (with-current-buffer info-buffer
                  (buffer-string))))
          (error (signal (car err) (cdr err))))
      ;; Cleanup: kill any new Info buffers that were created
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buffer))
                   (not (member (buffer-name buffer) info-buffer-names-before)))
          (kill-buffer buffer))))))

(gptel-tk-define gptel-tk-tool-eval-buffer (buffer-name)
  "Evaluate all Emacs Lisp code in BUFFER-NAME.
This evaluates the current buffer content, including any unsaved changes."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buf
      (eval-buffer))
    (format "Successfully evaluated all code in buffer %s." buffer-name)))

(gptel-tk-define gptel-tk-tool-eval-function (buffer-name function-name)
  "Evaluate FUNCTION-NAME in BUFFER-NAME.
This finds the function definition in the buffer and evaluates it,
including any unsaved changes."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' not found." buffer-name))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (unless (re-search-forward
                   (format "^(defun %s\\b" (regexp-quote function-name)) nil t)
            (error "Function '%s' not found in buffer '%s'."
                   function-name buffer-name)))
        (beginning-of-line)
        (let ((start (point)))
          (forward-sexp)
          (eval-region start (point)))))
    (format "Successfully evaluated function %s from buffer %s."
            function-name buffer-name)))

(gptel-tk-define gptel-tk-tool-eval-expression (expression)
  "Evaluate an Emacs Lisp EXPRESSION and return the result.
WARNING: This can execute arbitrary code and should be used with caution."
  (let ((result (eval (read expression))))
    (format "Expression result: %s" (prin1-to-string result))))

;; Project

(gptel-tk-define gptel-tk-tool-project-get-root ()
  "Get the root directory of the current project."
  (let ((project (project-current)))
    (unless project (error "Not inside a project."))
    (project-root project)))

(gptel-tk-define gptel-tk-tool-project-list-files (&optional include-counts)
  "Return a newline-separated string listing all files in the current project.
Each line is of the form \"NAME: PATH\", where NAME is the file's base
name and PATH is the path relative to the current project root.  If
INCLUDE-COUNTS is non-nil, append the number of lines as \" (N lines)\"."
  (let ((project (project-current)))
    (unless project (error "Not inside a project."))
    (let ((root (project-root project))
          (project-file-list (project-files project)))
      (mapconcat (lambda (f)
                   (let* ((rel (file-relative-name f root))
                          (name (file-name-nondirectory f)))
                     (if include-counts
                         (let ((nlines (with-temp-buffer
                                         (insert-file-contents f)
                                         (count-lines (point-min) (point-max)))))
                           (format "%s: %s (%d lines)" name rel nlines))
                       (format "%s: %s" name rel))))
                 project-file-list "\n"))))

;; (gptel-tk-define gptel-tk-tool-project-find-files (pattern)
;;   "In the current project, find files whose filenames contain PATTERN.
;; This function respects .gitignore.  It does not return directories."
;;   (with-temp-message "Running tool: my_project_find_files"
;;     (let ((proj (project-current)))
;;       (if (not proj)
;;           (error "No project found in the current context.")
;;         (let ((all-files (project-files proj)))
;;           (seq-filter (lambda (file) (string-search pattern (file-name-nondirectory file))) all-files))))))

(gptel-tk-define gptel-tk-tool-project-find-files-glob (pattern &optional include-counts)
  "In the current project, find files whose filenames match the glob PATTERN.
Returns a newline-separated string where each line is of the form
\"NAME: PATH\".  NAME is the file's base name and PATH is the path
relative to the current project root.  If INCLUDE-COUNTS is non-nil
append the number of lines as \" (N lines)\".  This function respects
.gitignore."
  (let ((proj (project-current)))
    (unless proj
      (error "No project found in the current context."))
    (let* ((root (project-root proj))
           ;; Get list of non-ignored files from project.el (absolute paths)
           (project-file-list (project-files proj))
           ;; Get list of files matching glob from filesystem (absolute paths)
           (wildcard-file-list
            (let ((default-directory root))
              ;; The 't' argument makes it return absolute paths
              (file-expand-wildcards pattern t)))
           ;; Return the files present in both lists
           (matched (seq-intersection project-file-list wildcard-file-list #'string-equal)))
      ;; Return as newline-separated relative paths, with optional line counts
      (mapconcat (lambda (f)
                   (let* ((rel (file-relative-name f root))
                          (name (file-name-nondirectory f)))
                     (if include-counts
                         (let ((nlines (with-temp-buffer
                                         (insert-file-contents f)
                                         (count-lines (point-min) (point-max)))))
                           (format "%s: %s (%d lines)" name rel nlines))
                       (format "%s: %s" name rel))))
                 matched "\n"))))

(gptel-tk-define gptel-tk-tool-project-search-regexp (regexp &optional include-columns)
  "In the current project, recursively search for content matching REGEXP.
Returns a newline-separated string of matching lines.  Each match is
formatted as PATH:LINE:TEXT or, if INCLUDE-COLUMNS is non-nil,
PATH:LINE:COLUMN:TEXT.  Both line and column numbers are 1-based.  This
search respects .gitignore."
  (let ((project (project-current)))
    (unless project (error "Not inside a project."))
    (let ((command (cond
                    ((executable-find "rg")
                     (let ((base (list "rg" "--no-heading" "--line-number" "--hidden" "--glob" "!.git/**"))
                           (flags (if include-columns
                                      (list "--column" "--vimgrep")
                                    (list "--no-column"))))
                       (append base flags (list "--regexp" regexp))))
                    ((executable-find "git")
                     (let ((base (list "git" "grep" "--full-name" "--perl-regexp" "--line-number"))
                           (flags (and include-columns (list "--column"))))
                       (append base flags (list "-e" regexp))))
                    (t (error "Neither 'rg' nor 'git' found for searching."))))
          (output-buffer (generate-new-buffer "*search-output*")))
      (unwind-protect
          (let* ((default-directory (project-root project))
                 (status (apply #'call-process (car command) nil
                                output-buffer nil (cdr command))))
            (cond
             ((zerop status)
              (with-current-buffer output-buffer
                (string-trim-right (buffer-string))))
             ((= status 1)
              (format "No matches found for regexp: %s" regexp))
             (t
              (error "Search command '%s' failed with status %d for regexp: %s"
                     (car command) status regexp))))
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))

;; Test

(defun gptel-tk--ert-parse-test-results (stats)
  "Parse ERT stats into a human-readable summary string.
STATS is an ERT stats object containing test results."
  (let ((total (ert-stats-total stats))
        (passed-expected (ert--stats-passed-expected stats))
        (failed-expected (ert--stats-failed-expected stats))
        (failed-unexpected (ert--stats-failed-unexpected stats))
        (passed-unexpected (ert--stats-passed-unexpected stats))
        (skipped (ert--stats-skipped stats)))
    (let ((passed (+ passed-expected failed-expected))
          (failed (+ passed-unexpected failed-unexpected)))
      (format "Ran %d test%s, %d passed, %d failed%s"
              total (if (= total 1) "" "s")
              passed failed
              (if (> skipped 0) (format ", %d skipped" skipped) "")))))

(defun gptel-tk--ert-format-detailed-results (stats)
  "Format detailed ERT test results for LLM consumption.
STATS is an ERT stats object containing test results."
  (let ((total (ert-stats-total stats))
        (detailed-info ""))
    (when (> total 0)
      (let ((tests (ert--stats-tests stats))
            (results (ert--stats-test-results stats)))
        (dotimes (i total)
          (let* ((test (aref tests i))
                 (result (aref results i))
                 (test-name (ert-test-name test)))
            (when (and result (ert-test-result-with-condition-p result))
              (setq detailed-info
                    (concat detailed-info
                            (format "\n\nTest: %s\n" test-name)))

              ;; Add failure information for failed tests
              (let ((condition (ert-test-result-with-condition-condition result)))
                (setq detailed-info
                      (concat detailed-info
                              (format "Condition:\n  %s\n" condition))))

              ;; Add failed assertions
              (when (ert-test-result-should-forms result)
                (setq detailed-info
                      (concat detailed-info
                              (format "Failed assertions:\n%s\n"
                                      (mapconcat
                                       (lambda (form) (format "  %s" form))
                                       (ert-test-result-should-forms result)
                                       "\n")))))

              ;; Add backtrace for failed tests
              (let ((backtrace (ert-test-result-with-condition-backtrace result)))
                (when backtrace
                  (with-temp-buffer
                    (let ((print-level 8)
                          (print-length 50))
                      (insert (backtrace-to-string backtrace)))
                    (goto-char (point-min))
                    (let ((truncated-backtrace ""))
                      (while (not (eobp))
                        (let* ((start (point))
                               (end (line-end-position))
                               (truncated-end (min end (+ start 70))))
                          (setq truncated-backtrace
                                (concat truncated-backtrace
                                        (buffer-substring-no-properties start truncated-end)
                                        "\n")))
                        (forward-line 1))
                      (setq detailed-info
                            (concat detailed-info
                                    (format "Backtrace:\n%s" truncated-backtrace))))))))))
        detailed-info))))

(defun gptel-tk--ert-format-simple-results (stats)
  "Format ERT test results using ERT's own print function directly.
Only shows unexpected results (failed tests), similar to ERT's default
behavior.  STATS is an ERT stats object containing test results."
  (with-temp-buffer
    ;; Set up buffer-local variable that ert--print-test-for-ewoc expects
    (setq-local ert--results-stats stats)

    ;; Summary line
    (let ((total (ert-stats-total stats))
          (passed (+ (ert--stats-passed-expected stats)
                     (ert--stats-failed-expected stats)))
          (failed (+ (ert--stats-passed-unexpected stats)
                     (ert--stats-failed-unexpected stats))))
      (insert (format "Ran %d test%s, %d passed, %d failed\n\n"
                      total
                      (if (= total 1) "" "s")
                      passed failed)))

    ;; Use ERT's own formatting function for each test
    (let ((tests (ert--stats-tests stats))
          (results (ert--stats-test-results stats)))
      (dotimes (i (length tests))
        (let ((test (aref tests i))
              (result (aref results i)))
          ;; Only show tests with unexpected results (failed tests)
          (when (and result (not (ert-test-result-expected-p test result)))
            ;; Create ewoc entry and use ERT's own print function
            ;; (let ((entry (make-ert--ewoc-entry
            ;;               :test test
            ;;               :hidden-p nil)))
            (let ((entry (make-ert--ewoc-entry test nil)))  ; fix compiler warning
              (ert--print-test-for-ewoc entry))
            (insert "\n")))))

    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(gptel-tk-define gptel-tk-tool-ert-list-unit-tests ()
  "List names of loaded ERT tests tagged \='unit\='."
  (require 'ert)
  (let* ((tests (ert-select-tests '(tag unit) t))
         (names (mapcar #'ert-test-name tests)))
    (if names
        (mapconcat (lambda (sym) (symbol-name sym)) names "\n")
      "No loaded ERT unit tests found.")))

(gptel-tk-define gptel-tk-tool-ert-run-unit ()
  "Run all ERT tests tagged \='unit\='."
  (require 'ert)

  ;; Run tests synchronously and capture results
  ;;   Suppress logging during test execution to avoid cluttering the log
  ;;   with internal tool calls made by the test code

  ;; Simple output
  (let* ((gptel-tk-suppress-logging t)
         (stats (ert-run-tests-batch '(tag unit))))
    ;; Format results similar to ERT buffer output
    (gptel-tk--ert-format-simple-results stats)))

  ;; Detailed output
  ;; (let* ((gptel-tk-suppress-logging t)
  ;;        (stats (ert-run-tests-batch '(tag unit)))
  ;;        (summary (gptel-tk--ert-parse-test-results stats))
  ;;        (detailed-info (gptel-tk--ert-format-detailed-results stats)))
  ;;   ;; Format results for LLM consumption with both summary and details
  ;;   (format "ERT Test Results for %s:\n%s%s"
  ;;           test-name
  ;;           summary
  ;;           detailed-info)))))

(gptel-tk-define gptel-tk-tool-ert-run-by-name (test-name)
  "Run a single ERT test by name and return results.
TEST-NAME is the string name of the ERT test symbol to run."
  (require 'ert)
  (let ((sym (intern test-name)))
    (unless (get sym 'ert--test)
      (error "No ERT test found named %s" test-name))

    ;; Run test synchronously and capture results
    ;;   Suppress logging during test execution to avoid cluttering the log
    ;;   with internal tool calls made by the test code

    ;; Simple output
    (let* ((gptel-tk-suppress-logging t)
           (stats (ert-run-tests-batch sym)))
      ;; Format results similar to ERT buffer output
      (gptel-tk--ert-format-simple-results stats))))

;;; Tool Registrations

;; Buffers

(gptel-make-tool
 :function #'gptel-tk-tool-buffer-search-regexp
 :name (gptel-tk--make-tool-name "buffer_search_regexp")
 :description "Search a buffer for content matching a regexp. This returns a newline-separated string of matching lines. Each line is formatted as LINE:TEXT or, if 'include-columns' is true, LINE:COLUMN:TEXT where LINE is 1-based and COLUMN is 0-based."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to search.")
         (:name "regexp"
                :type string
                :description "The regexp to search for in the buffer.")
         (:name "include-columns"
                :type boolean
                :optional t
                :description "If true, include 0-based column numbers in the result."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-open-file-in-buffer
 :name (gptel-tk--make-tool-name "open_file_in_buffer")
 :description "Open a file into a visiting buffer."
 :args (list '(:name "file-path"
                     :type string
                     :description "The path to the file to open."))
 :category "buffers")

;; (gptel-make-tool
;;  :function #'gptel-tk-tool-read-buffer
;;  :name (gptel-tk--make-tool-name "read_buffer")
;;  :description "Return the contents of a buffer."
;;  :args (list '(:name "buffer-name"
;;                       :type string
;;                       :description "The name of the buffer to read."))

;; (gptel-make-tool
;;  :function #'gptel-tk-tool-read-buffer-lines
;;  :name (gptel-tk--make-tool-name "read_buffer_lines")
;;  :description (format "Read lines from a buffer; max lines per call: %d. Use chunking for larger ranges. 'start-line' and 'end-line' are optional 1-based line numbers; if 'start-line' is omitted or false, read from the beginning of the buffer. If 'end-line' is omitted or false, read to the end of the buffer." gptel-tk-max-lines)
;;  :args (list '( :name "buffer-name"
;;                 :type string
;;                 :description "The name of the buffer to read the contents of.")
;;              '( :name "start-line"
;;                 :type integer
;;                 :optional t
;;                 :description "The first line to read from.")
;;              '( :name "end-line"
;;                 :type integer
;;                 :optional t
;;                 :description "The last line to read to."))
;;  :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-read-buffer-lines-count
 :name (gptel-tk--make-tool-name "read_buffer_lines_count")
 :description (format "Read lines from a buffer; max lines per call: %d. 'start-line' and 'count' are optional and default to 1 and %d, respectively." gptel-tk-max-lines gptel-tk-max-lines)
 :args (list '( :name "buffer-name"
                :type string
                :description "The name of the buffer to read from.")
             '( :name "start-line"
                :type integer
                :optional t
                :description "The 1-based line number to start reading from (default is 1).")
             (list :name "count"
                   :type integer
                   :optional t
                   :description (format "The number of lines to read (must be <= %d)."
                                        gptel-tk-max-lines)))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-read-buffer-definition
 :name (gptel-tk--make-tool-name "read_buffer_definition")
 :description "Return the code of a given top-level definition (function, macro, variable, etc.) from a specified buffer. Uses Emacs's structural parsing to extract complete code blocks, including any unsaved changes. Prefer this for user code to read the exact text in the buffer and to avoid stale reads from the loaded definition."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer containing the definition.")
             '(:name "definition-name"
                     :type string
                     :description "The name of the definition to return the code for (function, macro, variable, etc.)."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-list-buffers
 :name (gptel-tk--make-tool-name "list_buffers")
 :description "Return a newline-separated string listing all currently open buffers that are associated with a file. Each line is of the form \"NAME: PATH\" where NAME is the buffer name and PATH is the file path relative to the current project root when the file is inside a project; otherwise PATH is the absolute file path. If the optional argument 'include-counts' is true, append the number of lines as \" (N lines)\"."
 :args '((:name "include-counts"
                :type boolean
                :optional t
                :description "If true, append the number of lines to each entry as ' (N lines)'."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-list-all-buffers
 :name (gptel-tk--make-tool-name "list_all_buffers")
 :description "Return a newline-separated string listing all currently open buffers. Each line is either of the form \"NAME: PATH\" for file-backed buffers or just \"NAME\" for non-file buffers.  NAME is the buffer name and PATH is the file path relative to the current project root.  When the file is outside the current project, PATH is the absolute file path.  If INCLUDE-COUNTS is true, append the number of lines as \" (N lines)\"."
 :args '((:name "include-counts"
                :type boolean
                :optional t
                :description "If true, append the number of lines to each entry as ' (N lines)'."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-buffer-to-file
 :name (gptel-tk--make-tool-name "buffer_to_file")
 :description "Return the file path for a given buffer."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-file-to-buffer
 :name (gptel-tk--make-tool-name "file_to_buffer")
 :description "Return the buffer name for a given file path."
 :args (list '(:name "file-path"
                     :type string
                     :description "The path to the file."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-append-to-buffer
 :name (gptel-tk--make-tool-name "append_to_buffer")
 :description "Append text to a buffer (at the end of the buffer)."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to append text to.")
             '(:name "text"
                     :type string
                     :description "The text to append to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-insert-in-buffer
 :name (gptel-tk--make-tool-name "insert_in_buffer")
 :description "Insert text in a buffer at a specific line number. The text is inserted at the beginning of the specified line."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to insert text into.")
             '(:name "text"
                     :type string
                     :description "The text to insert into the buffer.")
             '(:name "line-number"
                     :type integer
                     :description "The 1-based line number where the text should be inserted."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-replace-buffer
 :name (gptel-tk--make-tool-name "replace_buffer")
 :description "Completely overwrite the contents of a buffer."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to overwrite.")
             '(:name "content"
                     :type string
                     :description "The content to write to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-edit-buffer-string
 :name (gptel-tk--make-tool-name "edit_buffer_string")
 :description "Edit a buffer by replacing a single instance of an exact string: The tool replaces a single instance of 'old-string' with 'new-string' in BUFFER-NAME. 'old-string' is treated literally and may contain newline characters; it must occur exactly once. 'new-string' may contain newline characters and will be inserted as-is."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to edit.")
         (:name "old-string"
                :type string
                :description "The text to be replaced by 'new-string'.")
         (:name "new-string"
                :type string
                :description "The text to replace 'old-string' with."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-replace-buffer-line
 :name (gptel-tk--make-tool-name "replace_buffer_line")
 :description "Replace a single line in a buffer with new content. The new content may contain newline characters."
 :args '((:name "buffer-name" :type string :description "The name of the buffer to modify.")
         (:name "line-number" :type integer :description "The 1-based line number of the line to replace.")
         (:name "content" :type string :description "The new content."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-replace-buffer-lines
 :name (gptel-tk--make-tool-name "replace_buffer_lines")
 :description "Replace a range of lines in a buffer with new content. The new content may contain newline characters. To replace a single line set 'start-line' == 'end-line'. Line numbers are 1-based."
 :args (list '(:name "buffer-name" :type string
                     :description "The name of the buffer to modify.")
             '(:name "start-line" :type integer
                     :description "The first line of the range to replace.")
             '(:name "end-line" :type integer
                     :description "The last line of the range to replace.")
             '(:name "content" :type string
                     :description "The new content."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-delete-buffer-string
 :name (gptel-tk--make-tool-name "delete_buffer_string")
 :description "Delete a single instance of an exact string from a buffer. 'old-string' is treated literally and may contain newline characters; it must occur exactly once."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to edit.")
         (:name "old-string"
                :type string
                :description "The text to delete."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-delete-buffer-line
 :name (gptel-tk--make-tool-name "delete_buffer_line")
 :description "Delete a single line in a buffer."
 :args '((:name "buffer-name" :type string :description "The name of the buffer to modify.")
         (:name "line-number" :type integer :description "The 1-based line number of the line to delete."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-delete-buffer-lines
 :name (gptel-tk--make-tool-name "delete_buffer_lines")
 :description "Delete a range of lines in a buffer. To delete a single line set 'start-line' == 'end-line'. Line numbers are 1-based."
 :args (list '(:name "buffer-name" :type string
                     :description "The name of the buffer to modify.")
             '(:name "start-line" :type integer
                     :description "The first line of the range to delete.")
             '(:name "end-line" :type integer
                     :description "The last line of the range to delete."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-apply-buffer-string-edits
 :name (gptel-tk--make-tool-name "apply_buffer_string_edits")
 :description "Edit a buffer with a list of string edits, applying changes directly without review. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. The 'old-string' must be found entirely on the specified line (it must not contain newline characters). The 'new-string' may contain newline characters and will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number where the edit starts.")
                                    :old-string
                                    (:type string :description "The string to be replaced by 'new-string'.")
                                    :new-string
                                    (:type string :description "The string to replace 'old-string' with.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-apply-buffer-string-edits-with-review
 :name (gptel-tk--make-tool-name "apply_buffer_string_edits_with_review")
 :description "Edit a buffer with a list of string edits and start an Ediff session for review. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. The 'old-string' must be found entirely on the specified line (it must not contain newline characters). The 'new-string' may contain newline characters and will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly.

This action requires manual user review. After calling this tool, you must stop and instruct the user to complete the review in the Ediff session and to notify you when they are finished. Do not proceed with any other tools or actions until you receive confirmation from the user."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number where the edit starts.")
                                    :old-string
                                    (:type string :description "The string to be replaced by 'new-string'.")
                                    :new-string
                                    (:type string :description "The string to replace 'old-string' with.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-apply-buffer-line-edits
 :name (gptel-tk--make-tool-name "apply_buffer_line_edits")
 :description "Edit a buffer with a list of edits, applying changes directly without review. Each edit targets a specific line and must contain a 'line-number' and an 'old-string' that must exactly match that line's content (old-string must not contain newline characters). The 'new-string' will replace the line and may contain newline characters; it will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number of the line to modify.")
                                    :old-string
                                    (:type string :description "The entire original content of the line to be replaced.")
                                    :new-string
                                    (:type string :description "The new content.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'gptel-tk-tool-apply-buffer-line-edits-with-review
 :name (gptel-tk--make-tool-name "apply_buffer_line_edits_with_review")
 :description "Edit a buffer with a list of edits and start an Ediff session for review. Each edit targets a specific line and must contain a 'line-number' and an 'old-string' that must exactly match that line's content (old-string must not contain newline characters). The 'new-string' will replace the matched text and may contain newline characters; it will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly.

This action requires manual user review. After calling this tool, you must stop and instruct the user to complete the review in the Ediff session and to notify you when they are finished. Do not proceed with any other tools or actions until you receive confirmation from the user."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number of the line to modify.")
                                    :old-string
                                    (:type string :description "The entire original content of the line to be replaced.")
                                    :new-string
                                    (:type string :description "The new content.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

;; Files

(gptel-make-tool
 :function #'gptel-tk-tool-create-file
 :name (gptel-tk--make-tool-name "create_file")
 :description "Create a new file with the specified content. Fails if the file already exists."
 :args '((:name "file-path"
                :type string
                :description "The path of the file to create.")
         (:name "content"
                :type string
                :description "The content to write to the new file."))
 :confirm t
 :category "files")

(gptel-make-tool
 :function #'gptel-tk-tool-create-directory
 :name (gptel-tk--make-tool-name "create_directory")
 :description "Create a new directory at the specified path. Creates parent directories as needed."
 :args '((:name "dir-path"
                :type string
                :description "The path of the directory to create."))
 :confirm t
 :category "files")

;; Emacs

(gptel-make-tool
 :function #'gptel-tk-tool-read-documentation
 :name (gptel-tk--make-tool-name "read_documentation")
 :description "Return the documentation for a given Emacs Lisp symbol. The symbol can be either a function or a variable"
 :args (list '(:name "symbol-name"
                     :type string
                     :description "The name of the symbol to return the documentation for."))
 :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-read-function
 :name (gptel-tk--make-tool-name "read_function")
 :description "Return the code of an Emacs Lisp function currently registered in memory (the evaluated definition), not from buffers. Use only for functions that are certain not to have been modified by the user (for example, built-in libraries such as find-file-noselect). Using this on user code risks reading a stale definition if there are buffer edits or unsaved changes; for user code prefer 'read_buffer_definition'."
 :args (list '(:name "function-name"
                     :type string
                     :description "The name of the function to return the code for."))
 :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-load-library
 :name (gptel-tk--make-tool-name "load_library")
 :description "Load an Emacs library or package into a buffer."
 :args (list '(:name "library-name"
                     :type string
                     :description "The name of the library or package to load into a buffer.")
             '(:name "include-counts"
                     :type boolean
                     :description "If true, include the number of lines in the result."))
 :category "emacs")

;; (gptel-make-tool
;;  :function #'gptel-tk-tool-read-library
;;  :name (gptel-tk--make-tool-name "read_library")
;;  :description "Return the source code for an Emacs library or package."
;;  :args (list '(:name "library-name"
;;                      :type string
;;                      :description "The name of the library or package to return source code for."))
;;  :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-read-info-symbol
 :name (gptel-tk--make-tool-name "read_info_symbol")
 :description "Return the contents of the info node where a given Emacs Lisp symbol is documented."
 :args (list '(:name "symbol-name"
                     :type string
                     :description "The name of the symbol to look up."))
 :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-read-info-node
 :name (gptel-tk--make-tool-name "read_info_node")
 :description "Return the contents of a specific info node from the Emacs Lisp manual."
 :args (list '(:name "node-name"
                     :type string
                     :description "The name of the node in the Emacs Lisp manual."))
 :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-eval-buffer
 :name (gptel-tk--make-tool-name "eval_buffer")
 :description "Evaluate all Emacs Lisp code in a buffer. This evaluates the buffer's current content, including any unsaved changes."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to evaluate."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-eval-function
 :name (gptel-tk--make-tool-name "eval_function")
 :description "Evaluate a function definition from a buffer. This finds the function definition in the buffer and evaluates it, including any unsaved changes."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer containing the function.")
         (:name "function-name"
                :type string
                :description "The name of the function to evaluate."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :function #'gptel-tk-tool-eval-expression
 :name (gptel-tk--make-tool-name "eval_expression")
 :description "Evaluate an Emacs Lisp expression and return the result. WARNING: This can execute arbitrary code and should be used with caution."
 :args '((:name "expression"
                :type string
                :description "The Emacs Lisp expression to evaluate."))
 :confirm t
 :category "emacs")

;; Project

(gptel-make-tool
 :function #'gptel-tk-tool-project-get-root
 :name (gptel-tk--make-tool-name "project_get_root")
 :description "Get the root directory of the current project."
 :args nil
 :category "project")

(gptel-make-tool
 :function #'gptel-tk-tool-project-list-files
 :name (gptel-tk--make-tool-name "project_list_files")
 :description "Return a newline-separated string listing all files in the current project. Each line is of the form \"NAME: PATH\", where NAME is the file's base name and PATH is the path relative to the current project root. If the optional argument 'include-counts' is true, append the number of lines as \" (N lines).\""
 :args '((:name "include-counts"
                :type boolean
                :optional t
                :description "If true, append the number of lines to each entry as ' (N lines)'."))
 :category "project")

;; (gptel-make-tool
;;  :function #'gptel-tk-tool-project-find-files
;;  :name (gptel-tk--make-tool-name "project_find_files")
;;  :description "In the current project, recursively find files whose filenames contain pattern. This search is case-sensitive. It does not find directories."
;;  :args '((:name "pattern"
;;                 :type string
;;                 :description "A pattern to match against the filenames in the project."))
;;  :category "proj

(gptel-make-tool
 :function #'gptel-tk-tool-project-find-files-glob
 :name (gptel-tk--make-tool-name "project_find_files_glob")
 :description "In the current project, find files matching a glob pattern. To search recursively, use the '**/' prefix. For example, a pattern of '**/*.el' finds all Emacs Lisp files in the project, while '*.el' finds them only in the root directory. The result is a newline-separated string where each line is of the form \"NAME: PATH\", where NAME is the file's base name and PATH is the path relative to the current project root.  If the optional argument 'include-counts' is true, append the number of lines as \" (N lines)\"."
 :args '((:name "pattern"
                :type string
                :description "A glob pattern to match against the filenames in the project.")
         (:name "include-counts"
                :type boolean
                :optional t
                :description "If true, append the number of lines to each entry as ' (N lines)'."))
 :category "project")

(gptel-make-tool
 :function #'gptel-tk-tool-project-search-regexp
 :name (gptel-tk--make-tool-name "project_search_regexp")
 :description "In the current project, recursively search for content matching REGEXP. The tool returns a newline-separated string of matching lines. Each line includes: PATH:LINE:TEXT, where PATH is the file path relative to the current project root, LINE is the 1-based line number of the match, and TEXT is the matched line text. If the optional argument 'include-columns' is true, the tool returns PATH:LINE:COLUMN:TEXT, where COLUMN is the 1-based column number of the match."
 :args '((:name "regexp"
                :type string
                :description "A regexp to search for in the project files. The regexp should be compatible with ripgrep or git grep.")
         (:name "include-columns"
                :type boolean
                :optional t
                :description "If true, include 1-based column numbers in the result."))
 :category "project")

;; Test

(gptel-make-tool
 :function #'gptel-tk-tool-ert-list-unit-tests
 :name (gptel-tk--make-tool-name "ert_list_unit_tests")
 :description "Return a newline-separated list of names for loaded ERT unit tests."
 :args '()
 :category "test")

(gptel-make-tool
 :function #'gptel-tk-tool-ert-run-unit
 :name (gptel-tk--make-tool-name "ert_run_unit")
 :description "Run all ERT unit tests."
 :args '()
 :category "test")

(gptel-make-tool
 :function #'gptel-tk-tool-ert-run-by-name
 :name (gptel-tk--make-tool-name "ert_run_by_name")
 :description "Run a single ERT unit test by name."
 :args '((:name "test-name"
                :type string
                :description "The name of the ERT test symbol to run."))
 :category "test")

(provide 'gptel-toolkit-tools)

;;; gptel-toolkit-tools.el ends here
