# gptel-toolkit AI Agent Instructions

## Project Overview

gptel-toolkit extends gptel (Emacs AI client) with 35+ tools and a framework for defining tools with support for messaging, logging, and error handling. The toolkit provides buffer operations, file operations, Emacs integration, project management, and testing support.

## Architecture

### Core Components
- **gptel-toolkit.el**: Main entry point with tool management functions
- **gptel-toolkit-core.el**: Framework infrastructure (messaging, logging, error handling, tool definition macro)
- **gptel-toolkit-tools.el**: 35+ tool implementations across 5 categories

### Tool Definition Pattern
All tools use the `gptel-tk-define` macro which provides:
- Automatic error handling (catch/return vs re-signal based on `gptel-tk-catch-errors`)
- Logging to `*gptel-tool-log*` buffer (suppressible via `gptel-tk-suppress-logging`)
- Minibuffer messaging with argument filtering and truncation
- JSON compatibility (`:json-false` → `nil` normalization)

```elisp
(gptel-tk-define gptel-tk-tool-example (arg1 &optional arg2)
  "Tool docstring."
  ;; Implementation here
  (format "Result: %s" arg1))
```

### Tool Naming Convention
- Function names: `gptel-tk-tool-*` (e.g., `gptel-tk-tool-buffer-search-regexp`)
- Registration names: Configurable via `gptel-tk-tool-prefix` (defaults to clean names like `buffer_search`)

## Tool Management

### Enable/Disable Tools
```elisp
(gptel-tk-enable-builtin-tools)   ; Enable all 35+ tools
(gptel-tk-disable-builtin-tools)  ; Disable all toolkit tools
```

Tools can be excluded via the `gptel-tk-excluded-tools` list.

## Tool Categories & Key Tools

### Buffer Operations (20+ tools)
- `gptel-tk-tool-buffer-search-regexp`: Search with optional column numbers
- `gptel-tk-tool-read-buffer-*`: Line-based reading and definition extraction
- `gptel-tk-tool-list-*-buffers`: Buffer listing with optional counts
- `gptel-tk-tool-edit-buffer-string`: String replacement
- `gptel-tk-tool-replace-buffer-*`: String/line replacement
- `gptel-tk-tool-delete-buffer-*`: String/line deletion
- `gptel-tk-tool-apply-buffer-*-edits*`: Batch edits with optional Ediff review
- `gptel-tk-tool-*-buffer`: Content insertion, appending, replacement

### File Operations (2 tools)
- `gptel-tk-tool-create-file`: File creation with confirmation
- `gptel-tk-tool-create-directory`: Recursive directory creation

### Emacs Integration (8 tools)
- `gptel-tk-tool-read-function`: Function documentation lookup
- `gptel-tk-tool-read-info-*`: Info system access (symbols, nodes)
- `gptel-tk-tool-eval-*`: Expression/function/buffer evaluation
- `gptel-tk-tool-*-library`: Load and read libraries

### Project Management (4 tools)
- `gptel-tk-tool-project-get-root`: Detect project root
- `gptel-tk-tool-project-list-files`: File listing with line counts
- `gptel-tk-tool-project-find-files-glob`: Pattern-based file finding
- `gptel-tk-tool-project-search-regexp`: Content searching with ripgrep/git grep

### Testing Support (3 tools)
- `gptel-tk-tool-ert-list-unit-tests`: List ERT tests with tags
- `gptel-tk-tool-ert-run-unit`: Execute all unit tests
- `gptel-tk-tool-ert-run-by-name`: Execute specific tests by name

## Testing Conventions

### Test Structure
- **File**: `test/gptel-toolkit-test.el`
- **Framework**: ERT (Emacs Lisp Regression Testing)
- **Tags**: `:tags '(unit category)` for organization
- **Helpers**: Custom macros for temp buffers/files/projects

### Key Test Helpers
```elisp
(with-temp-buffer-with-content buffer-name content ...)  ; Creates temp buffer with content
(with-temp-file-with-content file-var content ...)       ; Creates temp file with content
(with-temp-project ...)                                  ; Creates temp git repository with dummy files
```

### Error Testing Pattern
Tools have dual error modes controlled by `gptel-tk-catch-errors`:
- `nil`: Re-signal errors (for interactive use)
- `t`: Return errors as strings (for AI consumption)

### VS Code Tasks
- `Run all unit tests`: Batch execution of all unit tests
- `Run unit test test-gptel-tk-open-file-in-buffer`: Single test execution

## Key Configuration Variables

```elisp
(setq gptel-tk-max-lines 100)           ; Max lines per buffer read
(setq gptel-tk-tool-prefix "")          ; Tool name prefix
(setq gptel-tk-catch-errors t)          ; Error handling mode
(setq gptel-tk-suppress-logging nil)    ; Logging control
(setq gptel-tk-excluded-tools '())      ; Tools to exclude
```

## Development Patterns

### Adding New Tools
1. Use `gptel-tk-define` macro in `gptel-toolkit-tools.el`
2. Follow naming convention: `gptel-tk-tool-*`
3. Add comprehensive tests with both success and error cases
4. Document in tool docstring and update README

### Error Handling
Always test both error modes: use `should-error` when `gptel-tk-catch-errors` is `nil`, and string comparison when it's `t`.

### Message Suppression
Tools should use `gptel-tk--with-suppressed-messages` around Emacs functions that produce informational messages (e.g., `find-file-noselect`, `write-file`, `find-function-noselect`).

## Testing Commands

Run tests via VS Code tasks or directly:
```bash
# All unit tests
emacs -Q --batch --eval "(package-initialize)" --eval "(push \"${workspaceFolder}\" load-path)" -l test/gptel-toolkit-test.el --eval "(setq debug-on-error t)" --eval "(ert-run-tests-batch-and-exit '(tag unit))"

# Single test
emacs -Q --batch --eval "(package-initialize)" --eval "(push \"${workspaceFolder}\" load-path)" -l test/gptel-toolkit-test.el --eval "(setq debug-on-error t)" --eval "(ert-run-tests-batch-and-exit 'test-name)"
```