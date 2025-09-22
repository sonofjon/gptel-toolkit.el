# gptel-toolkit

A toolkit and framework for gptel tools in Emacs.

## Overview

gptel-toolkit provides 35+ built-in tools and a framework that extends
gptel with enhanced tool capabilities.

- **Framework**: Tool definition macro that adds logging, messaging, and
  error handling

- **Built-in Tools** (35+ tools across 5 categories):
  - Buffer operations: list, search, read, edit, batch edits with review
  - File operations: create files and directories
  - Emacs integration: documentation lookup, function evaluation, library
    loading
  - Project management: file listing, searching, root detection
  - Testing support: ERT test execution and listing

## Installation

### Requirements

- Emacs 28.1+
- gptel 0.9.5+

### Manual Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/sonofjon/gptel-toolkit.git
   ```

2. Add the directory to your Emacs `load-path`:
   ```elisp
   (add-to-list 'load-path "/path/to/gptel-toolkit")
   ```

3. Load and enable the toolkit:
   ```elisp
   (require 'gptel-toolkit)
   (gptel-tk-enable-builtin-tools)
   ```

### Using use-package

If you use `use-package`, you can install directly from GitHub:

```elisp
(use-package gptel-toolkit
  :vc (:url "https://github.com/sonofjon/gptel-toolkit.git")
  :after gptel
  :config
  (gptel-tk-enable-builtin-tools))
```

## Usage

### Managing Built-in Tools

You can enable or disable built-in tools manually:

```elisp
;; Enable all built-in tools
(gptel-tk-enable-builtin-tools)

;; Disable all built-in tools
(gptel-tk-disable-builtin-tools)
```

### Creating Custom Tools

The `gptel-tk-define` macro makes it easy to create your own tools that
integrate seamlessly with gptel. Custom tools automatically get logging,
messaging capabilities, and error handling from the framework.

```elisp
;; Define a custom tool using the framework
(gptel-tk-define my-custom-tool (arg1 arg2)
  "Example custom tool that does something with ARG1 and ARG2."
  (format "Processed: %s and %s" arg1 arg2))

;; Register the tool with gptel
(gptel-make-tool
 :function #'my-custom-tool
 :name "my_custom_tool"
 :description "Example custom tool"
 :args '((:name "arg1" :type string :description "First argument")
         (:name "arg2" :type string :description "Second argument")))
```

## Configuration

The toolkit provides several customization options to control its behavior:

### Core Framework Options

#### `gptel-tk-log-buffer`
- **Type**: String
- **Default**: `"*gptel-tool-log*"`
- **Description**: Name of the buffer used for tool logging

```elisp
(setq gptel-tk-log-buffer "*my-tool-log*")
```

#### `gptel-tk-suppress-logging`
- **Type**: Boolean
- **Default**: `nil`
- **Description**: When non-nil, suppress all tool logging

```elisp
(setq gptel-tk-suppress-logging t)
```

#### `gptel-tk-catch-errors`
- **Type**: Boolean
- **Default**: `t`
- **Description**: When non-nil, catch tool errors and return as strings instead of signaling

```elisp
(setq gptel-tk-catch-errors nil)
```

#### `gptel-tk-tool-prefix`
- **Type**: String
- **Default**: `""`
- **Description**: Prefix to add to all tool registration names. Useful to avoid conflicts with other tool packages

```elisp
;; Tools become "tk_buffer_search", "tk_file_create", etc.
(setq gptel-tk-tool-prefix "tk_")
```

### Built-in Tools Options

#### `gptel-tk-max-lines`
- **Type**: Integer
- **Default**: `100`
- **Description**: Maximum number of lines the buffer reading tool will
  return per call

```elisp
(setq gptel-tk-max-lines 200)
```

#### `gptel-tk-excluded-tools`
- **Type**: List of strings
- **Default**: `nil`
- **Description**: Tool names to disable in the gptel tool lists

```elisp
(setq gptel-tk-excluded-tools '("buffer_edit" "file_create"))
```

### Example Configuration

```elisp
(use-package gptel-toolkit
  :after gptel
  :custom
  ;; Exclude some tools
  (gptel-tk-excluded-tools '(;; Redundant
                             "list_buffers"
                             "insert_in_buffer"
                             "replace_buffer_line"
                             "delete_buffer_line"
                             "delete_buffer_string"
                             "apply_buffer_line_edits"
                             "apply_buffer_line_edits_with_review"
                             ;; Unwanted
                             "replace_buffer"
                             "ert_run_unit"))
  :config
  ;; Enable built-in tools
  ;; (gptel-tk-enable-builtin-tools)
  ;; Set built-in tools in preset
  (plist-put (gptel-get-preset 'coding) :tools (gptel-tk-get-tool-names)))
```
