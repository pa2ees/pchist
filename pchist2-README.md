# pchist2 - Project Compile History v2

## Overview

pchist2 is a complete rewrite of the pchist compile history system, providing robust data persistence and CRUD operations for managing project compile commands.

## Files

- **pchist2-data.el** - Core data structures and CRUD operations (380 lines)
- **pchist2-data-test.el** - Comprehensive ERT test suite (390 lines, 29 tests)

## Features

### Data Structures

**Command Record:**
- `project` - Project root path (string)
- `command` - Command name (e.g., "./build.sh")
- `switches` - List of switches (each can contain spaces, e.g., "-a x86_64")
- `targets` - List of target names
- `installers` - List of installer records
- `last_used` - ISO8601 timestamp with microseconds

**Installer Record:**
- `command` - Installer command (e.g., "scp", "rsync")
- `switches` - List of switches
- `artifacts` - List of file paths to install
- `host` - Target host (string or nil for local)
- `dest_path` - Destination path on target

### Storage

- **Format:** JSON at `~/.emacs.d/pchist/commands.json`
- **Locking:** Directory-based atomic locking for multi-process safety
- **In-memory:** Alist structures with symbol keys
- **Persistence:** Custom JSON serialization (no persist package dependency)

### CRUD Operations

#### Core Functions

- `pchist2-load` - Load from JSON with file locking
- `pchist2-save` - Save to JSON with file locking
- `pchist2-add-command` - Add new command (with silent auto-deduplication)
- `pchist2-get-commands &optional project` - Get commands (all or by project)
- `pchist2-update-command old-cmd updated-fields` - Update existing command
- `pchist2-delete-command cmd` - Delete command
- `pchist2-clear-all` - Clear all commands (interactive, with confirmation)

#### Query Helpers

For building suggestion UIs:

- `pchist2-get-unique-commands project` - All command names for project
- `pchist2-get-unique-switches project command` - All switches used with command
- `pchist2-get-unique-targets project command` - All targets used with command
- `pchist2-get-unique-projects` - All project paths
- `pchist2-get-installer-commands project command` - Installer commands used
- `pchist2-get-installer-hosts project` - All installer hosts in project

All query helpers return results ordered by most recent use first.

#### Debug Functions

- `pchist2-dump-commands` - Display all commands in readable format

### Automatic Deduplication

When adding a command, if an identical command already exists (comparing project, command, switches, targets, and installers - ignoring `last_used`), the existing command's `last_used` timestamp is silently updated instead of creating a duplicate.

### File Locking

Uses directory-based atomic locking (`make-directory` with exclusive flag) for safe concurrent access. Lock timeout is configurable via `pchist2-lock-timeout` (default 5 seconds). Stale locks (older than 2x timeout) are automatically cleaned up.

## Testing

### Running Tests

```elisp
;; In Emacs
M-x ert RET t RET

;; Run specific test
M-x ert RET pchist2-test-add-command RET

;; From command line
emacs -batch -l ert -l pchist2-data.el -l pchist2-data-test.el -f ert-run-tests-batch-and-exit
```

### Test Coverage (29 tests, all passing)

- **Serialization:** Alist/JSON conversion
- **Empty storage:** Initialization, saving empty data
- **Adding commands:** Single, multiple, with installers
- **Deduplication:** Exact matches, different switches/targets/projects
- **Queries:** Get all, by project, unique values
- **Updates:** Modify fields, update non-existent (error)
- **Deletion:** Remove commands
- **File I/O:** Save/load roundtrip, multiple projects
- **Locking:** Acquire/release, concurrent access prevention
- **Installer queries:** Commands, hosts
- **Edge cases:** Spaces in switches, empty lists, nil hosts
- **Clear all:** Complete history reset

## Example Usage

```elisp
;; Add a command
(pchist2-add-command
 "/imsar/hwtools/"
 "./build.sh"
 '("-a x86_64" "-c release")
 '("python-bindings")
 '(((command . "scp")
    (switches . ("-r"))
    (artifacts . ("build/foo.so"))
    (host . "my_stack")
    (dest_path . "/root/hwtools/"))))

;; Get all commands for a project
(pchist2-get-commands "/imsar/hwtools/")

;; Get unique switches used with a command
(pchist2-get-unique-switches "/imsar/hwtools/" "./build.sh")

;; Update a command
(let ((cmd (car (pchist2-get-commands "/imsar/hwtools/"))))
  (pchist2-update-command cmd '((switches . ("-a arm64")))))

;; Delete a command
(let ((cmd (car (pchist2-get-commands "/imsar/hwtools/"))))
  (pchist2-delete-command cmd))
```

## Design Notes

### Why No IDs?

Commands don't have explicit ID fields because:
1. No indices or external references need stable IDs
2. Commands are identified by their content for deduplication
3. Direct object references (via `eq`) work for updates/deletes
4. Simpler data structure

### Why Custom JSON Instead of persist?

1. Full control over serialization format
2. Enables file locking for multi-process safety
3. Human-readable JSON for debugging
4. No dependency on persist package
5. Better error handling and recovery

### Why Directory-Based Locking?

`make-directory` with exclusive flag is atomic across all platforms and Emacs versions, unlike:
- `write-region` with 'excl (not available in Emacs 27.1)
- `make-symbolic-link` (can have platform-specific issues)

## Next Steps

This data layer is complete and tested. Next steps for pchist v2:

1. **UI Layer** - Helm/completing-read interfaces for:
   - Selecting commands from history
   - Building new commands with suggestions
   - Managing installers
   
2. **Integration** - Connect to compile/recompile commands

3. **Migration** - Tool to import from pchist v1 (persist-based storage)

## Configuration

```elisp
(require 'pchist2-data)

;; Optional: customize storage location
(setq pchist2-storage-file "~/my-custom-location/commands.json")

;; Optional: adjust lock timeout
(setq pchist2-lock-timeout 10.0)  ; 10 seconds
```
