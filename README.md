# My Emacs config

This repository contains my Emacs configuration files. Tested on Emacs 31. The config's own code needs at least 29.1 (tree-sitter modes, `setopt`, `keymap-set`, `pos-eol`).

# Installation

From the repository root, link the two entry points into `~/.emacs.d` and make sure no `~/.emacs` exists, since it would take precedence:

```sh
ln -s "$PWD/init.el" ~/.emacs.d/init.el
ln -s "$PWD/early-init.el" ~/.emacs.d/early-init.el
```

To run another checkout without relinking, start `emacs --init-directory <checkout>`. `early-init.el` keeps `user-emacs-directory` at `~/.emacs.d`, so straight's builds and session state are shared. Set `EMACS_USER_DIRECTORY` to use another directory instead, for example a throwaway one that straight bootstraps from scratch.

# Loading order

`init.el` loads `.emacs`, which loads, in order:

1. `.emacs_lisp/config/init-pre.el`, if it exists, then calls `config-init-pre` if that file defines it.
1. The custom file `.emacs_lisp/custom.el`, if it exists. Because the custom file loads before the other modules, `:custom` values in `use-package` blocks override values saved through Customize.
1. Each module in `dn-modules`, in order. `init-programming.el` then loads every language module in `programming/` except those listed in `dn-disabled-languages`.
1. `.emacs_lisp/config/init-post.el`, if it exists, then `config-init-post` if defined.

Modules are loaded with `config-require`, which looks in `.emacs_lisp/config/` before `.emacs_lisp/`. A file there with the same name as a tracked module replaces it.

# Per-host customisation

`init-pre.el`, `init-post.el` and `custom.el` are not under version control. `init-pre.el` runs before any module loads, so it can change what gets loaded:

```elisp
(setq dn-modules (remq 'init-llm dn-modules))        ; skip a module
(setq dn-disabled-languages '(matlab gnuplot rest))  ; skip language modules
```

`dn-disabled-languages` takes file base names from `programming/`, as symbols.

# Layout

| Path | Contents |
|---|---|
| `init.el`, `early-init.el` | Entry points, linked from `~/.emacs.d` |
| `.emacs` | Loads the configuration |
| `.emacs_lisp/init-*.el` | One module per concern, listed in `dn-modules` |
| `.emacs_lisp/programming/` | Language modules; `foo.el` must provide `init-prog-foo` |
| `.emacs_lisp/config/` | Paths (`variables.el`), loader helpers (`functions.el`) and the untracked per-host files |
| `.emacs_lisp/vendor/` | Third-party libraries |
| `.emacs_lisp/lib/` | Own libraries |
| `.emacs_lisp/packages/` | Own packages (ROS 2 modes) |
| `.emacs_lisp/yas-lib/` | Helpers for snippets; `foo.el` must provide `yas-lib-foo` |
| `.emacs_lisp/snippets/` | yasnippet snippets |
| `.emacs_lisp/abbrev_*` | Abbrev tables |
| `.emacs_lisp/docsets/` | Dash docsets installed by `dn-dash-docs-install` |

# Timesheet

`init-org.el` loads `lib/dn-timesheet.el`, which keeps a daily timesheet in `dn-timesheet-file` (default `~/.emacs.d/timesheet.org`) using Org's clock. The file has one heading per ISO week and one subheading per day, plus a `Report` section with clock tables for the current week and month. The commands live under `C-c w`:

| Key | Command | Effect |
|---|---|---|
| `C-c w i` | `dn-timesheet-check-in` | Clock in on today's heading, creating the file, week and day as needed |
| `C-c w o` | `dn-timesheet-check-out` | Clock out; also closes a clock left open by a previous session |
| `C-c w t` | `dn-timesheet-toggle` | Check in or out, whichever applies |
| `C-c w s` | `dn-timesheet-status` | Show the time worked today |
| `C-c w r` | `dn-timesheet-report` | Refresh the clock tables and show the report |
| `C-c w f` | `dn-timesheet-open` | Visit the timesheet at today's heading |

Check-in and check-out take a prefix argument (`C-u C-c w i`) to enter the time by hand, for a forgotten check-in or check-out. A second check-in on the same day is a no-op.

# Checking the config

`make check` loads the whole config in batch mode and runs `test/smoke.el`. It exits non-zero on a load error, a `use-package` warning or a failed check.

`make compile` loads the config the same way, then byte-compiles the configuration's own files into a temporary directory. It exits non-zero if any file fails to compile or emits a warning.

Both load `init.el`, so the first run on a machine clones every package and takes a while.
